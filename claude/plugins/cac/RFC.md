# cac — compact-and-continue

A Claude Code plugin that triggers `/compact`[^compact] mid-session — automatically, when the math says it'll pay off — without losing the thread of what you were working on.

## Why this exists

Long Claude Code sessions cost more than turn-by-turn observation suggests. The plugin's job is to time `/compact` calls so they save more tokens than they spend. Making that decision requires a mental model of how Anthropic prices API usage, which is annoying to reason about cold.

### What's happening under the hood

Claude is a transformer-based LLM, and the mechanism it uses to read context is called *attention*[^attention]. On every API call, the model attends over the entire prefix to produce its next output. Longer prefixes mean more attention work, both in dollars (more tokens to bill) and in degraded quality: information in the middle of a long context is measurably less likely to be used than information at the start or end[^lost-in-middle]. Long sessions cost more *and* perform worse. Compacting is the lever that addresses both, though the equation below tracks only the cost side.

Each iteration of the agentic loop[^react] — Claude emits a `tool_use` block, the harness runs the tool locally, the result comes back as a `tool_result` on the next API call[^tool-use] — is a billed API call. A typical coding session is dozens or hundreds of such calls. The conversation transcript — your messages, Claude's responses, every file read, every command output — grows monotonically across the session and is sent in full on every API call.

### How tokens are billed

To keep this affordable, Anthropic operates a *prompt cache*[^prompt-caching]: the server remembers prefixes of recent requests and serves them at a discount on repeat. Every API call's tokens land in one of four buckets, billed at different rates relative to the base input price:

| Bucket          | What it counts                                                                                                                                                                                                 | Rate                         |
|-----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------|
| **Input**       | New tokens since the previous call: latest user message, latest tool result                                                                                                                                    | 1×                           |
| **Cache read**  | Tokens recognized as a prefix of a previous call, served from the cache                                                                                                                                        | 0.1×                         |
| **Cache write** | Tokens being stored to the cache for future reuse                                                                                                                                                              | 1.25× (5-min TTL) or 2× (1h) |
| **Output**      | Tokens Claude generates: visible text, tool-use blocks, and any internal reasoning the model produces before its visible response[^cot] (available as "extended thinking" in current Claude models[^thinking]) | 5×                           |

The cache is what makes long sessions affordable at all. A 100k-token transcript read fifty times would be ruinous at 1×; at 0.1× it's merely annoying. But the cache doesn't make long sessions *cheap* — the volume still adds up, and stale content (files read twenty turns ago, irrelevant to the current task) costs the same per token as live content.

### What this costs in practice

Concrete picture. You open Claude Code in a project that's been idle for a couple of hours, type a question, and Claude replies with 2000 tokens of output. That's one API call. The prefix — system prompt, tool definitions, your `CLAUDE.md`, plus whatever Claude Code injects per session — has gone cold (the 5-minute and 1-hour cache windows have both closed), so the call pays cache_write on the whole prefix. Assume 100k of it.

|                         | Sonnet 4.6 | Opus 4.7 |
|-------------------------|------------|----------|
| Cold-cache opening call | $0.41      | $0.68    |

You read the reply, type a follow-up. Same shape of call, but the prefix (now slightly larger because turn 1 was appended) is warm — no cache_write, just cache_read.

|                           | Sonnet 4.6 | Opus 4.7 |
|---------------------------|------------|----------|
| Warm-cache follow-up call | $0.06      | $0.10    |

Two human messages, two model replies, no tool calls — and the cold-resume penalty on the first call is ~7× the steady-state per-call cost. Total: ~$0.47 / ~$0.78.

Now picture either reply involving real work. Claude needs to grep a couple of files and run a build before it can answer. Every output Claude generates — each `tool_use` block, and the final text reply — is a separate API call, each one re-reading the cached prefix. **A Claude bash call bills the same as a user message; both are just API calls with new input on top of the prefix, only the input is `tool_result` instead of human text.** A reply that takes five sequential tool calls is six API calls instead of one.

Per-call cost scales linearly with prefix size within each cache state, so the picture across a session's prefix range looks like this:

| Prefix size | Sonnet warm | Opus warm | Sonnet cold | Opus cold |
|-------------|-------------|-----------|-------------|-----------|
| 50k         | $0.045      | $0.075    | $0.218      | $0.363    |
| 100k        | $0.060      | $0.100    | $0.405      | $0.675    |
| 150k        | $0.075      | $0.125    | $0.593      | $0.988    |
| 200k        | $0.090      | $0.150    | $0.780      | $1.300    |

A 200k warm prefix isn't catastrophic on any single call — $0.09 to $0.15 reads as noise — but the absolute number compounds across a session's worth of API calls, and the lost-in-the-middle quality degradation rides along with the dollars. Compaction's value is twofold: it dodges the one-time cache_write penalty when the cache eventually expires, *and* it lowers the ongoing read cost on every API call that follows.

### The break-even equation

Compacting isn't free. The summary has to be generated (output, 5×), the new prefix has to be re-cached (write, 1.25× for the 5-minute tier or 2× for the 1-hour tier), and the summarization API call itself reads the full current transcript once at cache_read to compose the summary. The break-even point for whether to compact *now* is:

$$K^* = \frac{N + c \cdot M}{N - S - M}$$

The variables:

- **N** — current cached prefix size, in tokens.
- **M** — summary size the compact would produce.
- **S** — stable overhead carried across the compact (system prompt, tool definitions, project `CLAUDE.md`).
- **c** — cost of producing one unit of summary relative to reading one unit of cached prefix: `(write multiplier + output multiplier) / read multiplier`. For the default 5-minute tier: `(1.25 + 5) / 0.1 = 62.5`. For the 1-hour tier: `(2 + 5) / 0.1 = 70`.

K\* is the count of further API calls against the new prefix at which compacting starts to save money. Some intuition, with M=8k summary and S=20k overhead:

| Current prefix N | K\* (5-min tier) | K\* (1-hour tier) | Read as                      |
|------------------|------------------|-------------------|------------------------------|
| 60k              | ~17              | ~19               | rarely worth compacting      |
| 100k             | ~8               | ~9                | compact if real work remains |
| 140k             | ~5               | ~6                | compact unless wrapping up   |
| 170k             | ~4               | ~5                | compact almost always        |

As N grows, K\* shrinks, and the threshold for "should compact" gets easier to clear. The 1-hour tier needs slightly more amortization (one to two extra API calls across the range shown) because the higher write cost takes more reads to pay off, but the operational decision in each row is the same. The plugin watches `N` against this curve and intervenes when the math is favourable.

K\* depends on ratios between the four buckets, not on absolute prices, and those ratios are identical across Anthropic's current-generation models. The break-even API-call count is the same whether you're on Sonnet or Opus[^pricing]; only the absolute dollars change.

## Making compaction safe

Compaction only saves money if it doesn't damage downstream work. The summary the model produces is half the safety story — the other half is what's already loaded into the session prefix, where compaction can't touch it.

`CLAUDE.md` (and the increasingly common cross-tool `AGENTS.md`[^agents-md]) lives in the session prefix, not the conversation transcript, so its contents persist across every compaction. These files **bootstrap** the agent at the start of every session and re-ground it after every compact: stable project knowledge — build commands, conventions, key file pointers, architecture sketches — lives in version control rather than in the transcript. (Bootstrapping is distinct from retrieval-augmented generation[^rag], where docs are *fetched at query time* based on relevance to the current question. `CLAUDE.md` is always loaded regardless of what you're asking about; RAG is the technique to reach for when project context outgrows what fits in the prefix.)

The same trick works for *active task state*. A plan document on disk — the artifact Plan Mode produces, or any equivalent markdown spec you maintain[^plan-and-solve] — grounds the agent in what it's working on. A structured task list, whether Claude Code's `TodoWrite` (preserved by the harness across compaction) or a hand-maintained `TASKS.md` on disk, grounds it in how far along it is. `AGENTS.md` bootstraps the agent into the repo; the plan and task list bootstrap it into the current workflow. All three survive compaction, and let the transcript summary drop everything they cover.

Together these artifacts raise the floor under compaction. The summary can drop anything reconstructible from disk or prefix — file contents, prior tool outputs, project conventions, current task state — and carry only what *can't* be re-derived: in-flight design decisions, contracts not yet landed, open questions raised but not resolved. Without that floor, every compact is partial amnesia rather than a clean handoff, and the plugin's cost savings come at the price of session quality.

HumanLayer's "Writing a Good CLAUDE.md"[^humanlayer-claude-md] is the most-cited community resource on the `CLAUDE.md` / `AGENTS.md` piece — short, opinionated, grounded in measured behaviour around how many instructions models reliably follow. The plugin assumes you've done all of this work; it's a precondition, not something compaction can paper over.

## Footnotes

[^attention]: Attention is the mechanism by which transformer models relate each input token to every other input token, allowing the model to weigh which prior tokens matter for predicting the next one. Introduced in "Attention Is All You Need" (Vaswani et al., 2017): <https://arxiv.org/abs/1706.03762>.

[^lost-in-middle]: "Lost in the Middle: How Language Models Use Long Contexts" (Liu et al., 2023) demonstrated a U-shaped performance curve when LLMs retrieve information from long inputs: accuracy drops significantly when the relevant content sits in the middle of the context window rather than near the start or end. The effect is robust across model families and persists even in models marketed as long-context. <https://arxiv.org/abs/2307.03172>.

[^tool-use]: Anthropic's tool-use protocol: the model emits a `tool_use` block, the client executes the tool locally, and the result is submitted as a `tool_result` on the next API call. See <https://platform.claude.com/docs/en/build-with-claude/tool-use>.

[^react]: "ReAct: Synergizing Reasoning and Acting in Language Models" (Yao et al., 2022) introduced the canonical pattern for tool-using LLM agents: alternate between *reasoning* steps and *acting* steps, feeding observations back into the loop. This is the architectural skeleton underneath essentially every modern coding agent, including Claude Code's tool-use loop. <https://arxiv.org/abs/2210.03629>.

[^prompt-caching]: Anthropic's prompt caching documentation covers the `cache_control` mechanism, supported TTLs (5-minute default, 1-hour beta), the four-bucket pricing structure summarized above, and the four-breakpoint limit per request: <https://platform.claude.com/docs/en/build-with-claude/prompt-caching>.

[^thinking]: Recent Claude models have an extended-thinking mode in which the model generates internal reasoning tokens before producing visible output. These tokens are billed as output even when summarized or hidden from the response: <https://platform.claude.com/docs/en/build-with-claude/extended-thinking>.

[^cot]: "Chain-of-Thought Prompting Elicits Reasoning in Large Language Models" (Wei et al., 2022) was the foundational result showing that allowing a model to generate intermediate reasoning tokens before its final answer produces large, robust accuracy gains on multi-step problems. The "extended thinking" feature in modern Claude models is the productization of this insight. <https://arxiv.org/abs/2201.11903>.

[^compact]: `/compact` is a built-in Claude Code slash command that replaces the conversation transcript with a model-generated summary, freeing context-window space and resetting per-turn token volume. The official explanation of context, compaction, and session management: <https://code.claude.com/docs/en/how-claude-code-works>.

[^pricing]: Anthropic's current pricing as of May 2026: Sonnet 4.6 at $3 / $15 per million input/output tokens, Opus 4.7 at $5 / $25. Prompt caching adds two multipliers on input — 1.25× to write to the 5-minute tier or 2× to write to the 1-hour tier, and 0.1× to read from either tier. The 5x output-to-input ratio is consistent across the current generation. <https://www.anthropic.com/pricing>.

[^rag]: "Retrieval-Augmented Generation for Knowledge-Intensive NLP Tasks" (Lewis et al., 2020) established the pattern of augmenting an LLM's context with externally-retrieved documents at inference time, rather than relying solely on what the model memorized during training. RAG's defining property is *retrieval*: a query triggers a search that selects relevant docs to inject. `CLAUDE.md`-style bootstrapping is its degenerate, always-on cousin — the same docs are loaded regardless of query. <https://arxiv.org/abs/2005.11401>.

[^agents-md]: `AGENTS.md` is a cross-tool convention for project-level agent instructions, stewarded by the Linux Foundation's Agentic AI Foundation and read natively by Codex, Cursor, Copilot, the Gemini CLI, etc. Claude Code reads `CLAUDE.md` as its primary file; the common pragmatic move is to author `AGENTS.md` as the canonical source and symlink `CLAUDE.md` to it, keeping a single source of truth across tools. Spec and rationale: <https://agents.md/>.

[^humanlayer-claude-md]: HumanLayer's "Writing a Good CLAUDE.md" is the most-cited community guide on the authoring practice. Concrete advice on instruction-count limits (frontier thinking models follow roughly 150–200 instructions reliably; smaller and non-thinking models follow fewer), what to omit (anything a linter or formatter could enforce instead), and how to structure progressive disclosure so task-specific docs load only when relevant. <https://www.humanlayer.dev/blog/writing-a-good-claude-md>.

[^plan-and-solve]: "Plan-and-Solve Prompting: Improving Zero-Shot Chain-of-Thought Reasoning by Large Language Models" (Wang et al., ACL 2023) introduced the technique of instructing an LLM to first devise an explicit plan before executing the steps, demonstrating reduced *missing-step* errors compared to Zero-shot CoT's "let's think step by step." Claude Code's Plan Mode is essentially a UX-layer application of this idea — the plan document on disk is the durable artifact that lets a fresh post-compaction agent reconstitute task state without re-deriving it. <https://arxiv.org/abs/2305.04091>.

---

*Written by Claude Opus 4.7*
