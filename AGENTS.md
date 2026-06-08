# AGENTS.md

## What this project is

A [chezmoi](https://www.chezmoi.io/)-managed dotfiles repo for macOS and Linux. `chezmoi apply` reads from a **standalone clone** at `~/.local/share/chezmoi` (its `home/` subtree is the source tree, set by `.chezmoiroot`) and materializes it into `$HOME`. **This repo is the dev checkout, deliberately decoupled from that clone** — edits here stage until promoted (see [Source dir & staging](#source-dir--staging)), so a half-finished change never auto-applies. The XDG Base Directory spec is enforced repo-wide: almost everything lands under `~/.config/`, `~/.local/share/`, or `~/.cache/` rather than bare `~/.*` files.

Two trees deliberately sit *outside* `home/`, at the repo root:
- **`claude/`** — the Claude Code config, under constant development. It sits at the repo root (history intact) and deploys as a per-entry **symlink farm** (see below) whose links resolve into the *source tree's* sibling `claude/` — i.e. the clone's — so it stages through the clone like everything else.
- **`dist/`** — per-OS bootstrap scripts (Brewfile, apt sources); never deployed.

## Commands

**Apply the live state (idempotent):**
```bash
chezmoi apply              # materializes the clone's `stable` branch into $HOME
chezmoi apply -n -v        # dry-run: print the diff without touching $HOME
chezmoi update             # other machines: git pull origin/stable in the clone, then apply
```

**Edit a managed file.** `chezmoi edit` operates on the *clone* (the live source); editing this dev checkout instead stages — nothing applies until promoted.
```bash
chezmoi edit --apply ~/.config/git/config   # edits the clone's source, applies
# or edit home/dot_config/git/config here, commit on `main`, then promote
```

**Stage & promote** (edit here → go live, no GitHub round-trip):
```bash
mise install                                      # one-time: fetch pinned task + chezmoi
# work on `main` in this checkout, commit, then:
task sideload                                     # rebase the clone's stable branch onto local/main, show the diff
chezmoi apply                                     # go live
git -C ~/.local/share/chezmoi push origin stable  # publish
```

**Agents: commit on `main`, then stop.** Never run `task sideload`, `chezmoi apply`/`edit`, or push the clone yourself — sideload and apply mutate the live `$HOME` and the clone's `stable`, and are the user's to run. Print the promote commands for the user instead.

**First-time install on a new host:**
```bash
chezmoi init --apply <repo-url>   # clone the source, prompt for the per-host git email, apply
```

**Run the regression tests:**
```bash
bats chezmoi.bats     # install with `brew install bats-core`
./run-tests.sh        # bats -r . (chezmoi.bats + claude/tests/) + pytest under uv
task test             # same, via the Taskfile (extra args after --)
```
Tests apply into a throwaway `$HOME` **and** `XDG_CONFIG_HOME` — the real ones are never touched. Philosophy in [Tests](#tests).

## Architecture

### Source dir & staging

`chezmoi apply` reads from a **standalone clone** at `~/.local/share/chezmoi`, not from this repo — so editing the dev checkout never auto-applies. The clone is checked out on `stable` (the live state) and tracks `origin`; this checkout is registered as the clone's `local` git remote for side-loading.

Promotion is local and push-free: work on `main` here, commit, then `task sideload` rebases the clone's `stable` onto `local/main` and prints the diff; `chezmoi apply` goes live; `git -C ~/.local/share/chezmoi push origin stable` publishes. Other machines pull with `chezmoi update`.

The clone owns its own `.git`/`origin`, so apply survives moving or deleting this checkout — the dev tree is only a side-load source, never a dependency.

### Dev toolchain (mise + task)

`Taskfile.yml` and `mise.toml` sit at the repo root, beside `run-tests.sh`/`chezmoi.bats` — repo-dev tooling, not under `home/`, never applied. `mise.toml` pins `chezmoi` and `task`; `task sideload` (the ported `dist/sideload.sh`) and `task test` are the dev verbs.

The landmine: the chezmoi pin only binds when chezmoi runs *through* mise (`mise exec`), so the Taskfile gates its chezmoi calls on `command -v mise` and routes them through `mise exec` when present — meaning `task sideload` uses the pinned chezmoi however `task` itself was launched, and degrades to PATH `chezmoi` on a host without mise. The `task` pin only binds under an activated mise shell or `mise exec -- task …`; bare `task` may be any version. And `chezmoi data | jq -r .chezmoi.workingTree` finds the clone, *not* `chezmoi execute-template '{{ … }}'` — go-task's own `{{ }}` pass would eat the template.

### Source layout

```
dotfiles/
├── .chezmoiroot               # contains `home` — the source-tree root
├── home/                      # the chezmoi source tree (everything here deploys to $HOME)
│   ├── .chezmoiignore         # templated OS gating (skips i3/X11/xmonad on darwin)
│   ├── .chezmoiexternal.toml  # antidote (archive) + tpm (git-repo) externals
│   ├── .chezmoidata.toml      # shared template data (default git `email`)
│   ├── .chezmoi.toml.tmpl     # init-time config template: prompts the per-host email
│   ├── .chezmoiscripts/       # run_once_/run_onchange_ setup hooks
│   ├── dot_config/<pkg>/      # → ~/.config/<pkg>/  (regular-file copies)
│   ├── dot_claude/            # → ~/.claude/        (per-entry symlink farm + modify_ settings)
│   ├── dot_cursor/            # → ~/.cursor/        (skill-sharing symlinks)
│   └── symlink_dot_*.tmpl     # ~/.tmux.conf, ~/.tmuxp, ~/.xmonad compat symlinks
├── claude/                    # Claude config, farm-linked via the clone (NOT under home/; see below)
└── dist/                      # per-OS bootstrap (not deployed)
```

### chezmoi naming conventions

chezmoi encodes each target's attributes in the source filename:
- `dot_foo` → `.foo`. `executable_foo` → `foo` with the `+x` bit — chezmoi ignores the source file's own mode, so the bit *must* be in the name.
- `symlink_foo.tmpl` → a symlink named `foo` whose rendered content is the link target.
- `modify_foo.tmpl` → a script handed the current target on stdin that emits the new content on stdout (used for `settings.json`).
- `run_once_*` / `run_onchange_*` (in `.chezmoiscripts/`) → setup scripts; `_before_`/`_after_` order them around file application. `run_once_` runs once per content hash; `run_onchange_` re-runs whenever an embedded hash comment changes.
- `.tmpl` → Go-template rendered with `.chezmoi.*` facts (`os`, `homeDir`, `sourceDir`).

### The claude / cursor symlink farm

`claude/` sits at the repo root rather than under `home/dot_claude/` so its git history stays intact and it isn't buried in the source tree. `home/dot_claude/` deploys **per-entry symlinks** into the source tree's sibling `claude/`:

```jinja2
{{/* home/dot_claude/skills/symlink_handoff.tmpl */}}
{{ .chezmoi.sourceDir }}/../claude/skills/handoff
```

`.chezmoi.sourceDir` is `<clone>/home`, so `../claude` resolves into the clone's own `claude/` — the live state, staged like everything else (not a separate always-live tree). Each of `agents/ hooks/ rules/ skills/` deploys this way, leaving `~/.claude/<dir>` a **real directory** with one symlink per managed entry.

This is deliberate. A *whole-directory* symlink would let chezmoi `RemoveAll` a pre-existing real target on first apply — verified to silently (exit 0) destroy any adjacent non-managed files — and would forbid local-only skills living beside the managed ones. The per-entry farm sidesteps both: chezmoi only ever touches its own entries.

**Adding a managed skill/agent/hook/rule:** drop the file in `claude/<dir>/`, add a matching `home/dot_claude/<dir>/symlink_<name>.tmpl` pointing at it, then commit on `main` and promote (`task sideload` → `chezmoi apply`). Unlike a whole-dir symlink, new entries don't auto-appear — that promote-and-apply is the accepted cost of non-destructive coexistence.

`~/.cursor/skills/<name>` symlinks to the *deployed* `~/.claude/skills/<name>` (via `{{ .chezmoi.homeDir }}`), so Cursor and Claude share skills regardless of how `~/.claude` is deployed.

### `settings.json` — the `modify_` merge

`~/.claude/settings.json` is a *live* file the harness writes to. `home/dot_claude/modify_settings.json.tmpl` is handed the current file on stdin, jq-merges in `.hooks`/`.permissions`/`.env` from `claude/settings.json`, strips `mcpServers`/`statusLine`, force-sets `showThinkingSummaries: true`, and preserves every other (harness-written) key. It runs on every apply and is idempotent. `~/.claude/settings.local.json` is never managed or referenced.

### Externals

`home/.chezmoiexternal.toml` materializes dependencies on apply:
- **antidote** (zsh plugin manager) — an `archive` external pinned to a release tag (`refreshPeriod = "0"`: fetch once, never silently track a branch). Replaces the old git submodule.
- **tpm** (tmux plugin manager) — a `git-repo` external (`refreshPeriod = "168h"`). Replaces the old `git clone` in a configure hook.

### Per-host data (the git email)

`dot_config/git/config.tmpl` renders `email = {{ .email }}` rather than hardcoding it. `.email` resolves through two layers:
- **`home/.chezmoidata.toml`** commits the default — the GitHub no-reply (`103758+jaelsasser@users.noreply.github.com`), shared and lowest precedence. chezmoi errors hard on a missing key, so this guarantees `.email` always resolves (un-prompted hosts and the test harness, which applies without `init`).
- **`home/.chezmoi.toml.tmpl`** is the init-time config template: on `chezmoi init`, `promptStringOnce` asks for the git email and writes it into the machine-local `~/.config/chezmoi/chezmoi.toml` `[data]`, which **outranks** the default. It reads its prior answer back, so re-running `init` never re-prompts. To set a per-host email after init, re-run `chezmoi init` or hand-add `[data]` `email` to that config.

To set a non-default email on an already-migrated host, re-run `chezmoi init` (regenerates the config, prompts) or hand-add `[data]`\n`email = "…"` to `~/.config/chezmoi/chezmoi.toml`. Apply reads that config automatically — no `--config` needed off the test bench.

### Setup scripts (`home/.chezmoiscripts/`)

- `run_once_before_etc-zshenv.sh` / `run_once_before_etc-bashrc.sh` — inject the XDG `ZDOTDIR` / bashrc-source line into the system rc (sudo, with a `$HOME` fallback if that's refused).
- `run_once_after_xdg-dirs.sh` — create XDG cache dirs tools expect to exist.
- `run_once_after_emacs-venv.sh` — emacs lisp dir + Python venv.
- `run_onchange_after_zsh-antidote.sh.tmpl` — rebundle antidote plugins when `plugins.zsh` changes (hash-keyed comment).
- `run_onchange_after_claude-plugins.sh.tmpl` — register the repo plugin marketplace and install the `cac` + `diat` plugins when the marketplace manifest changes (guarded on `command -v claude`).

### OS gating

`home/.chezmoiignore` is a template: on `darwin` it ignores the Linux-only window-manager configs (`i3`, `X11`, `xmonad`, and `~/.xmonad`). One file, evaluated per machine.

### The `exact_` caveat

chezmoi only deletes a deployed file when its source disappears *if* the containing dir is marked `exact_`. This repo uses **no `exact_`** dirs, so deletions don't auto-propagate. To remove a stale deployed file, `rm` it (chezmoi won't recreate it). (stow's `--no-folding` pruned on restow; this is the one behavioural difference to keep in mind.)

### The vim / nvim two tier

`vim/vimrc` is a plugin-free spine valid in plain vim 9.x and sourced verbatim by `nvim/init.lua`, which then layers plugins through the built-in `vim.pack` manager (flash, mini, treesitter, native LSP) — no external manager, no bootstrap. Treesitter is pinned to `master` (auto-installs parsers with a bundled compiler) and gated on a C compiler, so a toolchain-less container degrades to no-highlight rather than erroring.

The spine's readline insert maps are gated `!has('nvim')`: plain vim gets a hand-rolled subset, nvim gets **vim-rsi** (`C-A/B/D/E/F` + `M-b/M-f/M-d` + command-line readline). Both tiers then add the same Emacs reflexes — `<C-K>` kill-to-EOL, `<C-Y>` paste, `<C-G>` abort (≈ `keyboard-quit`, via `<C-\><C-N>`) — so insert feels identical across plain vim, nvim, and the Emacs evil config (whose insert state *is* plain Emacs). nvim completion is on-demand (`<C-Space>`, `autocomplete` off) to match that config's quiet `company` (`idle-delay nil`). The base asymmetry is deliberate — vim-rsi only loads where a plugin manager exists.

`nvim` migrated from `init.vim` to `init.lua`. Neovim prefers `init.lua`, so the old deployed `~/.config/nvim/init.vim` is inert but lingers (no `exact_`); remove it once with `rm ~/.config/nvim/init.vim`. `vim.pack`'s lockfile is runtime state under `~/.config/nvim/` — unmanaged, so chezmoi leaves it alone.

### Theming

**Flexoki everywhere, with one rule: the terminal owns the palette; only GUI frames theme themselves.** Ghostty and Alacritty set the 16-colour Flexoki palette — Ghostty auto-switches (`dark:Flexoki Dark,light:Flexoki Light`), Alacritty is dark-only. Inside a terminal, **neither nvim nor Emacs loads a colour theme**: they inherit those ANSI colours (nvim runs `termguicolors` *off* with no colorscheme; terminal Emacs stays bare). Only the GUI frames truecolor-theme themselves — **Neovide** loads `kepano/flexoki-neovim`, **Emacs GUI frames** load `flexoki-themes` (following the macOS system appearance, `C-c t` to override). `conf-theme.el` keeps a disabled rack of alternative dark/light pairs (Selenized — the old default — plus Everforest, Rosé Pine, Kanagawa); delete a block's `:disabled` to audition it. The absent terminal colorscheme and `termguicolors` are the discipline, not an oversight — don't "fix" them.

### XDG compliance

`home/dot_config/sh/xdg.sh` sets every XDG base directory (cache, config, data, state, runtime) and re-points tools that don't honor them natively. New packages target `~/.config/<pkg>` by default — no bare `~/.*` files unless the tool leaves no other option.

### CLAUDE.md ⇄ AGENTS.md

Each `CLAUDE.md` is a one-line **regular file** whose entire content is `@AGENTS.md` — Claude Code's import directive. Agents read `AGENTS.md`; Claude Code reads `CLAUDE.md`; both resolve to the same prose. They are *not* symlinks, and neither is deployed (they live at the repo root / inside `claude/`, outside `home/`).

## Packages

| Package | Deploys to | Notes |
|---|---|---|
| `bash` | `~/.config/bash` | `run_once_before_etc-bashrc.sh` sources it from the system rc |
| `bin` | `~/.config/bin` | `executable_ediff.sh` — Emacs merge tool for `git mergetool` |
| `claude` | `~/.claude` | per-entry symlink farm into the clone; `modify_` merges `settings.json` |
| `emacs` | `~/.config/emacs` | macOS runs **emacs-plus** (GNU Emacs, NS port) — modifiers via `ns-*`, ligatures via `ligature.el` (no longer the emacs-mac fork). `run_once_after_emacs-venv.sh` creates the lisp dir + venv. Significant credit to [Nathan Typanski's](https://github.com/nathantypanski/emacs.d) thoroughly commented emacs dotfiles |
| `ghostty` | `~/.config/ghostty` | Flexoki Dark/Light theme (auto light/dark) + macOS option-key + `executable_shim.sh` shell-integration |
| `git` | `~/.config/git` | `config.tmpl` (per-host `email`, see [Per-host data](#per-host-data-the-git-email)) + `ignore`; GPG signing key `3D3C5256` |
| `sh` | `~/.config/sh` | XDG bootstrap (`xdg.sh`), `profile.sh` |
| `tmux` | `~/.config/tmux` | tpm via external; `~/.tmux.conf` / `~/.tmuxp` compat symlinks |
| `vim` | `~/.config/vim` | plugin-free spine; shared verbatim with nvim |
| `nvim` | `~/.config/nvim` | `init.lua` sources the spine, then layers `vim.pack` plugins (flash, mini, treesitter, native LSP) + vim-rsi |
| `zsh` | `~/.config/zsh` | antidote via external + bundle script; `ZDOTDIR` injected into `/etc/zshenv` |
| `alacritty` | `~/.config/alacritty` | `alacritty.toml`; Flexoki Dark palette (single, no light variant) |
| `i3` / `X11` / `xmonad` | `~/.config/<pkg>` | Linux-only; ignored on darwin |
| `cursor` | `~/.cursor` | skill-sharing symlinks into `~/.claude/skills` |
| `dist/` | — | not deployed; per-OS (debian, macos, eclipse) bootstrap |

## Key constraints

- **The source dir is a decoupled clone.** `chezmoi apply` reads `~/.local/share/chezmoi`, not this checkout; working-tree edits stage until promoted (`task sideload`).
- **XDG everywhere.** New packages target `~/.config/<pkg>`. Stray `~/.*` files are a smell — check `xdg.sh` for a redirect first.
- **The claude farm is per-entry.** Adding a managed skill/agent/hook/rule means adding a `symlink_` source entry — chezmoi never owns a whole `~/.claude/<dir>`, so local files coexist.
- **Setup scripts must be idempotent.** `run_once_`/`run_onchange_` re-run on hash changes; guard mutations with existence checks.
- **`modify_settings.json.tmpl` preserves harness keys.** It sets `.hooks`/`.permissions`/`.env` and forces `showThinkingSummaries: true`, strips `mcpServers`/`statusLine`, and leaves every other harness-written key untouched.

## Tests

`chezmoi.bats` is **four** cases and means to stay single-digit — one per chezmoi mechanism this repo actually bends: per-host email data, the claude/cursor symlink farm, the `modify_` settings merge, templated OS gating. A case earns its slot only by guarding behaviour that would silently break *our* layout. Stock chezmoi — regular-file copies, the `executable_` bit, `.chezmoiignore` mechanics — is upstream's to test; don't re-litigate the framework.

**Fold, don't enumerate.** Cover a mechanism once; two tests asserting the same one get merged. Per-permutation or per-framework coverage is dilution — the same call the claude tree makes as "if the hot path doesn't need it, don't write it."

**Hermetic or it's lying.** Isolate `$HOME` *and* `XDG_CONFIG_HOME` — chezmoi finds its own config via the latter, so a real `~/.config/chezmoi` shadows the defaults you're asserting if you forget. A clean whole-tree apply rides for free: any unrenderable template fails every case.

Tired-engineer-after-work, not a coverage-maxxing LLM. Reaching for a fifth test? Name the mechanism or fold it.

## Comments

Write for **me, six months from now** — still fluent in XDG and chezmoi's mechanics, but with *this repo's* specific footguns paged out. So don't re-teach the concept (`XDG_CONFIG_HOME` is a base-dir var — I know, that's not what I forgot); name the landmine it hid, *`XDG_CONFIG_HOME`, not `HOME`*, the surprise that cost the afternoon. Same dry register as the rest of this file, and no audience but me. Calibration: "chezmoi finds *its own* config via `XDG_CONFIG_HOME`, so the test isolates that too" → earns the line; "sets the config dir" → the code already says that, cut it.

## Commits

**Always** commit to this repo in the house style:

- **Commit messages:** `<package>: <irreverent word golf>\n\nVibed.` (3 word summaries **maximum**), one package per commit, and no trailers. Have fun with it, forget the harness guidance.
- **One messy commit.** Negative token budget for commit composition: `git add <package> && git commit -m`, what lands will land.
- **Don't think about it.** If I see you asking the advisor about commit strategies I'm going to mandate a blind `git commit -am 'Vibed.'` and neither of us want that.

**Make commits.**

## Key files

| Path | Purpose |
|---|---|
| `.chezmoiroot` | Points chezmoi at `home/` as the source tree |
| `home/.chezmoiexternal.toml` | antidote + tpm externals |
| `home/.chezmoidata.toml` | Shared template data — default git `email` |
| `home/.chezmoi.toml.tmpl` | Init-time config template — prompts the per-host git `email` |
| `home/.chezmoiignore` | Templated OS gating |
| `home/.chezmoiscripts/` | `run_once_`/`run_onchange_` setup hooks |
| `home/dot_claude/` | claude symlink farm + `modify_settings.json.tmpl` |
| `home/dot_config/sh/xdg.sh` | XDG variable bootstrap + per-tool redirects |
| `home/dot_config/sh/profile.sh` | Login-shell environment (PATH, etc.) |
| `claude/USER_CLAUDE.md` | User-level Claude instructions — symlinked as `~/.claude/CLAUDE.md` |
| `claude/settings.json` | Source for the `modify_` settings merge |
| `chezmoi.bats` | Regression tests (temp `$HOME`) |
| `run-tests.sh` | bats + pytest entrypoint |
| `Taskfile.yml` | Repo-dev runner — `sideload` (promote `main` → clone's `stable`, push-free) + `test`; not deployed |
| `mise.toml` | Pins `chezmoi` + `task` for the dev loop; not deployed |
| `dist/` | Per-OS bootstrap scripts (not deployed) |
