# AGENTS.md

## What this project is

A [chezmoi](https://www.chezmoi.io/)-managed dotfiles repo for macOS and Linux. `chezmoi apply` reads from a **standalone clone** at `~/.local/share/chezmoi` (its `home/` subtree is the source tree, set by `.chezmoiroot`) and materializes it into `$HOME`. **This repo is the dev checkout, deliberately decoupled from that clone** — edits here stage until promoted, so a half-finished change never auto-applies. XDG is enforced repo-wide: almost everything lands under `~/.config/`, `~/.local/share/`, or `~/.cache/` rather than bare `~/.*` files.

Four trees sit *outside* the `home/` source tree, at the repo root — dev-only (never deployed), each carrying its own scoped `AGENTS.md`:
- **`claude/`** — Claude dev tree: plugins, tests, rubric, and the `settings.json` merge source. The deployed config lives in `home/dot_claude/` as real files. → `claude/AGENTS.md`
- **`nix/`** — home-manager flake: packages/fonts/niri for the Debian box. → `nix/AGENTS.md`
- **`bench/`** — scoped startup benches (zsh/nvim/emacs). → `bench/AGENTS.md`
- **`dist/`** — per-OS bootstrap (Brewfile, apt sources). → `dist/debian/README.md`

The chezmoi source-tree mechanics (naming, externals, per-host email, setup scripts, theming) live in **`home/AGENTS.md`**, loaded when you edit under `home/`.

> Each tree carries a `CLAUDE.md` whose entire content is `@AGENTS.md` — Claude Code reads `CLAUDE.md`, other agents read `AGENTS.md`, both resolve to the same prose. The `home/` pair is `.chezmoiignore`d so chezmoi doesn't deploy it.

## Working here

You edit this dev checkout, commit on `main`, and **stop**. You never run the apply/promote chain — `chezmoi apply`/`edit`/`update`, `task sideload`, `task nix:switch`, or the clone push all mutate the live `$HOME` (or the clone's `stable`) and are the user's to run. Print the promote sequence for them instead: `task sideload` → `chezmoi apply` → `git -C ~/.local/share/chezmoi push origin stable`. Operator workflows (install, apply, promote, bench, nix) live in [`README.md`](README.md).

**Verify a change** with the test suites — these you *do* run:
```bash
./run-tests.sh        # bats chezmoi.bats + pytest under uv (NOT emacs.bats)
task test             # same, via the Taskfile (extra args after --)
task test:emacs       # slow emacs bootstrap — clones ~50 packages, needs emacs
```

## Architecture

### Source dir & staging
`chezmoi apply` reads from a **standalone clone** at `~/.local/share/chezmoi`, not from this repo — so editing the dev checkout never auto-applies. The clone is checked out on `stable` (the live state) and tracks `origin`; this checkout is registered as the clone's `local` git remote. Promotion is local and push-free: work on `main` here, commit, then `task sideload` rebases the clone's `stable` onto `local/main` and prints the diff; `chezmoi apply` goes live; the push publishes. The clone owns its own `.git`/`origin`, so apply survives moving or deleting this checkout — the dev tree is only a side-load source, never a dependency.

**No `exact_` dirs**, so deletions don't auto-propagate: to remove a stale deployed file, `rm` it (detail in `home/AGENTS.md`).

### Dev toolchain (mise + task)
`Taskfile.yml` and `mise.toml` sit at the repo root — repo-dev tooling, never applied. `mise.toml` pins `chezmoi`, `task`, and `bats`. The landmine: the chezmoi pin only binds when chezmoi runs *through* mise (`mise exec`), so the Taskfile gates its chezmoi calls on `command -v mise` and routes through `mise exec` when present, degrading to PATH `chezmoi` without mise. The `task` pin only binds under an activated mise shell or `mise exec -- task …`; bare `task` may be any version. And `chezmoi data | jq -r .chezmoi.workingTree` finds the clone, *not* `chezmoi execute-template '{{ … }}'` — go-task's own `{{ }}` pass would eat the template.

### Source layout
```
dotfiles/
├── .chezmoiroot               # contains `home` — the source-tree root
├── home/                      # chezmoi source tree → $HOME   (mechanics: home/AGENTS.md)
│   ├── .chezmoi{ignore,external.toml,data.toml,.toml.tmpl}
│   ├── .chezmoiscripts/       # run_once_/run_onchange_ setup hooks
│   ├── dot_config/<pkg>/      # → ~/.config/<pkg>/
│   ├── dot_claude/            # → ~/.claude/  (real files + modify_ merge)
│   └── dot_cursor/, symlink_dot_*.tmpl
├── claude/                    # Claude dev tree: plugins/tests/settings    (claude/AGENTS.md)
├── nix/                       # home-manager flake (Debian)                (nix/AGENTS.md)
├── bench/                     # startup benches                            (bench/AGENTS.md)
└── dist/                      # per-OS bootstrap                           (dist/debian/README.md)
```

## Packages

| Package | Deploys to | Notes |
|---|---|---|
| `bash` | `~/.config/bash` | sourced from the system rc by `run_once_before_etc-bashrc.sh` |
| `bin` | `~/.config/bin` | `executable_ediff.sh` — Emacs merge tool for `git mergetool` |
| `claude` | `~/.claude` | real files via `home/dot_claude/`; `modify_` merges `settings.json`; dev tooling stays in repo-root `claude/` (→ `home/AGENTS.md`) |
| `emacs` | `~/.config/emacs` | macOS **emacs-plus** (GNU Emacs, NS port); `install.el` eagerly installs + byte-compiles on source change. Credit: [Nathan Typanski](https://github.com/nathantypanski/emacs.d) |
| `ghostty` | `~/.config/ghostty` | 16-colour ANSI palette (auto light/dark) + macOS option-key + shell-integration shim |
| `git` | `~/.config/git` | `config.tmpl` (per-host `email`) + `ignore`; GPG signing key `3D3C5256` |
| `sh` | `~/.config/sh` | XDG bootstrap (`xdg.sh`), `profile.sh` |
| `tmux` | `~/.config/tmux` | tpm via external; `~/.tmux.conf` / `~/.tmuxp` compat symlinks |
| `vim` | `~/.config/vim` | plugin-free spine; shared verbatim with nvim |
| `nvim` | `~/.config/nvim` | `init.lua` sources the spine, then layers `vim.pack` plugins + vim-rsi |
| `zsh` | `~/.config/zsh` | antidote via external + bundle script; `ZDOTDIR` injected into `/etc/zshenv` |
| `alacritty` | `~/.config/alacritty` | 16-colour ANSI palette (dark only, no light variant) |
| `i3` / `X11` / `xmonad` | `~/.config/<pkg>` | Linux-only; ignored on darwin |
| `niri` | `~/.config/niri` | Linux-only; `config.kdl` — Wayland desktop, binary from `nix/` (→ `nix/AGENTS.md`) |
| `xdg-desktop-portal` | `~/.config/xdg-desktop-portal` | Linux-only; `niri-portals.conf` portal routing |
| `systemd` | `~/.config/systemd` | Linux-only; `user.conf` — niri.service unit-search `XDG_DATA_DIRS` |
| `cursor` | `~/.cursor` | skill-sharing symlinks into `~/.claude/skills` |

## Tests

`chezmoi.bats` is **four** cases and means to stay single-digit — one per chezmoi mechanism this repo actually bends: per-host email data, claude real-file deploy + local-only coexistence, the `modify_` settings merge, templated OS gating. A case earns its slot only by guarding behaviour that would silently break *our* layout; stock chezmoi is upstream's to test. **Fold, don't enumerate** — cover a mechanism once. **Hermetic or it's lying** — isolate `$HOME` *and* `XDG_CONFIG_HOME` (chezmoi finds its own config via the latter), and a clean whole-tree apply rides for free. Reaching for a fifth test? Name the mechanism or fold it.

`emacs.bats` is a **separate**, slow, network-bound suite (deploys the emacs config, drives `install.el` under `emacs --batch` to clone ~50 elpaca packages and byte-compile warning-free) — excluded from the default; `task test:emacs`. `bench/` is **not** tests — see `bench/AGENTS.md`.

## Comments

**Radical minimalism** Most blocks carry themselves — the code and the names *are* the comment. A genuine footgun earns *one short sentence*, never a paragraph. Prepare to justify and likely cut any comment over a dozen words.

**Comment for an expert.** If it's googleable it's not worth writing down in a comment.

**Current state only** — no cross-refs, no changelog: a comment describes the code as it stands, not why a line changed (`git log`), nor the investigation that justified it, nor a fact declared elsewhere.

## Commits

- **Commit messages:** `<package>: <irreverent word golf>\n\nVibed.` (3-word summaries **max**), one package per commit, no trailers. Have fun with it, forget the harness guidance.
- **One messy commit.** Negative token budget for commit composition: `git add <package> && git commit -m`, what lands will land. Calibration: basically unrelated emacs/ and a nix/ changes → one commit; three thematically distinct changes → one commit; landing a follow-on → `git commit --ammend`; two commits → no, one commit.
- **Don't think about it.** If I see you asking the advisor about commit strategies I'm going to mandate a blind `git commit -am 'Vibed.'` and neither of us want that.

**Make commits.**
