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
./run-tests.sh        # bats chezmoi.bats + pytest under uv (NOT emacs.bats)
task test             # same, via the Taskfile (extra args after --)
task test:emacs       # the slow emacs bootstrap suite — clones ~50 packages, needs emacs
```
Tests apply into a throwaway `$HOME` **and** `XDG_CONFIG_HOME` — the real ones are never touched. Philosophy in [Tests](#tests).

**Run the startup benches** (live config, report-only — never fail):
```bash
task bench           # zsh + nvim + emacs scoped startup tables
task bench:zsh       # one tool; resample with e.g. BENCH_RUNS=20 task bench:nvim
```
These measure the **live deployed** `~/.config/*`, not a throwaway. Detail in [Benches](#benches).

**Apply the Nix package layer** (Debian box only — packages/fonts/niri, never config):
```bash
task nix:switch     # home-manager switch from the clone's nix/ flake
task nix:update     # bump flake inputs, then switch
```
Reads the **clone** (promote with `task sideload` first); first-install runbook in `dist/debian/README.md`.

## Architecture

### Source dir & staging

`chezmoi apply` reads from a **standalone clone** at `~/.local/share/chezmoi`, not from this repo — so editing the dev checkout never auto-applies. The clone is checked out on `stable` (the live state) and tracks `origin`; this checkout is registered as the clone's `local` git remote for side-loading.

Promotion is local and push-free: work on `main` here, commit, then `task sideload` rebases the clone's `stable` onto `local/main` and prints the diff; `chezmoi apply` goes live; `git -C ~/.local/share/chezmoi push origin stable` publishes. Other machines pull with `chezmoi update`.

The clone owns its own `.git`/`origin`, so apply survives moving or deleting this checkout — the dev tree is only a side-load source, never a dependency.

### Dev toolchain (mise + task)

`Taskfile.yml` and `mise.toml` sit at the repo root, beside `run-tests.sh`/`chezmoi.bats` — repo-dev tooling, not under `home/`, never applied. `mise.toml` pins `chezmoi`, `task`, and `bats`; `task sideload` (the ported `dist/sideload.sh`), `task test`, `task test:emacs`, and `task bench` are the dev verbs.

The landmine: the chezmoi pin only binds when chezmoi runs *through* mise (`mise exec`), so the Taskfile gates its chezmoi calls on `command -v mise` and routes them through `mise exec` when present — meaning `task sideload` uses the pinned chezmoi however `task` itself was launched, and degrades to PATH `chezmoi` on a host without mise. The `task` pin only binds under an activated mise shell or `mise exec -- task …`; bare `task` may be any version. And `chezmoi data | jq -r .chezmoi.workingTree` finds the clone, *not* `chezmoi execute-template '{{ … }}'` — go-task's own `{{ }}` pass would eat the template.

### Benches

`bench/` (repo root, dev-only, never deployed) holds three scoped startup-time benches over the **live deployed config** (`~/.config/{zsh,nvim,emacs}`) — warm, read-only, **report-only**: they print a table and never fail, because a startup time isn't pass/fail (a bench, not a test). `task bench` runs all three; `task bench:{zsh,nvim,emacs}` one each; `BENCH_RUNS` (default 10) sets the sample count. Each lens, and the footgun that shaped it:

- **`zsh.zsh`** — `EPOCHREALTIME` around `zsh -i -c exit` × N (total), an `.zshenv` vs `.zshrc` split, and a `zprof` self-time table. The zprof run is `zsh -f` + a manual `source` of the real rc files, because `/etc/zshenv` force-exports `ZDOTDIR` — the obvious temp-`ZDOTDIR` inject, and `-d`/`--no-globalrcs`, lose that fight. zprof also prints per-function callgraph blocks after its summary table (both start with `N)`), so the parser reads only the summary.
- **`nvim.sh`** — `nvim --startuptime` × N; the median run's log read twice, as nvim's own phase timeline and as per-plugin/`require` cost bucketed on the log's *self* column so nested requires don't double-count their parent. One warm-up drops the cold ShaDa/parser hit.
- **`emacs.sh` + `emacs.el`** — batch load of the real config, median total + per-package init+config from `use-package-statistics`. Two traps: `--batch` alone won't load the user init (so `-q -l early-init … -l init`, and `-q` is also the only point early enough to set `use-package-compute-statistics`), and native-comp is *synchronous* under `--batch` — `native-comp-jit-compilation nil` keeps the load to byte-code so an un-cached `.eln` doesn't dwarf the hot path. GUI frame cost is excluded; an eagerly-`require`d package (no `:defer`/`:commands`, e.g. `man`) shows its full load on the path, and `use-package-statistics-time` sums phase timers that can overlap, so a hot package may read above the wall total.

### Nix package layer (niri desktop over Debian)

`nix/` (repo root, dev-only, **never deployed** — same posture as `dist/`/`bench/`) is a standalone
[home-manager](https://nix-community.github.io/home-manager/) flake that drops a *modern* toolchain and
a [niri](https://github.com/YaLTeR/niri) Wayland desktop onto a non-NixOS **Debian Trixie** box, where
apt's freeze can't. Strict division of labour: **Nix owns binaries, fonts, and the niri compositor
binary; chezmoi still owns every config file** (niri's `config.kdl` included) — no home-manager
`programs.*` modules, so the two never dual-own a dotfile. `task nix:switch` reads the **clone**
(`<clone>/nix`, promoted state), so the loop is `edit nix/ here → task sideload → task nix:switch` — the
mirror-image of `chezmoi apply`, which reads the clone too.

Layout: `flake.nix` (inputs: `nixpkgs-unstable` — neovim 0.12/`vim.pack` is unstable-only — + home-manager
`master` for `targets.genericLinux.gpu`; output `homeConfigurations."josh@trixie"`), `home.nix` (identity +
`targets.genericLinux{,.gpu}.enable`), and three single-concern `modules/`: `packages.nix` (toolchain +
LSPs + CLI), `fonts.nix` (`fonts.fontconfig.enable` + `nerd-fonts.*`), `desktop.nix` (ghostty / neovide /
neovim / niri / emacs30-pgtk). `flake.lock` is **not** committed from here — there's no Nix on macOS; it's
generated on the first switch on the Trixie box.

**GPU — the gpu module, not nixGL.** `targets.genericLinux.gpu.enable` symlinks Nix's Mesa into
`/run/opengl-driver`, so every Nix GL app finds drivers with no per-binary wrapping. Cost: a switch that
changes the Mesa version prints a `sudo .../non-nixos-gpu-setup` line — run it, reboot. Mesa (Intel/AMD)
means **pure builds**: no `--impure`, no version pinning, no `.gpu.nvidia` block.

**The host ⇄ Nix boundary.** Nix ships the apps/fonts/niri binary; the kernel DRM driver + firmware,
systemd-logind seat, greetd+tuigreet, pipewire stack, xdg portals, and the polkit agent are all **apt**
(`dist/debian/` is the manifest + runbook). The seam has four load-bearing glue pieces, all chezmoi-owned
and Linux-only (the `.chezmoiignore` darwin block ignores `.config/{niri,xdg-desktop-portal,systemd}`):
- **`~/.config/systemd/user.conf`** — `[Manager] ManagerEnvironment=XDG_DATA_DIRS=%h/.nix-profile/share:…`.
  The systemd *user manager* computes its unit search path from its **own** `XDG_DATA_DIRS` at startup;
  `environment.d` does **not** reach it. Without this, niri's Nix-installed `niri.service` is "not found" →
  `graphical-session.target` never fires → portals cascade-fail.
- **Login-shell PATH** — the greeter runs `zsh -l -c niri-session`, a *non-interactive login* shell, which
  reads `dot_zshenv` but **not** `dot_zshrc`; so the `hm-session-vars.sh` source lives in `dot_zshenv`
  (guarded by `-f`, inert until a switch), else `niri-session` can't resolve `niri` by name.
- **Portals** — `niri-portals.conf` routes `default=gtk` with ScreenCast/Screenshot to `gnome`; gnome
  otherwise wins by D-Bus priority then fails silently. Never set `GDK_BACKEND=wayland` globally — it
  breaks the screencast portal.
- **Seat** — logind + uaccess, nothing to install (Trixie's systemd 257 predates the 258 uaccess
  regression); needs a *real* logind session (greeter/TTY, never SSH).

First-install + verification runbooks live in `dist/debian/README.md`. niri is `pkgs.niri` (26.04, version
parity with Arch/Fedora); its `homeModules.niri` is unused — the `niri.service` it ships is NixOS-only.

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
├── nix/                       # standalone home-manager flake (packages/fonts/niri for Debian); not deployed
├── bench/                     # scoped startup benches (zsh/nvim/emacs); dev-only, not deployed
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
- `run_onchange_after_emacs-bootstrap.sh.tmpl` — eagerly elpaca-install + byte-compile the emacs config whenever any `emacs/*.el` or `emacs/conf/*.el` changes (hash-keyed via `glob`+`include`). Runs `emacs -nw -l install.el` for live progress; TTY-guarded (`[ -t 0 ]`), so a headless apply skips it and lazy first-launch still installs.

### OS gating

`home/.chezmoiignore` is a template: on `darwin` it ignores the Linux-only window-manager configs (`i3`, `X11`, `xmonad`, and `~/.xmonad`). One file, evaluated per machine.

### The `exact_` caveat

chezmoi only deletes a deployed file when its source disappears *if* the containing dir is marked `exact_`. This repo uses **no `exact_`** dirs, so deletions don't auto-propagate. To remove a stale deployed file, `rm` it (chezmoi won't recreate it). (stow's `--no-folding` pruned on restow; this is the one behavioural difference to keep in mind.)

### The vim / nvim two tier

`vim/vimrc` is a plugin-free spine valid in plain vim 9.x and sourced verbatim by `nvim/init.lua`, which then layers plugins through the built-in `vim.pack` manager (flash, mini, treesitter, native LSP) — no external manager, no bootstrap. Treesitter is pinned to `master` (auto-installs parsers with a bundled compiler) and gated on a C compiler, so a toolchain-less container degrades to no-highlight rather than erroring.

The spine's readline insert maps are gated `!has('nvim')`: plain vim gets a hand-rolled subset, nvim gets **vim-rsi** (`C-A/B/D/E/F` + `M-b/M-f/M-d` + command-line readline). Both tiers then add the same Emacs reflexes — `<C-K>` kill-to-EOL, `<C-Y>` paste, `<C-G>` abort (≈ `keyboard-quit`, via `<C-\><C-N>`) — so insert feels identical across plain vim, nvim, and the Emacs evil config (whose insert state *is* plain Emacs). nvim completion is on-demand (`<C-Space>`, `autocomplete` off) to match that config's quiet `company` (`idle-delay nil`). The base asymmetry is deliberate — vim-rsi only loads where a plugin manager exists.

`nvim` migrated from `init.vim` to `init.lua`. Neovim prefers `init.lua`, so the old deployed `~/.config/nvim/init.vim` is inert but lingers (no `exact_`); remove it once with `rm ~/.config/nvim/init.vim`. `vim.pack`'s lockfile is runtime state under `~/.config/nvim/` — unmanaged, so chezmoi leaves it alone.

### Theming

**One rule: the terminal owns the palette; only GUI frames theme themselves.** Ghostty and Alacritty set the 16-colour ANSI palette — Ghostty auto-switches light/dark, Alacritty is dark-only. Inside a terminal, **neither nvim nor Emacs loads a colour theme**: they inherit those ANSI colours (nvim runs `termguicolors` *off* with no colorscheme; terminal Emacs stays bare). Only the GUI frames truecolor-theme themselves — **Neovide** loads a `vim.pack` colorscheme that tracks `&background`, **Emacs GUI frames** load a theme following the macOS system appearance. `my-theme.el` holds a switchable rack of light/dark pairs: the active pair is a `defcustom` you `setopt` to switch (lazily elpaca-installing the pick), `C-c t` flips light↔dark. The absent terminal colorscheme and `termguicolors` are the discipline, not an oversight — don't "fix" them.

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
| `emacs` | `~/.config/emacs` | macOS runs **emacs-plus** (GNU Emacs, NS port) — modifiers via `ns-*`, ligatures via `ligature.el` (no longer the emacs-mac fork). `run_once_after_emacs-venv.sh` creates the lisp dir + venv; `install.el` (chezmoi-driven, see [Setup scripts](#setup-scripts-homechezmoiscripts)) eagerly installs packages + byte-compiles on source change. Significant credit to [Nathan Typanski's](https://github.com/nathantypanski/emacs.d) thoroughly commented emacs dotfiles |
| `ghostty` | `~/.config/ghostty` | 16-colour ANSI palette (auto light/dark) + macOS option-key + `executable_shim.sh` shell-integration |
| `git` | `~/.config/git` | `config.tmpl` (per-host `email`, see [Per-host data](#per-host-data-the-git-email)) + `ignore`; GPG signing key `3D3C5256` |
| `sh` | `~/.config/sh` | XDG bootstrap (`xdg.sh`), `profile.sh` |
| `tmux` | `~/.config/tmux` | tpm via external; `~/.tmux.conf` / `~/.tmuxp` compat symlinks |
| `vim` | `~/.config/vim` | plugin-free spine; shared verbatim with nvim |
| `nvim` | `~/.config/nvim` | `init.lua` sources the spine, then layers `vim.pack` plugins (flash, mini, treesitter, native LSP) + vim-rsi |
| `zsh` | `~/.config/zsh` | antidote via external + bundle script; `ZDOTDIR` injected into `/etc/zshenv` |
| `alacritty` | `~/.config/alacritty` | `alacritty.toml`; 16-colour ANSI palette (dark only, no light variant) |
| `i3` / `X11` / `xmonad` | `~/.config/<pkg>` | Linux-only; ignored on darwin |
| `niri` | `~/.config/niri` | Linux-only; `config.kdl` starter — Wayland desktop, binary from the `nix/` flake |
| `xdg-desktop-portal` | `~/.config/xdg-desktop-portal` | Linux-only; `niri-portals.conf` portal routing |
| `systemd` | `~/.config/systemd` | Linux-only; `user.conf` — niri.service unit-search `XDG_DATA_DIRS` |
| `cursor` | `~/.cursor` | skill-sharing symlinks into `~/.claude/skills` |
| `dist/` | — | not deployed; per-OS bootstrap — `debian/` carries the niri host manifest + runbook |

## Key constraints

- **The source dir is a decoupled clone.** `chezmoi apply` reads `~/.local/share/chezmoi`, not this checkout; working-tree edits stage until promoted (`task sideload`).
- **XDG everywhere.** New packages target `~/.config/<pkg>`. Stray `~/.*` files are a smell — check `xdg.sh` for a redirect first.
- **The claude farm is per-entry.** Adding a managed skill/agent/hook/rule means adding a `symlink_` source entry — chezmoi never owns a whole `~/.claude/<dir>`, so local files coexist.
- **Setup scripts must be idempotent.** `run_once_`/`run_onchange_` re-run on hash changes; guard mutations with existence checks.
- **`modify_settings.json.tmpl` preserves harness keys.** It sets `.hooks`/`.permissions`/`.env` and forces `showThinkingSummaries: true`, strips `mcpServers`/`statusLine`, and leaves every other harness-written key untouched.
- **Nix is the package layer; chezmoi owns configs.** The `nix/` home-manager flake (Debian only) delivers binaries/fonts/the niri binary, never config — no `programs.*` modules. `task nix:switch` reads the **clone**, so promote (`task sideload`) before switching.

## Tests

`chezmoi.bats` is **four** cases and means to stay single-digit — one per chezmoi mechanism this repo actually bends: per-host email data, the claude/cursor symlink farm, the `modify_` settings merge, templated OS gating. A case earns its slot only by guarding behaviour that would silently break *our* layout. Stock chezmoi — regular-file copies, the `executable_` bit, `.chezmoiignore` mechanics — is upstream's to test; don't re-litigate the framework.

**Fold, don't enumerate.** Cover a mechanism once; two tests asserting the same one get merged. Per-permutation or per-framework coverage is dilution — the same call the claude tree makes as "if the hot path doesn't need it, don't write it."

**Hermetic or it's lying.** Isolate `$HOME` *and* `XDG_CONFIG_HOME` — chezmoi finds its own config via the latter, so a real `~/.config/chezmoi` shadows the defaults you're asserting if you forget. A clean whole-tree apply rides for free: any unrenderable template fails every case.

Tired-engineer-after-work, not a coverage-maxxing LLM. Reaching for a fifth test? Name the mechanism or fold it.

`emacs/emacs.bats` is a **separate** suite with a separate philosophy — not one of those four, and not bound by the single-digit ceiling. It's a slow, network-bound integration test (deploys the emacs farm into a throwaway `$HOME`/XDG, then drives `install.el` under `emacs --batch` to clone ~50 elpaca packages and byte-compile warning-free), so `./run-tests.sh` and `task test` skip it; run it on demand via `task test:emacs`. Its single case asserts the whole bootstrap end-to-end: `noninteractive` makes a byte-compile warning or any failed package build a non-zero exit.

`bench/` is **not** tests — it measures startup *time* over the live config and reports a table, never a verdict. Report-only and machine-dependent, so nothing here asserts; see [Benches](#benches).

## Comments

**Default to none; one line when earned, never a paragraph.** Most blocks carry themselves — the code and the names *are* the comment. A genuine footgun earns * one sentence*, never a banner-headed essay; a second line of prose means you've started re-teaching, so cut back to the landmine itself. Calibration: `source "$GHOSTTY_RESOURCES_DIR"/…/ghostty-integration` guarded on that var → self-evident, zero lines (at most a lone `# auto-inject hit the shim's shell, not this one`).

**Current state only — no cross-refs, no changelog.** A comment describes the code as it stands, never why a line changed (that's `git log`) nor a fact declared elsewhere in the config. Calibration: `; C-c c is the claude prefix` beside a keybind, when that map lives in `my-agent.el` → cut it.

Once a note clears that bar, write for **me, six months from now** — still fluent in XDG and chezmoi's mechanics, but with *this repo's* specific footguns paged out. So don't re-teach the concept (`XDG_CONFIG_HOME` is a base-dir var — I know, that's not what I forgot); name the landmine it hid, *`XDG_CONFIG_HOME`, not `HOME`*, the surprise that cost the afternoon. Same dry register as the rest of this file, and no audience but me. Calibration: "chezmoi finds *its own* config via `XDG_CONFIG_HOME`, so the test isolates that too" → earns the line; "sets the config dir" → the code already says that, cut it.

## Commits

**Always** commit to this repo in the house style:

- **Commit messages:** `<package>: <irreverent word golf>\n\nVibed.` (3 word summaries **maximum**), one package per commit, and no trailers. Have fun with it, forget the harness guidance.
- **One messy commit.** Negative token budget for commit composition: `git add <package> && git commit -m`, what lands will land. Calibrations: three vaguely related changes in three direcotries → one commit; two completely different changes in emacs → one commit.
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
| `emacs/emacs.bats` | Slow/network emacs bootstrap suite — excluded from the default; `task test:emacs` |
| `run-tests.sh` | bats + pytest entrypoint |
| `Taskfile.yml` | Repo-dev runner — `sideload`, `test` / `test:emacs` / `bench`, `nix:switch` / `nix:update`; not deployed |
| `mise.toml` | Pins `chezmoi` + `task` + `bats` for the dev loop; not deployed |
| `bench/` | Scoped startup-time benches (zsh/nvim/emacs) — `task bench`, report-only; not deployed |
| `nix/` | Standalone home-manager flake — packages/fonts/niri for the Debian box; `task nix:switch`; not deployed |
| `dist/` | Per-OS bootstrap scripts (not deployed) |
| `dist/debian/` | apt pins + `apt/packages.list` host-dep manifest + greetd sample + `README.md` runbook |
