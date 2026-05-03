# AGENTS.md

## What this project is

A [GNU Stow](https://www.gnu.org/software/stow/)-based dotfiles repo for macOS and Linux. Each top-level directory is a stow *package* — a tree of config files that `stow.sh` symlinks into place. The XDG Base Directory spec is enforced repo-wide: almost everything lands under `~/.config/`, `~/.local/share/`, or `~/.cache/` rather than bare `~/.*` files.

## Commands

**Install all packages:**
```bash
./stow.sh            # re-stow everything in the default TARGETS list, then run configure.sh for each
```

**Install or reinstall a specific package:**
```bash
./stow.sh claude     # re-stow one package
./stow.sh -R claude  # explicit re-stow (same as above — -R is the default)
```

**Unstow a package:**
```bash
./stow.sh -D claude
```

**Dry-run a single package (bypass stow.sh):**
```bash
stow --simulate -R <pkg> -t ~/.config/<pkg> -d .
# or, for packages with a custom target:
stow --simulate -R claude -t ~/.claude -d .
```

> **Note:** `stow.sh` must be run from the repo root — it sources `sh/xdg.sh` via a relative path. This is a known bug; see Known issues below.

## Architecture

### Package layout

```
dotfiles/
├── stow.sh           # installer: iterates TARGETS, calls stow / link.sh / configure.sh
├── .stowrc           # global stow flags: --no-folding, ignore patterns
├── sh/xdg.sh         # sourced by stow.sh; sets XDG_* vars and XDG-redirect aliases/exports
├── <pkg>/            # one directory per stow package
│   ├── link.sh       # (optional) overrides the default stow invocation for this package
│   ├── configure.sh  # (optional) runs after stow for post-install setup
│   └── .stow-local-ignore  # (optional) replaces global ignore rules for this package
└── dist/             # per-OS bootstrap scripts (not stowed)
```

### Default stow target

`stow.sh` stows each package into `$XDG_CONFIG_HOME/<pkg>` (i.e., `~/.config/<pkg>`). This means package files live flat at the package root — `git/config` lands at `~/.config/git/config`.

### `link.sh` — custom targets

If a package needs a non-`~/.config/<pkg>` target, add a `link.sh` that calls stow directly. `stow.sh` calls `link.sh` instead of its own stow invocation, then calls `configure.sh` afterwards as usual.

The `claude` package is the current example — it targets `~/.claude` instead of `~/.config/claude`:

```bash
# claude/link.sh
stow -R claude -t "$HOME/.claude" -d "$DOTFILES_ROOT"
```

**Caveat:** `link.sh` does not receive `STOW_ACTION`, so `stow.sh -D <pkg>` will re-stow instead of unstowing for packages with a `link.sh`. Workaround: call stow manually for unstow.

### `configure.sh` — post-install hooks

Runs after stow (or after `link.sh`) on every `-R` or `-S` action, skipped on `-D`. Use for:
- Creating directories that stow won't create
- Writing symlinks that need renaming (e.g., `claude/configure.sh` links `USER_CLAUDE.md` → `~/.claude/CLAUDE.md`)
- Bootstrapping external tools (e.g., cloning zplug, creating Python venvs)

### `.stow-local-ignore`

When present in a package, **replaces** the global `.stowrc` ignore rules entirely for that package. The global rules ignore `.*\.md`, `.*\.png`, `configure\.sh`, `link\.sh`. If a package needs to stow `.md` files (e.g., `claude/commands/vet.md`), add a local ignore that lists only what should be skipped.

### XDG compliance

`sh/xdg.sh` sets all four XDG variables and then re-points every tool that doesn't respect them natively via env vars or aliases. New packages should follow the same pattern — no bare `~/.*` files unless the tool leaves no other option.

## Packages

| Package | Stow target | Notes |
|---|---|---|
| `bash` | `~/.config/bash` | `configure.sh` installs bash completion |
| `bin` | `~/.config/bin` | `ediff.sh` — Emacs merge tool for `git mergetool` |
| `claude` | `~/.claude` | `link.sh` for custom target; `configure.sh` for `USER_CLAUDE.md → CLAUDE.md` rename |
| `emacs` | `~/.config/emacs` | `configure.sh` creates XDG data dirs and a Python venv |
| `ghostty` | `~/.config/ghostty` | Ghostty terminal emulator; theme + macOS option-key + shell-integration |
| `git` | `~/.config/git` | `config` + `ignore`; GPG signing key `3D3C5256` |
| `sh` | `~/.config/sh` | XDG bootstrap (`xdg.sh`), `profile.sh`, dircolors |
| `tmux` | `~/.config/tmux` | `configure.sh` installs TPM |
| `vim` | `~/.config/vim` | `configure.sh` installs vim-plug |
| `zsh` | `~/.config/zsh` | `configure.sh` injects `ZDOTDIR` into `/etc/zshenv`; runtime plugin manager is zplug |
| `alacritty` | `~/.config/alacritty` | Config is still `.yml` — needs migration to `.toml` (see Known issues) |
| `i3` | `~/.config/i3` | i3 window manager |
| `X11` | `~/.config/X11` | `xinitrc`, `xresources` |
| `xmonad` | `~/.config/xmonad` | `configure.sh` bootstraps xmonad/xmobar |
| `dist/` | — | Not stowed; per-OS (debian, macos, eclipse) bootstrap scripts |

## Known issues

**`stow.sh` requires CWD = repo root.**  
`source sh/xdg.sh` is a relative path. Running `~/Repos/dotfiles/stow.sh` from another directory silently breaks XDG setup. Fix: use `source "$(dirname "$0")/sh/xdg.sh"`.

**`stow.sh` only processes one positional argument.**  
The argument-parsing loop exits after the first non-flag arg. `./stow.sh git zsh` installs only `git`. Fix: wrap the arg-parse block in a `while [[ "$#" -ge 1 ]]` loop.

**`link.sh` packages don't receive `STOW_ACTION`.**  
`./stow.sh -D claude` re-stows instead of unstowing. Fix: have `stow.sh` pass `$STOW_ACTION` as `$1` to `link.sh` and honor it there.

**`ZPLUG_CACHE_DIR` uses a nonexistent variable.**  
`zsh/zshrc` sets `${XDG_DATA_CACHE:-...}` — no such variable. Should be `$XDG_CACHE_HOME`.

**`alacritty/alacritty.yml` is in the deprecated YAML format.**  
Alacritty moved to TOML (`alacritty.toml`) and may have dropped YAML support. Needs migration.

## Key constraints

- **XDG everywhere.** New packages target `~/.config/<pkg>` by default. Stray `~/.*` files are a smell — check `xdg.sh` for a redirect pattern first.
- **`link.sh` is for target overrides only.** Don't put general setup logic there; that belongs in `configure.sh`.
- **`configure.sh` must be idempotent.** It runs on every re-stow. Guard mutations with existence checks.
- **One logical change per commit.** Config and tooling changes go in separate commits.

## Key files

| Path | Purpose |
|---|---|
| `stow.sh` | Installer entrypoint — iterates packages, calls stow / link.sh / configure.sh |
| `.stowrc` | Global stow flags and ignore patterns |
| `sh/xdg.sh` | XDG variable bootstrap + per-tool XDG redirects |
| `sh/profile.sh` | Login-shell environment (PATH, etc.) |
| `claude/USER_CLAUDE.md` | User-level Claude instructions — `configure.sh` links as `~/.claude/CLAUDE.md` |
| `claude/commands/vet.md` | `/vet` slash command for Claude Code |
| `git/config` | Git identity, aliases, merge/diff tool wiring |
| `zsh/zshrc` | Interactive zsh config — plugin loading, history, zplug |
| `zsh/zshenv` | All-shells zsh env — sets `ZDOTDIR`, sources `sh/profile.sh` |
| `emacs/init.el` | Emacs config |
| `bin/ediff.sh` | Emacs-client merge driver for `git mergetool` |
| `dist/` | Per-OS bootstrap scripts (not managed by stow) |
