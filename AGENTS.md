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

**Run the regression tests:**
```bash
bats stow.bats       # install with `brew install bats-core`
./run-tests.sh       # runs bats -r claude/tests/ recursively; pass bats args or a path to filter
```
Tests run against a temp `$HOME` — they never touch the real one. Play test-case golf to give a radically small number of tests full user-facing-behaviour coverage.

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
│   └── .stow-local-ignore  # (optional) per-package ignore patterns on top of .stowrc
└── dist/             # per-OS bootstrap scripts (not stowed)
```

### Default stow target

`stow.sh` stows each package into `$XDG_CONFIG_HOME/<pkg>` (i.e., `~/.config/<pkg>`). This means package files live flat at the package root — `git/config` lands at `~/.config/git/config`.

### `link.sh` — custom targets

If a package needs a non-`~/.config/<pkg>` target, add a `link.sh` that calls stow directly. `stow.sh` calls `link.sh` instead of its own stow invocation, then calls `configure.sh` afterwards as usual.

The `claude` package is the current example — it targets `~/.claude` instead of `~/.config/claude`:

```bash
# claude/link.sh
ACTION=${1:--R}
stow "$ACTION" claude -t "$HOME/.claude" -d "$DOTFILES_ROOT"
```

`stow.sh` passes `$STOW_ACTION` (e.g., `-R`, `-D`) as `$1`; `link.sh` should default to `-R` so direct invocation still works.

### `configure.sh` — post-install hooks

Runs after stow (or after `link.sh`) on every `-R` or `-S` action, skipped on `-D`. Use for:
- Creating directories that stow won't create
- Writing symlinks that need renaming (e.g., `claude/configure.sh` links `USER_CLAUDE.md` → `~/.claude/CLAUDE.md`)
- Bootstrapping external tools (e.g., cloning antidote, creating Python venvs)

### `.stow-local-ignore`

When present in a package, replaces stow's built-in default ignore list (CVS, RCS, `.git`, etc.) for that package. **It does not override `--ignore` flags from `.stowrc`** — those CLI ignores apply to every package on every run. Use `.stow-local-ignore` only to *add* per-package patterns on top of `.stowrc`'s set.

`zsh/.stow-local-ignore` is the real example here: it skips the vendored `antidote/` subtree, which `.stowrc`'s patterns don't match.

Caveat: there is no per-package way to *unfilter* something `.stowrc` already ignores. If a package needs to stow a `.md` or `.png` file, drop the matching CLI ignore from `.stowrc`.

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
| `zsh` | `~/.config/zsh` | `configure.sh` injects `ZDOTDIR` into `/etc/zshenv`; runtime plugin manager is antidote |
| `alacritty` | `~/.config/alacritty` | Config is still `.yml` — needs migration to `.toml` (see Known issues) |
| `i3` | `~/.config/i3` | i3 window manager |
| `X11` | `~/.config/X11` | `xinitrc`, `xresources` |
| `xmonad` | `~/.config/xmonad` | `configure.sh` bootstraps xmonad/xmobar |
| `dist/` | — | Not stowed; per-OS (debian, macos, eclipse) bootstrap scripts |

`**/CLAUDE.md → **/AGENTS.md` via symlink.

## Known issues

**`alacritty/alacritty.yml` is in the deprecated YAML format.**  
Alacritty moved to TOML (`alacritty.toml`) and may have dropped YAML support. Needs migration.

## Key constraints

- **XDG everywhere.** New packages target `~/.config/<pkg>` by default. Stray `~/.*` files are a smell — check `xdg.sh` for a redirect pattern first.
- **`link.sh` is for target overrides only.** Don't put general setup logic there; that belongs in `configure.sh`.
- **`configure.sh` must be idempotent.** It runs on every re-stow. Guard mutations with existence checks.

## Commits

Always commit to this repo in the house style:

- **Commit messages:** `<package>: <irreverent word golf summary>\n\nVibed.` (4 word summaries **maximum**), one package per commit, and no trailers.
- **One messy commit.** Negative token budget for this, never inspect diffs or strategize beyond 'every change made it in'.

## Key files

| Path | Purpose |
|---|---|
| `stow.sh` | Installer entrypoint — iterates packages, calls stow / link.sh / configure.sh |
| `stow.bats` | bats-core regression tests for `stow.sh` + per-package `link.sh` wrappers |
| `.stowrc` | Global stow flags and ignore patterns |
| `sh/xdg.sh` | XDG variable bootstrap + per-tool XDG redirects |
| `sh/profile.sh` | Login-shell environment (PATH, etc.) |
| `claude/USER_CLAUDE.md` | User-level Claude instructions — `configure.sh` links as `~/.claude/CLAUDE.md` |
| `claude/commands/vet.md` | `/vet` slash command for Claude Code |
| `git/config` | Git identity, aliases, merge/diff tool wiring |
| `zsh/zshrc` | Interactive zsh config — plugin loading, history, antidote |
| `zsh/zshenv` | All-shells zsh env — sets `ZDOTDIR`, sources `sh/profile.sh` |
| `emacs/init.el` | Emacs config |
| `bin/ediff.sh` | Emacs-client merge driver for `git mergetool` |
| `dist/` | Per-OS bootstrap scripts (not managed by stow) |
