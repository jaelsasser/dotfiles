# dotfiles

[chezmoi](https://www.chezmoi.io/)-managed dotfiles for macOS and Linux, XDG everywhere. `chezmoi apply` reads from a standalone clone at `~/.local/share/chezmoi` and materializes it into `$HOME`; **this repo is the dev checkout**, decoupled from that clone — edits here stage until promoted.

Architecture and per-tree conventions live in [`AGENTS.md`](AGENTS.md) (plus scoped `AGENTS.md` under `home/`, `nix/`, `bench/`, `claude/`).

## Install

```bash
chezmoi init --apply <repo-url>   # clone the source, prompt for the per-host git email, apply
```

## Apply

```bash
chezmoi apply              # materialize the clone's `stable` branch into $HOME
chezmoi apply -n -v        # dry-run: print the diff without touching $HOME
chezmoi update             # other machines: git pull origin/stable in the clone, then apply
```

## Edit a managed file

`chezmoi edit` operates on the clone (the live source); editing this dev checkout instead stages until promoted.

```bash
chezmoi edit --apply ~/.config/git/config   # edit the clone's source, apply
# or: edit home/dot_config/git/config here, commit on `main`, then promote
```

## Stage & promote (dev checkout → live)

```bash
mise install                                      # one-time: fetch pinned task + chezmoi
# work on `main` in this checkout, commit, then:
task sideload                                     # rebase the clone's stable onto local/main, show the diff
chezmoi apply                                     # go live
git -C ~/.local/share/chezmoi push origin stable  # publish
```

## Tests

```bash
./run-tests.sh        # bats chezmoi.bats + pytest under uv (NOT emacs.bats)
task test             # same, via the Taskfile (extra args after --)
task test:emacs       # the slow emacs bootstrap suite — clones ~50 packages, needs emacs
```

## Benches & the Nix layer

```bash
task bench           # zsh + nvim + emacs scoped startup tables (report-only)
task nix:switch      # Debian box only: home-manager switch from the clone's nix/ flake
```

Benches: [`bench/AGENTS.md`](bench/AGENTS.md). Nix package layer (niri desktop over Debian): [`nix/AGENTS.md`](nix/AGENTS.md), first-install runbook in [`dist/debian/README.md`](dist/debian/README.md).
