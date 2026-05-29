# dotfiles

A [chezmoi](https://www.chezmoi.io/)-managed dotfiles repo for macOS and Linux. The source tree lives under `home/`; `chezmoi apply` materializes it into `$HOME`. All config lands under `$XDG_CONFIG_HOME` (`~/.config/`) by default.

## Commands

```bash
chezmoi apply                # apply the whole tree (idempotent)
chezmoi apply -n -v          # dry-run: show the diff, touch nothing
chezmoi edit --apply ~/.config/git/config   # edit a managed file, then apply
dist/migrate-to-chezmoi.sh   # first install / cutover from the old stow layout
./run-tests.sh               # bats + pytest, against a temp $HOME
```

## Architecture

See [AGENTS.md](AGENTS.md) for the source layout, chezmoi naming conventions, the live `claude/` symlink farm, the `settings.json` merge, externals, OS gating, and the stow handover.

## Known issues

**`alacritty/alacritty.yml` is in the deprecated YAML format.** Alacritty moved to TOML and may have dropped YAML support. Needs migration.
