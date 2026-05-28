# dotfiles

A [GNU Stow](https://www.gnu.org/software/stow/)-based dotfiles repo for macOS and Linux. All config lands under `$XDG_CONFIG_HOME` (`~/.config/`) by default.

## Commands

```bash
./stow.sh            # stow all packages
./stow.sh claude     # stow one package
./stow.sh -D claude  # unstow
./run-tests.sh       # bats test suite (runs against a temp $HOME)
```

## Architecture

See [AGENTS.md](AGENTS.md) for the package list, `link.sh`/`configure.sh` conventions, XDG wiring, and constraint details.

## Known issues

**`alacritty/alacritty.yml` is in the deprecated YAML format.** Alacritty moved to TOML and may have dropped YAML support. Needs migration.
