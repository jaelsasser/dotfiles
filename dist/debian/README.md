# Debian Trixie host bootstrap

The host-side (apt / kernel / seat) layer beneath the niri Wayland desktop. The Nix flake
at `nix/` owns the apps, fonts, and the niri binary; this tree is everything Nix deliberately
*doesn't* touch. The full host ⇄ Nix boundary is in the repo `AGENTS.md`.

`apt/sources.list.d` + `apt/preferences.d` are the long-standing apt pins; `apt/packages.list`
is the desktop host-dep set.

## 1. apt

    grep -vE '^\s*#|^\s*$' apt/packages.list | xargs sudo apt install -y

## 2. Nix (Determinate — flakes on by default, no nix.conf editing)

    curl -fsSL https://install.determinate.systems/nix | sh -s -- install

Open a new shell, then bootstrap home-manager (not installed yet):

    nix run home-manager/master -- switch --flake ~/.local/share/chezmoi/nix#josh@trixie

Thereafter: `task nix:switch`. (Mesa → no `--impure`.)

## 3. GPU

Run the `sudo .../non-nixos-gpu-setup` line the switch prints, then reboot (or restart the
tmpfiles unit + re-login). Populates `/run/opengl-driver`; re-run only after a switch that
changes the Mesa version.

## 4. Session services

    systemctl --user enable --now pipewire pipewire-pulse wireplumber
    systemctl --user daemon-reload      # picks up niri.service via ~/.config/systemd/user.conf

## 5. greetd

    sudo install -Dm644 greetd/config.toml /etc/greetd/config.toml
    sudo systemctl enable greetd

Smoke-test from tty1 first: `exec niri-session`.
