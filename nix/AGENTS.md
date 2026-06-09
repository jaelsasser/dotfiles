# AGENTS.md — Nix package layer

`nix/` is a standalone [home-manager](https://nix-community.github.io/home-manager/) flake that drops a *modern* toolchain and a [niri](https://github.com/YaLTeR/niri) Wayland desktop onto a non-NixOS **Debian Trixie** box, where apt's freeze can't. Dev-only, **never deployed** (same posture as `dist/`/`bench/`) — it sits at the repo root, outside the chezmoi source tree.

Strict division of labour: **Nix owns binaries, fonts, and the niri compositor binary; chezmoi still owns every config file** (niri's `config.kdl` included) — no home-manager `programs.*` modules, so the two never dual-own a dotfile.

## Commands

```bash
task nix:switch     # home-manager switch from the clone's nix/ flake
task nix:update     # bump flake inputs, then switch
```

`task nix:switch` reads the **clone** (`<clone>/nix`, promoted state), so the loop is `edit nix/ here → task sideload → task nix:switch` — the mirror-image of `chezmoi apply`, which reads the clone too. First-install + verification runbooks live in `dist/debian/README.md`.

## Layout

`flake.nix` (inputs: `nixpkgs-unstable` — neovim 0.12/`vim.pack` is unstable-only — + home-manager `master` for `targets.genericLinux.gpu`; output `homeConfigurations."josh@trixie"`), `home.nix` (identity + `targets.genericLinux{,.gpu}.enable`), and three single-concern `modules/`: `packages.nix` (toolchain + LSPs + CLI), `fonts.nix` (`fonts.fontconfig.enable` + `nerd-fonts.*`), `desktop.nix` (ghostty / neovide / neovim / niri / emacs30-pgtk). `flake.lock` is **not** committed from here — there's no Nix on macOS; it's generated on the first switch on the Trixie box.

## GPU — the gpu module, not nixGL

`targets.genericLinux.gpu.enable` symlinks Nix's Mesa into `/run/opengl-driver`, so every Nix GL app finds drivers with no per-binary wrapping. Cost: a switch that changes the Mesa version prints a `sudo .../non-nixos-gpu-setup` line — run it, reboot. Mesa (Intel/AMD) means **pure builds**: no `--impure`, no version pinning, no `.gpu.nvidia` block.

## The host ⇄ Nix boundary

Nix ships the apps/fonts/niri binary; the kernel DRM driver + firmware, systemd-logind seat, greetd+tuigreet, pipewire stack, xdg portals, and the polkit agent are all **apt** (`dist/debian/` is the manifest + runbook). The seam has four load-bearing glue pieces, all chezmoi-owned and Linux-only (the `.chezmoiignore` darwin block ignores `.config/{niri,xdg-desktop-portal,systemd}`):

- **`~/.config/systemd/user.conf`** — `[Manager] ManagerEnvironment=XDG_DATA_DIRS=%h/.nix-profile/share:…`. The systemd *user manager* computes its unit search path from its **own** `XDG_DATA_DIRS` at startup; `environment.d` does **not** reach it. Without this, niri's Nix-installed `niri.service` is "not found" → `graphical-session.target` never fires → portals cascade-fail.
- **Login-shell PATH** — the greeter runs `zsh -l -c niri-session`, a *non-interactive login* shell, which reads `dot_zshenv` but **not** `dot_zshrc`; so the `hm-session-vars.sh` source lives in `dot_zshenv` (guarded by `-f`, inert until a switch), else `niri-session` can't resolve `niri` by name.
- **Portals** — `niri-portals.conf` routes `default=gtk` with ScreenCast/Screenshot to `gnome`; gnome otherwise wins by D-Bus priority then fails silently. Never set `GDK_BACKEND=wayland` globally — it breaks the screencast portal.
- **Seat** — logind + uaccess, nothing to install (Trixie's systemd 257 predates the 258 uaccess regression); needs a *real* logind session (greeter/TTY, never SSH).

niri is `pkgs.niri` (26.04, version parity with Arch/Fedora); its `homeModules.niri` is unused — the `niri.service` it ships is NixOS-only.
