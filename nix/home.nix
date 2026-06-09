{ ... }:
{
  imports = [ ./modules/packages.nix ./modules/fonts.nix ./modules/desktop.nix ];

  home.username      = "josh";
  home.homeDirectory = "/home/josh";
  home.stateVersion  = "25.11";   # set ONCE; never bump without reading release notes

  targets.genericLinux.enable     = true;  # XDG_DATA_DIRS/TERMINFO/XCURSOR + hm-session-vars; required off-NixOS
  targets.genericLinux.gpu.enable = true;  # /run/opengl-driver via sudo systemd-tmpfiles (Mesa: no extra config)

  # Do NOT enable programs.zsh/bash — chezmoi owns shell config (also dodges HM #8076 PATH reorder).
}
