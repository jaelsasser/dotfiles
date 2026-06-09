{ pkgs, ... }:
{
  # Writes ~/.config/fontconfig/conf.d/10-hm-fonts.conf. Trixie's /etc/fonts/conf.d/50-user.conf
  # (default-on) includes $XDG_CONFIG_HOME/fontconfig/conf.d → apt apps reliably see these fonts.
  # (Inverse caveat: a few *Nix* apps hardcode FONTCONFIG_FILE and bypass the chain; apt apps don't.)
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    monaspace nerd-fonts.monaspace        # Monaspace Neon (+ patched)
    nerd-fonts.commit-mono                # CommitMono: only the nerd-patched variant exists in nixpkgs
    jetbrains-mono nerd-fonts.jetbrains-mono
    hack-font      nerd-fonts.hack
  ];

  # nerdfonts.override { fonts = [...]; } is a hard error since Nov 2024 — nerd-fonts.<name> only.
  # Berkeley Mono excluded (paid): drop it in ~/.local/share/fonts/, the HM fontconfig chain finds it.
}
