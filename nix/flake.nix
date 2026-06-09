{
  description = "josh@trixie home-manager (packages/fonts/niri only; chezmoi owns configs)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";   # neovim 0.12/vim.pack is unstable-only
    home-manager = {
      url = "github:nix-community/home-manager";             # master: carries targets.genericLinux.gpu
      inputs.nixpkgs.follows = "nixpkgs";                    # one nixpkgs instance
    };
  };

  outputs = { nixpkgs, home-manager, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;     # belt-and-suspenders for font licence metadata
    };
  in {
    homeConfigurations."josh@trixie" = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [ ./home.nix ];
    };
  };
}
