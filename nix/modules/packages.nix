{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # toolchain (runtime tree-sitter grammar compile: nvim-treesitter + emacs treesit-auto)
    gcc                  # provides `gcc` AND `cc` (cc-wrapper symlink) on PATH
    git python3 go       # go bundles gofmt
    tree-sitter          # CLI
    # LSP / formatters
    clang-tools          # ONE attr → clangd + clang-format (do NOT also add clang/llvm)
    basedpyright
    lua-language-server  # lua_ls
    # CLI
    ripgrep fd dtach abduco   # dtach/abduco feed the ghostty shell-shim
    difftastic zoxide         # difftastic's binary is `difft`
    direnv nix-direnv
    xdg-utils wl-clipboard
  ];

  # `cc` caveat: pkgs.gcc puts cc/gcc on PATH, but runtime grammar compilation linking against
  # Nix's glibc on Debian is unverified end-to-end. If :TSInstall / treesit-auto misbehaves, fall
  # back to `apt install build-essential` + the host /usr/bin/cc. Either way editors must launch
  # from a shell with ~/.nix-profile/bin on PATH (the .zshenv hm-session-vars source) to see Nix gcc.
}
