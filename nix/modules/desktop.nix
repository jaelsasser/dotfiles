{ pkgs, ... }:
{
  # All plain entries — the gpu module supplies GL via /run/opengl-driver, so no per-binary wrapping.
  home.packages = with pkgs; [
    ghostty            # GTK4+EGL — finds /run/opengl-driver, no wrap needed
    neovide            # Skia/OpenGL — same; pass --neovim-bin or rely on neovim on PATH
    neovim             # 0.12.2 (vim.pack); also feeds neovide
    niri               # 26.04; drives EGL/GBM directly → /run/opengl-driver
    emacs30-pgtk       # GTK3 Cairo software surfaces over wl_shm — never touches EGL; tree-sitter + native-comp on by default
  ];

  # No emacs-pgtk alias exists — use emacs30-pgtk. It software-renders, so the gpu module is a no-op for it.
}
