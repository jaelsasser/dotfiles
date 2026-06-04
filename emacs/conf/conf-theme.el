;;; conf-theme.el --- solarized with jae face overrides -*- lexical-binding: t; -*-

(use-package solarized-theme :defer nil
  :custom
  (solarized-distinct-fringe-background nil)
  (solarized-high-contrast-mode-line nil)
  (solarized-scale-org-headlines t)
  (solarized-use-variable-pitch nil)
  (solarized-use-more-italic nil)
  :config
  (defvar els--current-theme nil
    "The solarized variant currently enabled.")
  (deftheme els--solarized-light)
  (deftheme els--solarized-dark)
  (eval-when-compile
      (require 'solarized-palettes))
  (require 'solarized-theme)
  (defvar els--solarized-faces
    '("Customized solarized faces."
      (custom-theme-set-faces
             theme-name
             ;; font-lock: minimize color accents in source code
             `(font-lock-type-face ((,class (:foreground ,base0 :underline t))))
             `(font-lock-variable-name-face ((,class (:foreground ,blue))))
             `(font-lock-function-name-face ((,class (:foreground ,base0 :weight bold))))

             ;; info: don't scale faces
             `(info-menu-header ((,class (:inherit s-variable-pitch :weight ,s-maybe-bold))))
             `(Info-quoted ((,class (:inherit font-lock-constant-face))))

             ;; markdown: don't scale code blocks
             `(markdown-code-face ((,class (:inherit org-block))))

             ;; markdown: scale headings
             `(markdown-header-face-1 ((,class (:inherit markdown-header-face
                                                         ,@(when solarized-scale-org-headlines
                                                             (list :height solarized-height-plus-4))))))
             `(markdown-header-face-2 ((,class (:inherit markdown-header-face
                                                         ,@(when solarized-scale-org-headlines
                                                             (list :height solarized-height-plus-3))))))
             `(markdown-header-face-3 ((,class (:inherit markdown-header-face
                                                         ,@(when solarized-scale-org-headlines
                                                             (list :height solarized-height-plus-2))))))
             `(markdown-header-face-4 ((,class (:inherit markdown-header-face
                                                         ,@(when solarized-scale-org-headlines
                                                             (list :height solarized-height-plus-1))))))
             `(markdown-header-face-5 ((,class (:inherit markdown-header-face))))
             `(markdown-header-face-6 ((,class (:inherit markdown-header-face))))

             ;; org: clarity
             `(org-block ((,class (:background ,base03 :foreground ,base00))))
             `(org-block-begin-line ((,class (:inherit font-lock-comment-face :underline t))))
             `(org-block-end-line ((,class (:inherit font-lock-comment-face :overline t)))))))
  (solarized-with-color-variables
    'light 'els--solarized-light solarized-light-color-palette-alist els--solarized-faces)
  (solarized-with-color-variables
    'dark 'els--solarized-dark solarized-dark-color-palette-alist els--solarized-faces)
  ;; Activation: follow the macOS system appearance where the NS port reports it
  ;; (emacs-plus), with C-c t as a manual override. Both routes flow through
  ;; `els--enable-theme', so the override and the next system flip stay in sync.
  (defun els--enable-theme (theme)
    "Enable solarized variant THEME, recording it as current."
    (setq els--current-theme theme)
    (let ((custom--inhibit-theme-enable nil))
      (enable-theme theme)))

  (defun els--theme-for-appearance (appearance)
    "Enable the solarized variant matching APPEARANCE.
Light maps to light; dark and the terminal's nil both map to dark."
    (els--enable-theme (if (eq appearance 'light)
                           'els--solarized-light
                         'els--solarized-dark)))

  (defun invert-theme ()
    "Flip between the light and dark solarized variants."
    (interactive)
    (els--enable-theme (if (eq els--current-theme 'els--solarized-dark)
                           'els--solarized-light
                         'els--solarized-dark)))

  ;; emacs-plus fires the hook on every OS light/dark flip; it's bound even in a
  ;; terminal (value nil → dark). Non-NS builds get a static dark.
  (if (boundp 'ns-system-appearance)
      (progn
        (add-hook 'ns-system-appearance-change-functions
                  #'els--theme-for-appearance)
        (els--theme-for-appearance ns-system-appearance))
    (els--enable-theme 'els--solarized-dark))
  :bind (("C-c t" . invert-theme)))

(provide 'conf-theme)
