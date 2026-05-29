;;; conf-theme.el --- solarized with jae face overrides -*- lexical-binding: t; -*-

(use-package solarized-theme :defer nil
  :custom
  (solarized-distinct-fringe-background nil)
  (solarized-high-contrast-mode-line nil)
  (solarized-scale-org-headlines t)
  (solarized-use-variable-pitch nil)
  (solarized-use-more-italic nil)
  :config
  (setq jae--current-theme 'jae--solarized-light)
  (deftheme jae--solarized-light)
  (deftheme jae--solarized-dark)
  (eval-when-compile
      (require 'solarized-palettes))
  (require 'solarized-theme)
  (defvar jae--solarized-faces
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
    'light 'jae--solarized-light solarized-light-color-palette-alist jae--solarized-faces)
  (solarized-with-color-variables
    'dark 'jae--solarized-dark solarized-dark-color-palette-alist jae--solarized-faces)
  (defun invert-theme ()
    (interactive)
    (setq jae--current-theme (if (eq jae--current-theme 'jae--solarized-dark)
                                'jae--solarized-light
                              'jae--solarized-dark))
    (let* ((custom--inhibit-theme-enable nil))
      (enable-theme jae--current-theme)))
  (invert-theme)
  :bind (("C-c t" . invert-theme)))

(provide 'conf-theme)
