(use-package zenburn-theme
   :ensure zenburn-theme
   :config
   (progn
     (unless noninteractive
     (load-theme 'zenburn t))))

;; line numbers with nlinum
(use-package nlinum
  :ensure nlinum
  :config
  (progn
    (global-nlinum-mode t)
    (setq-default nlinum-format " %d "))
  )

(global-hl-line-mode t)

;; Show parentheses
(show-paren-mode 1)

(provide 'my-appearance)
