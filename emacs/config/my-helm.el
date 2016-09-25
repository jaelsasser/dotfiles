(use-package helm
  :ensure t
  :config
  (progn
    (setq helm-buffer-max-length 40)

    ;; 10-30% of screen space
    (helm-autoresize-mode t)
    (setq helm-autoresize-min-height 10
          helm-autoresize-max-height 30)
    ;; sane keybindings
	(define-key helm-map (kbd "C-k") 'helm-previous-line)
	(define-key helm-map (kbd "C-j") 'helm-next-line)
    )
)

(use-package helm-ag
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c f") 'helm-ag-project-root)
    )
  )

(use-package helm-swoop
  :ensure t
  :config
  (progn
    (setq helm-swoop-pre-input-function #'ignore
          helm-swoop-use-line-number-face t
          helm-swoop-split-with-multiple-windows t
          helm-swoop-speed-or-color t
          helm-swoop-use-fuzzy-match t
          )
    )
)

(provide 'my-helm)
