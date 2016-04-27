(use-package markdown-mode
  :ensure t
)

(use-package deft
  :ensure t
  :config
  (progn
    ;; (add-hook 'deft-mode-hook 'turn-off-visual-line-mode)
	(setq deft-directory (expand-file-name "~/Dropbox/Text")
          deft-extensions '("txt" "md" "org")
    	  deft-use-filename-as-title t
          ;; deft-use-filter-string-for-filename t
          )
  	)
)

(provide 'my-notes)
