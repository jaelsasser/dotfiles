(use-package markdown-mode
  :ensure t
)

(use-package deft
  :ensure t
  :config
  (progn
    ;; (add-hook 'deft-mode-hook 'turn-off-visual-line-mode)
	(custom-set-variables
        '(deft-directory (expand-file-name "~/Dropbox/Text"))
        '(deft-extensions '("txt" "md" "org"))
        '(deft-use-filename-as-title t)
        ;; '(deft-use-filter-string-for-filename t)
        )
    )
  	;; no vim-y goodness in deft
    (add-to-list 'evil-emacs-state-modes 'deft-mode)
    (define-key deft-mode-map (kbd "C-j") 'next-line)
    (define-key deft-mode-map (kbd "C-k") 'previous-line)
)

(provide 'my-notes)
