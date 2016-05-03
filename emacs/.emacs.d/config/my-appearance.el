(use-package git-gutter
  :ensure t
  :config
  (progn
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
    (custom-set-variables
     	;; update every x seconds
        '(git-gutter:update-interval 1)
        ;; we're probably using visual mode
        '(git-gutter:visual-line t)
        ;; hide gutter when no diffs
        '(git-gutter:hide-gutter t)
        ;; saner symbols
        '(git-gutter:modified-sign "*")
        '(git-gutter:added-sign "+")
        '(git-gutter:deleted-sign "-")
        )
    )
)

(use-package linum-relative
    :ensure t
    :config
   	(progn
      (global-linum-mode t)
      (linum-relative-on)
      (custom-set-variables
        '(linum-relative-format " %s ")
		)
    )
)

;; TODO: doesn't play the nicest with evil mode
;; (use-package rainbow-mode
;;   :ensure t
;; )

(use-package zenburn-theme
   :ensure t
   :config
   (progn
     (load-theme 'zenburn t)
     ;; highlight current line -- not currently working
     (global-hl-line-mode t)
   )
)

(provide 'my-appearance)
