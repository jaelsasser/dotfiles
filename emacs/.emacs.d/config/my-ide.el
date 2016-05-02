(use-package git-gutter
  :ensure t
  :config
  (progn
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
    (custom-set-variables
      '(git-gutter:update-interval 2))
    )
)


(provide 'my-ide)
