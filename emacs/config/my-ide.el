(use-package fiplr
  :ensure t
  :config
  (progn
    (setq fiplr-root-markers '(".git")
    	  fiplr-ignored-globs '((directories (".git"))
                                (files ("*.jpg" "*.png" "*.zip" "*~"))
                                )
          )
    (global-set-key (kbd "C-c p") 'fiplr-find-file)
    )
)

;; (use-package projectile
;;   :ensure t
;;   :config
;;   (progn
;;     (projectile-global-mode)
;;     (setq projectile-completion-system 'grizzl
;;           projectile-enable-caching t
;;           )
;;   )
;; )

(use-package stickyfunc-enhance
  :ensure t
  :init
  (progn
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
    (require 'stickyfunc-enhance)
  )
)

(use-package semantic
  :ensure t
  :init
  (semantic-mode 1)
)

(provide 'my-ide)
