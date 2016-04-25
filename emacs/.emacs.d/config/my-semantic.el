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

(provide 'my-semantic)
