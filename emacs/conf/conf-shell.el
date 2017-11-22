;; Terminal
(use-package eshell :ensure nil
  :init
  (require 'company-pcomplete)
  ;; binding keys in mode maps is hard, apparently
  (defun jae--setup-eshell ()
    (bind-key "<tab>" 'company-pcomplete eshell-mode-map))
  (add-hook 'eshell-mode-hook 'jae--setup-eshell)
  :config
  (setq eshell-destroy-buffer-when-process-dies t))

(use-package eshell-bookmark :ensure t
  :init
  (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))

(use-package fish-completion :ensure t
  :if (executable-find "fish")
  :init
  (add-hook 'eshell-mode-hook 'fish-completion-mode))

(use-package company-eshell-autosuggest :ensure t :disabled
  :init
  (defun jae--setup-company-eshell-autosuggest ()
    (with-eval-after-load 'company
      (setq-local company-backends '(company-eshell-autosuggest))
      (setq-local company-frontends '(company-preview-if-just-one-frontend))))
  (add-hook 'eshell-mode-hook 'jae--setup-company-eshell-autosuggest))

(provide 'conf-shell)
