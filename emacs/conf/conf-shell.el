;; Terminal
(use-package eshell :ensure nil
  :custom
  (eshell-destroy-buffer-when-process-dies t))

(use-package company-pcomplete :disabled
  :after eshell
  :commands company-pcomplete
  :init
  (defun jae--setup-pcomplete ()
    "It's surprisingly hard to bind keys within eshell-mode"
    (setq company-backends '(company-pcomplete))
    (bind-key "<tab>" 'company-complete eshell-mode-map))
  (add-hook 'eshell-mode-hook #'jae--setup-eshell))

(use-package eshell-bookmark
  :commands eshell-bookmark-setup
  :init
  (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))

(use-package fish-completion
  :commands fish-completion-mode
  :if (executable-find "fish")
  :init
  (add-hook 'eshell-mode-hook 'fish-completion-mode))

(use-package company-eshell-autosuggest :ensure t :disabled
  :commands company-eshell-autosuggest
  :init
  (defun jae--setup-company-eshell-autosuggest ()
    (with-eval-after-load 'company
      (setq-local company-backends '(company-eshell-autosuggest))
      (setq-local company-frontends '(company-preview-if-just-one-frontend))))
  (add-hook 'eshell-mode-hook 'jae--setup-company-eshell-autosuggest))

(provide 'conf-shell)
