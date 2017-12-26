;; Terminal
(use-package eshell :ensure nil
  :preface
  (defun jae--setup-eshell ()
    (setenv "TERM" "emacs"))
  :hook (eshell-mode . jae--setup-eshell)
  :custom
  (eshell-destroy-buffer-when-process-dies t))

(use-package company-pcomplete :disabled
  :after (eshell company)
  :commands company-pcomplete
  :preface
  (defun jae--setup-pcomplete ()
    "It's surprisingly hard to bind keys within eshell-mode"
    (setq company-backends '(company-pcomplete))
    (bind-key "<tab>" 'company-complete eshell-mode-map))
  :hook (eshell-mode . jae--setup-eshell))

(use-package eshell-bookmark
  :commands eshell-bookmark-setup
  :hook (eshell-mode . eshell-bookmark-setup))

(use-package fish-completion
  :commands fish-completion-mode turn-on-fish-completion-mode
  :hook (eshell-mode . turn-on-fish-completion-mode))

(use-package esh-autosuggest
  :commands esh-autosuggest-mode
  :preface
  (defun jae--setup-company-eshell-autosuggest ()
    "Fish-like autosuggestion in Eshell"
    (setq-local company-backends '(company-eshell-autosuggest))
    (setq-local company-frontends '(company-preview-if-just-one-frontend))
    (setq-local company-idle-delay 0.5))
  :hook (eshell-mode . esh-autosuggest-mode))

(provide 'conf-shell)
