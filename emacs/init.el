(require 'package)
(package-initialize)
(setq package-enable-at-startup nil
      package-archives '(("melpa" . "https://melpa.org/packages/")))

;; For important compatibility libraries like cl-lib
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enabl use-package
(eval-when-compile
    (require 'use-package))
(require 'diminish)
(require 'bind-key)
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (byte-recompile-directory user-emacs-directory 0))

;; Disable extra gui elements
(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; Sensible defaults
(setq-default vc-follow-symlinks nil      ; don't prompt to follow version-controlled symlinks
	      inhibit-startup-screen t    ; disable startup screen
	      x-select-enable-clibpoard t ; use the system clipboard
	      )

(set-frame-font "Hack 12") ; sensible font
(global-font-lock-mode t) ; syntax-highlighting

;; TODO: working dark theme
;; leuven-theme is great, but it's still a light theme
(use-package leuven-theme :ensure t
  :init (load-theme 'leuven t))

;; Set the path variable to match the system PATH
(use-package exec-path-from-shell :ensure t
  :config (exec-path-from-shell-initialize))

;; Better command discoverability
(use-package which-key :ensure t :defer t
  :init (which-key-mode t)
  :config
  (setq which-key-popup-type 'minibuffer))

;; Pull in pdf-tools
(use-package pdf-tools :ensure t :defer t
  :config (pdf-tools-install)
          (setq-default pdf-view-display-size 'fit-page))

(use-package magit :ensure t :defer t
  :bind ("C-x g" . magit-status)) 

;; TODO: configure (http://ebzzry.io/en/emacs-pairs/)
(use-package smartparens :ensure t :defer t)

(use-package evil :ensure t
  :init (evil-mode 1)
  :config
  (defalias 'evil-insert-state 'evil-emacs-state) ;mask insert-state
  (setq evil-default-state 'emacs
        evil-search-module 'isearch
	evil-magic 'very-magic
	evil-want-fine-undo-want-fine-undo t
	evil-want-change-word-to-end t)
  :bind (:map evil-emacs-state-map (([escape] . evil-normal-state)
				    ("C-[" . evil-normal-state))))
  
(use-package evil-snipe :ensure t
  :diminish evil-snipe-local-mode
  :init (evil-snipe-mode 1))

(use-package undo-tree :ensure t
  :diminish undo-tree-mode)

;; better completion with ivy, counsel, and friends
(use-package ivy :ensure t
  :diminish ivy-mode
  :init (ivy-mode 1))

(use-package counsel :ensure t :defer t)

(use-package company :ensure t :defer t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :bind ("C-;" . company-complete-common))

(provide 'init)
