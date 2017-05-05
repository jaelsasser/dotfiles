;;; init.el --- custom emacs init file
(require 'package)
(setq custom-file "~/.emacs.d/custom.el" ;; this isn't sourced or tracked
      package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 10)
                                   ("elpa" . 5)
                                   ("melpa" . 0))
      package-enable-at-startup nil
       package-user-dir (format "~/.local/share/emacs/elpa-%s/" emacs-major-version)
      use-package-enable-imenu-support t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(diminish 'abbrev-mode)


;;;
;;; Sensible defaults
;;;

(require 'uniquify)
(require 'saveplace)

;; make sure PATH matches our shell path on macOS
(use-package exec-path-from-shell :ensure t
  :if (memq window-system '(mac ns))
  :init (setq exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              save-place t
              ibuffer-saved-filter-groups ; more pleasant ibuffer filtering
              '(("default"
                 ("dired" (mode . dired-mode))
                 ("erc" (mode . erc-mode))
                 ("emacs" (or (name . "^\\*scratch\\*$")
                              (name . "^\\*Messages\\*$"))))))

(setq save-place-file (concat user-emacs-directory "places")
      backup-directory-alist '(("." . "~/.local/share/emacs/backups"))
      auto-save-default nil
      load-prefer-newer t

      show-paren-delay 0
      x-underline-at-descent-line t

      tramp-default-method "ssh"
      tramp-chunksize 512

      version-control t
      delete-old-versions t
      backup-by-copying t
      create-lockfiles nil

      ediff-window-setup-function 'ediff-setup-windows-plain
      uniquify-buffer-name-style 'forward
      vc-follow-symlinks nil

      mouse-yank-at-point t
      save-interprogram-paste-before-kill t
      select-enable-clibpoard t
      select-enable-primary t

      apropos-do-all t
      inhibit-startup-messages t
      inhibit-startup-screen t
      load-prefer-newer t
      visible-bell t)

;; via EmacsWiki: KillingAndYanking
(defun unix-werase-or-kill (arg)
  (interactive "*p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(bind-keys ("M-/" . hippie-expand)
           ("C-w" . unix-werase-or-kill)
           ("C-x k" . kill-this-buffer)

           ("C-s" . isearch-forward-regexp)
           ("C-r" . isearch-backward-regexp)
           ("C-M-s" . isearch-forward)
           ("C-M-r" . isearch-backward))

;;; Enable disabled commands
(put 'downcase-region             'disabled nil)   ; let downcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; let esc-esc work
(put 'narrow-to-page              'disabled nil)   ; let narrowing work
(put 'narrow-to-region            'disabled nil)   ; let narrowing work
(put 'set-goal-column             'disabled nil)   ; C-n and C-p respects goal-column
(put 'upcase-region               'disabled nil)   ; let upcasing work
(put 'company-coq-fold            'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'dired-find-alternate-file   'disabled nil)

;; Frame and modeline customization

(global-auto-revert-mode t)             ; reload files on disk
(diminish 'auto-revert-mode)

(line-number-mode t)                    ; line number in mode line
(column-number-mode t)                  ; column number in mode line

(global-font-lock-mode t)		; syntax highlighting
(winner-mode t)				; better window management
(show-paren-mode t)                     ; highlight matching parens

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(if (eq system-type 'darwin)
    (set-frame-font "Menlo-13" nil t)
  (set-frame-font "Terminus-14:antialias=none:hint=none" nil t))

(defun ring-bell-function-minimal ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell       nil
      ring-bell-function #'ring-bell-function-minimal)


;;;
;;; Color themes
;;;

(use-package solarized-theme :ensure t
  :config
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil
        solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(use-package leuven-theme :ensure t :disabled)
(use-package zerodark-theme :ensure t :disabled)
(use-package tao-theme :ensure t :disabled)


;;;
;;; General Plugins
;;;

;; Better Key Discovery
(use-package which-key :ensure t
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t)
  :bind ("C-h b" . which-key-show-top-level))

;; Minibuffer Matching
(use-package ivy :ensure t
  :init (ivy-mode t)
  :diminish ivy-mode
  :config
  (setq
   ivy-use-virtual-buffers t
   enable-recursive-minibuffers t
   ;; fuzzy matching in ivy buffers
   ivy-initial-inputs-alist nil
   ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
(use-package counsel :ensure t :defer t
  :config
  (setq counsel-find-file-at-point t)
  :bind (("M-x" . counsel-M-x)
         ("C-M-y" . counsel-yank-pop)
         ("C-c r" . counsel-recentf)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-load-library)
         ("C-c u" . counsel-unicode-char)
         ("C-." . counsel-imenu)))
(use-package flx :ensure t :defer t)    ; better fuzzy-match sort
(use-package smex :ensure t :defer t)   ; better M-x sort

;; Code Completion
(use-package company :ensure t
  :init (global-company-mode)
  :diminish company-mode
  :config
  (setq company-idle-delay nil		; only complete when asked (C-M-i, usually)
        company-tooltip-align-annotations t)
  :bind (([remap completion-at-point] . company-complete)
         ([remap complete-symbol] . company-complete)))

;; Window Management
(use-package ace-window :ensure t
  :bind ([remap other-window] . ace-window))
(use-package transpose-frame :ensure t :defer t)
(use-package ibuffer
  :config
  ;; expand ibuffer-mode with TRAMP, version control
  (use-package ibuffer-vc :ensure t :defer t)
  (use-package ibuffer-tramp :ensure t :defer t)

  (setq ibuffer-expert t                ; don't prompt for confirmation on delete
        ibuffer-show-empty-filter-groups nil)
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1)
               (ibuffer-switch-to-saved-filter-groups "default")))
  :bind ("C-x C-b" . ibuffer))

;; Buffer Navigation
(use-package avy :ensure t :defer t
  :bind ("C-:" . avy-goto-char-2))
(use-package swiper :ensure t :defer t :disabled
  :bind ("C-s" . swiper))

;; Misc Utilities
(use-package macrostep :ensure t :defer t)
(use-package rainbow-delimiters :ensure t :defer t)
(use-package rainbow-mode :ensure t :defer t)

;; Linting
(use-package flycheck :ensure t :defer t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; Terminal
(use-package eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t))
(use-package multi-term :ensure t :defer t)

;; Version Control
(use-package magit :ensure t
  :init (global-magit-file-mode t)
  :diminish magit-file-mode
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-paint-whitespace t)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))
(use-package diff-hl :ensure t       ; subtle git-gutter in the fringe
  :init
  (global-diff-hl-mode)
  (diff-hl-dired-mode)
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package undo-tree :ensure t     ; file-local branching undo
  :diminish undo-tree-mode)

;; Projects
(use-package projectile :ensure t :defer t :pin melpa
  :init (projectile-mode)
  :config
  (use-package counsel-projectile :ensure
    :init (counsel-projectile-on))
  
  (setq projectile-completion-system 'ivy
        projectile-mode-line            ; don't show an empty Projectile indicator
        '(:eval (if (projectile-project-p)
                    (format " Projectile[%s]"
                            (projectile-project-name))
                  ""))))
(use-package deft :ensure t :defer t   ; lightweight text file indexing
  :config
  (setq deft-directory "~/.local/text"))

;; sexp editing everywhere
(use-package smartparens-config :ensure smartparens :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (sp-use-smartparens-bindings))

;; expand regions by semantic regions
(use-package expand-region :ensure t :defer t
  :bind ("C-=" . er/expand-region))

;; fallback modal-editing environmanet
(use-package evil :ensure t :disabled
  :init
  (setq
   evil-default-state 'emacs
   evil-disable-insert-state-bindings t
   evil-motion-state-modes '()
   evil-magic 'very-magic
   evil-search-module 'isearch
   evil-want-change-word-to-end t
   evil-want-fine-undo 'fine)
  (evil-mode t))

;; Snippets
(use-package yasnippet :ensure t :defer t)


;:;
;;; LANGUAGES
;;;

(use-package lsp-mode :ensure t)

;; C/C++
(use-package irony :ensure t :pin melpa
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  
  (use-package company-irony :ensure t :defer t)
  (use-package company-irony-c-headers :ensure t :defer t)
  
  (add-hook
   'irony-mode-hook (lambda ()
                      (irony-cdb-autosetup-compile-options)
                      (irony-eldoc)
                      
                      (make-local-variable 'company-backends)
                      (set (make-local-variable 'company-backends)
                           '(company-irony-c-headers company-irony company-capf)))))

(use-package ggtags :ensure t :disabled
  :init
  (add-hook 'c-mode-common-hook 'ggtags-mode))

(use-package rtags :ensure t :pin melpa
  :init
  (add-hook
   'irony-mode-hook (lambda ()
                      (require 'irony-cdb-json)
                      (when-let (cdb (irony-cdb-json--locate-db))
                        (rtags-start-process-unless-running)
                        (rtags-call-rc :silent "-J" cdb)
                        (rtags-enable-standard-keybindings)))))
(use-package disaster :ensure t)

;; Go
(use-package go-mode :ensure t :defer t
  :config
  (use-package company-go :ensure t :defer t)
  (use-package go-eldoc :ensure t :defer t)
  
  (add-hook
   'go-mode-hook (lambda ()
                   (set (make-local-variable 'company-backends)
                        '(company-go company-capf))
                   (add-hook 'before-save-hook 'gofmt-before-save nil 'local)
                   (go-eldoc-setup))))

;; Rust
(use-package rust-mode :ensure t :defer t
  :config
  (setq rust-format-on-save t))
(use-package racer :ensure t :defer t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

;; LaTeX
(use-package tex :ensure auctex :defer t
  :config
  (put 'TeX-narrow-to-group 'disabled nil))
(use-package pdf-tools :ensure t :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

;; Org
(use-package org :ensure t :defer t
  :init (setq org-startup-indented t))

;; Python
(use-package anaconda-mode :ensure t
  :commands anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config
  (use-package company-anaconda :ensure t :defer t)
  
  (add-hook 
   'python-mode-hook (lambda ()
                       (set (make-local-variable 'company-backends)
                            'company-anaconda)))
  :bind ([remap anaconda-mode-complete] . company-complete))


;; Haskell
(use-package haskell-mode :ensure t)

;; Markdown
(use-package markdown-mode :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode 'visual-line-mode))

;; HTML/CSS/JS
(use-package web-mode :ensure t
  :mode (("\\.html?\\'". web-mode)
         ("\\.[tj]sx?\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))

(provide 'init)
