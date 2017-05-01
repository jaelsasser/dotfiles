(require 'package)
(package-initialize)
(setq custom-file "~/.emacs.d/custom.el" ;; this isn't sourced or tracked
      package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;;; Bootstrap and enable use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
    (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;; Sensible defaults

(require 'uniquify)
(require 'saveplace)

(setq-default indent-tabs-mode nil
	      save-place t
              ibuffer-saved-filter-groups ; more pleasant ibuffer filtering
              '(("default"
                 ("dired" (mode . dired-mode))
                 ("erc" (mode . erc-mode))
                 ("emacs" (or (name . "^\\*scratch\\*$")
                              (name . "^\\*Messages\\*$"))))))

(setq save-place-file (concat user-emacs-directory "places")
      backup-directory-alist '(("." . "~/.emacs.d/backups"))

      ibuffer-expert t
      ibuffer-show-empty-filter-groups nil

      show-paren-delay 0

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
           ("C-x C-b" . ibuffer-other-window)

           ("C-w" . unix-werase-or-kill)
	   
	   ("C-s" . isearch-forward-regexp)
	   ("C-r" . isearch-backward-regexp)
	   ("C-M-s" . isearch-forward)
	   ("C-M-r" . isearch-backward))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "default")))

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

;;; Frame and modeline customization

(global-auto-revert-mode t)             ; reload files on disk
(diminish 'auto-revert-mode)

(global-font-lock-mode t)		; syntax highlighting
(winner-mode t)				; better window management
(show-paren-mode t)                     ; highlight matching parens

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(if (eq system-type 'darwin)
    (set-frame-font "Terminus (TTF)-18:antialias=none:hint=none" nil t)
  (set-frame-font "Terminus-13:antialias=none:hint=none" nil t))

(defun ring-bell-function-minimal ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell       nil
      ring-bell-function #'ring-bell-function-minimal)

;;; Color themes

(use-package leuven-theme :ensure t :disabled
  :init (load-theme 'leuven t))
(use-package solarized-theme :ensure t
  :config
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil
        solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))
(use-package zerodark-theme :ensure t :disabled
  :init (load-theme 'zerodark t))
(use-package tao-theme :ensure t :disabled)
(use-package rainbow-mode :ensure t :defer t)

;;; Basic qol

;; ace-window for quick jumps
(use-package ace-window :ensure t
  :bind ("M-p" . ace-window))

;; make sure PATH matches our shell path on macOS
(use-package exec-path-from-shell :ensure t
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;; better key discovery
(use-package which-key :ensure t :defer t
  :init (which-key-mode t)
  :diminish which-key-mode
  :config (which-key-setup-side-window-right-bottom)
  :bind ("C-h b" . which-key-show-top-level))

;; C-x u to visualize a branching history of a file
(use-package undo-tree :ensure t
  :diminish undo-tree-mode)

;; ivy + counsel for fuzzy minibuffer matching
(use-package ivy :ensure t
  :init (ivy-mode t)
  :diminish ivy-mode
  :config
  (setq
   ivy-use-virtual-buffers t
   ;; fuzzy matching in ivy buffers
   ivy-initial-inputs-alist nil
   ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
(use-package counsel :ensure t :defer t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

;; complete-anywhere framework
(use-package company :ensure t
  :init (global-company-mode)
  :diminish company-mode
  :config
  (setq company-idle-delay nil		; only complete when asked (C-M-i)
        company-tooltip-align-annotations t)
  :bind ([remap completion-at-point] . company-complete))

;; expand ibuffer-mode with TRAMP, version control
(use-package ibuffer-vc :ensure t :defer t)
(use-package ibuffer-tramp :ensure t :defer t)

;; expand macros, inspect asm
(use-package macrostep :ensure t)
(use-package disaster :ensure t)

;; flx and smex provide additional sort for ivy+counsel
(use-package flx :ensure t :defer t)
(use-package smex :ensure t :defer t)

;; flycheck - enable with flycheck-mode, not loaded by default
(use-package flycheck :ensure t :defer t)

;;; Tools

;; magit is awesome
(use-package magit :ensure t :defer t
  :init (global-magit-file-mode t)
  :diminish magit-file-mode
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)))
(use-package diff-hl :ensure t :defer t)

(use-package projectile :ensure t :defer t
  :init (projectile-mode)
  :config
  (setq projectile-completion-system 'ivy))

;; Deft is basically nvAlt, but in Emacs
(use-package deft :ensure t :defer t)

;; track the upstream org-mode version
(use-package org :ensure t :defer t
  :config (setq org-startup-indented t))

;;; Editing helpers

;; sexp editing everywhere
;; TODO: spent 30m ingraining these shortcuts
(use-package smartparens-config :ensure smartparens :disabled
  :diminish smartparens-mode
  :config (smartparens-global-mode)
  :bind
  (:map smartparens-mode-map ; see http://ebzzry.io/en/emacs-pairs/
  	("C-M-a" . sp-beginning-of-sexp)
  	("C-M-e" . sp-end-of-sexp)

  	("C-M-n" . sp-next-sexp)
  	("C-M-p" . sp-previous-sexp)
	
  	("C-M-f" . sp-forward-sexp)
  	("C-M-b". sp-backward-sexp)

  	("C-S-f" . sp-forward-symbol)
  	("C-S-b" . sp-backward-symbol)

  	("C-M-t" . sp-transpose-sexp)
  	("C-M-k" . sp-kill-sexp)
  	("C-k"   . sp-kill-hybrid-sexp)
  	("M-k"   . sp-backward-kill-sexp)
  	("C-M-w" . sp-copy-sexp)
  	("C-M-d" . delete-sexp)
	
  	("M-[" . sp-backward-unwrap-sexp)
  	("M-]" . sp-unwrap-sexp)

  	("M-<up>" . sp-backward-up-sexp)
  	("M-<down>" . sp-backward-down-sexp)
  	("C-<up>" . sp-up-sexp)
 	("C-<down>" . sp-down-sexp)))

;; fallback modal-editing environmanet
(use-package evil :ensure t :disabled
  :config
  (custom-set-variables ; make sure to hit the evil setup hooks
   '(evil-default-state 'emacs)
   '(evil-disable-insert-state-bindings t)
   '(evil-motion-state-modes '())
   '(evil-magic 'very-magic)
   '(evil-search-module 'isearch)
   '(evil-want-change-word-to-end t)
   '(evil-want-fine-undo t))
  (evil-mode t))

;;; Languages

(use-package anaconda-mode :ensure t :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :bind ([remap anaconda-mode-complete] . company-complete))
(use-package company-anaconda :ensure t :defer t
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (add-to-list 'company-backends 'company-anaconda))))

(use-package go-mode :ensure t :defer t
  :config
  (add-hook
   'go-mode (lambda ()
	      (add-hook 'before-save-hook 'gofmt-before-save nil 'local))))
(use-package go-eldoc :ensure t :defer t
  :config (add-hook 'go-mode 'go-eldoc-setup))

(use-package haskell-mode :ensure t :defer t)

(use-package markdown-mode :ensure t :defer t)

(use-package rust-mode :ensure t :defer t
  :config
  (setq rust-format-on-save t))
(use-package racer :ensure t :defer t 
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package tex :ensure auctex :defer t
  :config
  (put 'TeX-narrow-to-group 'disabled nil))
(use-package pdf-tools :ensure t :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(use-package web-mode :ensure t :defer t
  :config
  (setq web-mode-enable-current-element-highlight t))

(provide 'init)
