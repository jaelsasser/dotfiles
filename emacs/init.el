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

;;; Disable extra gui elements

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;;; Enable disabled commands

(put 'downcase-region             'disabled nil)   ; let downcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; let esc-esc work
(put 'narrow-to-page              'disabled nil)   ; let narrowing work
(put 'narrow-to-region            'disabled nil)   ; let narrowing work
(put 'set-goal-column             'disabled nil)   ; C-n and C-p respecs goal-column
(put 'upcase-region               'disabled nil)   ; let upcasing work
(put 'company-coq-fold            'disabled nil)
(put 'TeX-narrow-to-group         'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

;;; Sensible defaults

(setq-default
 ;; swap / lockfie handling
 version-control t
 delete-old-versions t
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 create-lockfiles nil

 vc-follow-symlinks nil
 
 select-enable-clibpoard t
 select-enable-primary t
 
 inhibit-startup-messages t
 inhibit-startup-screen t)

;; Built-in global modes
(global-font-lock-mode t)		; syntax highlighting
(winner-mode)				; better window management
(global-font-lock-mode t)		; syntax-highlighting

(set-frame-font "Hack 11" nil t)	; sensible font

;;; Color themes

(use-package leuven-theme :ensure t
  :init (load-theme 'leuven t))
(use-package tao-theme :ensure t)
(use-package rainbow-mode :ensure t :defer t)

;;; Basic qol

;; make sure PATH matches our shell path on macOS
(use-package exec-path-from-shell :ensure t
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;; better key discovery
(use-package which-key :ensure t :defer t
  :init (which-key-mode t)
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-right-bottom)
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
   ;; fuzzy matching in ivy buffers
   ivy-initial-inputs-alist nil
   ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
(use-package counsel :ensure t :defer t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)))

;; complete-anywhere framework
(use-package company :ensure t :defer t
  :diminish company-mode
  :init (global-company-mode)
  :config
  (setq company-idle-delay nil)		; only complete when I to
  :bind ([remap completion-at-point] . company-complete))

;; flx and smex provide additional sort for ivy+counsel
(use-package flx :ensure t :defer t)
(use-package smex :ensure t :defer t)

;; flycheck - enable with flycheck-mode
(use-package flycheck :ensure t :defer t)

;;; Tools

;; magit is awesome
(use-package magit :ensure t :defer t
  :init (global-magit-file-mode t)
  :diminish magit-file-mode
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)))

;; Deft is basically nvAlt, but in Emacs
(use-package deft :ensure t :defer t)

;; track the upstream org-mode version
(use-package org :ensure t :defer t)

;;; Editing helpers

;; sexp editing everywhere
(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config
  (show-smartparens-global-mode)
  (smartparens-global-mode)
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
(use-package evil
  :ensure t
  :config
  (custom-set-variables
    '(evil-default-state 'emacs)
    '(evil-disable-insert-state-bindings t)
    '(evil-motion-state-modes '())
    '(evil-magic 'very-magic)
    '(evil-search-module 'isearch)
    '(evil-want-change-word-to-end t)
    '(evil-want-fine-undo t))
  (evil-mode t)

  ;TODO: god-mode and evil-god-state
  
  (use-package evil-snipe :ensure t
    :init (evil-snipe-mode t)
    :diminish evil-snipe-local-mode))

;;; Language modes

;; pdf-tools provides some useful enhancements over doc-view
(use-package pdf-tools :ensure t :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(use-package markdown-mode :ensure t :defer t)

(use-package go-mode :ensure t :defer t
  :config
  (add-hook
   'go-mode (lambda ()
	      (add-hook 'before-save-hook 'gofmt-before-save nil 'local))))
(use-package go-eldoc :ensure t :defer t
  :config (add-hook 'go-mode 'go-eldoc-setup))

(provide 'init)
