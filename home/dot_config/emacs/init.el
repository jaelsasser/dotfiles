;;; init.el --- custom emacs init file -*- lexical-binding: t; -*-
(add-to-list 'load-path (concat user-emacs-directory "conf"))
(require 'bootstrap)

(eval-when-compile
  (require 'use-package))
(use-package diminish)
(use-package bind-key :ensure nil)

(when (< emacs-major-version 31)
  (use-package compat :demand t)
  (use-package transient :demand t))

(use-package abbrev :ensure nil
  :diminish abbrev-mode)

(use-package eldoc :ensure nil
  :diminish eldoc-mode
  :custom (eldoc-idle-delay 1.0))

(use-package gcmh
  :diminish gcmh-mode
  :hook (after-init . gcmh-mode)
  :custom (gcmh-idle-delay 'auto))

;;;
;;; Sensible defaults
;;;

(use-package uniquify :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package autorevert :ensure nil
  :init (global-auto-revert-mode)
  :diminish auto-revert-mode)

(use-package whitespace :ensure nil
  :diminish (global-whitespace-mode whitespace-mode)
  :init (global-whitespace-mode)
  :custom
  (whitespace-style '(face trailing lines-tail))
  (whitespace-global-modes t)
  (whitespace-line-column nil)
  :config
  (defun my--whitespace-prog-p ()
    (derived-mode-p 'prog-mode))
  (add-function :before-while whitespace-enable-predicate
                #'my--whitespace-prog-p))

(use-package saveplace :ensure nil
  :init (save-place-mode 1))

(savehist-mode 1)

(setq use-short-answers t)

;; remap modifier keys on macOS (NS port: emacs-plus; 'mac kept for emacs-mac hosts)
(when (memq window-system '(ns mac))
  (setq ns-command-modifier  'none      ; ⌘ stays a macOS shortcut (Cmd-Q/C/V/W)
        ns-option-modifier   'meta      ; ⌥ → Meta
        ns-control-modifier  'control
        mac-command-modifier nil        ; emacs-mac equivalents; harmless dynamic vars on ns
        mac-option-modifier  'meta
        mac-control-modifier 'control))

(menu-bar-mode -1)                      ; tty frames too (the -nw bootstrap); tool/scroll-bar: early-init.el

;; ensure access to git on Windows, plus other tweaks
(defvar magit-git-executable)           ; set below before magit defines it
(defvar explicit-shell-file-name)       ; from shell.el, which isn't loaded (tramp defers)
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:\\tools\\msys64\\usr\\bin\\bash.exe")
  (setq shell-file-name "C:\\tools\\msys64\\usr\\bin\\bash.exe")
  (set-face-attribute 'default nil :font "Terminus-12")
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (setq magit-git-executable "C:\\Program Files\\Git\\bin\\git.exe")
  (setenv "PATH" (concat "C:\\tools\\msys64\\mingw64\\bin" ";"
                         "C:\\tools\\msys64\\usr\\local\\bin" ";"
                         "C:\\tools\\msys64\\usr\\bin" ";"
                         (getenv "PATH")))
  (setq exec-path (append exec-path '("C:\\tools\\msys64\\mingw64\\bin"))))

(setq-default require-final-newline 'save
              truncate-lines t
              indent-tabs-mode nil
              tab-width 4
              fill-column 80)

(setq auth-sources `(,(expand-file-name "~/.config/authinfo.gpg")
                     ,(expand-file-name "~/.config/authinfo"))
      load-prefer-newer t
      frame-title-format "%b"
      auto-hscroll-mode 'current-line

      bookmark-save-flag 1

      enable-recursive-minibuffers t
      disabled-command-function 'nil
      epa-pinentry-mode 'loopback
      x-underline-at-descent-line t

      scroll-conservatively 8
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1)

      show-paren-delay 0.1

      auto-save-default t
      version-control t
      delete-old-versions t
      backup-by-copying t
      create-lockfiles nil

      ediff-window-setup-function 'ediff-setup-windows-plain
      vc-follow-symlinks nil

      mouse-yank-at-point nil
      save-interprogram-paste-before-kill t
      select-enable-clipboard t
      select-enable-primary t

      split-height-threshold 120
      split-width-threshold 160

      xref-prompt-for-identifier ()     ; don't prompt on cross-references
      help-window-select t              ; shift focus to help window on C-h
      inhibit-startup-screen t)

(setq safe-local-variable-values
      '((glyphless-char-display . hex-code)
        (eval c-set-offset 'arglist-cont-nonempty '(c-lineup-arglist-intro-after-paren))
        (show-trailing-whitespace . t)))

;; via EmacsWiki: KillingAndYanking
(defun unix-werase-or-kill (arg)
  "When a region is active, `kill-region'; otherwise, `backward-kill-word'."
  (interactive "*p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(bind-keys ("C-w" . unix-werase-or-kill))

(declare-function which-function "which-func")
(defun which-func-insert-at-point ()
  (interactive)
  (insert (which-function)))
(bind-keys ("C-c y" . which-func-insert-at-point))

;; these are annoying
(bind-keys ("<mouse-2>" . nil)
           ("<down-mouse-2>" . nil))

(column-number-mode t)                  ; column number in mode line (line is default-on)

(defun ring-bell-function-minimal ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function #'ring-bell-function-minimal)


;;;
;;; Theme
;;;
(require 'my-theme)


;;;
;;; Font
;;;
(require 'my-font)


;;;
;;; Evil
;;;
(require 'my-evil)


;;;
;;; Editing
;;;

(defun my--setup-prog-mode ()
  (setq-local show-trailing-whitespace t)
  (toggle-truncate-lines 1))
(add-hook 'prog-mode-hook #'my--setup-prog-mode)

(defun my--large-file-hook ()
  "Turn off expensive functions (font-lock, undo-mode) for large files"
  (when (> (buffer-size) (* 1024 1024))
    (setq-local buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook #'my--large-file-hook)

(defun move-line-up ()
  "Move the current line up"
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move the current line down"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(bind-keys ("C-c p" . move-line-up)
           ("C-c n" . move-line-down))

(use-package corfu
  ;; corfu-auto stays nil: complete only when asked (C-M-i), as company was
  :init (global-corfu-mode)
  :bind (:map corfu-map
         ("M-." . corfu-info-location)))   ; company-show-location muscle memory

(use-package corfu-terminal                ; emacs 31 does tty child frames natively
  :when (< emacs-major-version 31)
  :after corfu
  :config (corfu-terminal-mode 1))

(use-package cape
  ;; depth 100 = global tail: dabbrev fires only where capf (eglot/elisp) yields nothing
  :init (add-hook 'completion-at-point-functions #'cape-dabbrev 100))

(use-package goto-chg
  :bind ("M-]" . goto-last-change))

(use-package smartparens
  :custom
  (sp-base-key-bindings nil)
  (sp-highlight-wrap-overlay nil)
  (sp-show-pair-delay 0)
  :hook
  ((emacs-lisp-mode . smartparens-strict-mode)
   (after-init . smartparens-global-mode))
  :bind (:map smartparens-mode-map
         ("C-]" . nil)
         ("C-)" . sp-forward-slurp-sexp)
         ("C-(" . sp-forward-barf-sexp)))
(use-package smartparens-config :ensure nil
  :after smartparens)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))


;;;
;;; Navigation
;;;

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode t)
  :custom
  (ivy-re-builders-alist '((counsel-descbinds . ivy--regex)
                           (t . ivy--regex-plus)))
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :bind (("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c r" . ivy-resume)))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :init (counsel-mode t)
  :preface
  (defun my--counsel-grep-use-swiper-p ()
    (or (not (file-exists-p (buffer-file-name)))
        (counsel-grep-use-swiper-p-default)))
  :custom
  (counsel-grep-use-swiper-p #'my--counsel-grep-use-swiper-p)
  (counsel-find-file-at-point t)
  :config
  (when (boundp 'counsel--git-grep-count-threshold)
    (setq counsel--git-grep-count-threshold 200))
  :bind (("C-M-y"   . counsel-yank-pop)
         ("C-c f"   . counsel-git)
         ("C-c s"   . counsel-git-grep)
         ("C-c j"   . counsel-imenu)
         ("C-x r b" . counsel-bookmark)))

(use-package swiper
  :bind (("C-s"   . counsel-grep-or-swiper)
         ("C-M-s" . search-forward)))

;; edit grep/counsel hits in place: ivy-occur (C-c C-o), then C-x C-q
(use-package wgrep
  :custom (wgrep-auto-save-buffer t)
  :bind (:map grep-mode-map
         ("C-c C-p" . wgrep-change-to-wgrep-mode)))

(use-package ivy-rich
  :after (ivy counsel)
  :init (ivy-rich-mode 1))

(use-package prescient
  :config (prescient-persist-mode 1))

(use-package ivy-prescient
  :after (ivy counsel prescient)
  :custom
  ;; keep ivy--regex-plus filtering; prescient handles sorting only
  (ivy-prescient-enable-filtering nil)
  :init (ivy-prescient-mode 1))

(use-package corfu-prescient
  :after (corfu prescient)
  :init (corfu-prescient-mode 1))

(use-package avy
  :bind (("M-g h"   . avy-goto-char-2)
         ("M-g c"   . avy-goto-char)
         ("M-g g"   . avy-goto-line)
         ("M-g M-g" . avy-goto-line)))

(use-package ace-window
  :custom (aw-scope 'frame)
  :bind (([remap other-window] . ace-window)
         ("M-o" . ace-window)))

(use-package transpose-frame
  :commands transpose-frame
  :bind ("C-x 7" . transpose-frame))

(use-package ibuffer :ensure nil
  :commands ibuffer
  :custom
  (ibuffer-expert t) ; don't prompt for confirmation on delete
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-title-face 'font-lock-type-face)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("M-o" . nil)))

(use-package which-key :ensure nil
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

(use-package beginend
  :diminish (beginend-global-mode beginend-prog-mode)
  :hook (after-init . beginend-global-mode))


;;;
;;; Shell
;;;

(use-package eshell :ensure nil
  :preface
  (defun my--setup-eshell ()
    (setenv "TERM" "emacs"))
  :hook (eshell-mode . my--setup-eshell)
  :custom
  (eshell-destroy-buffer-when-process-dies t))


;;;
;;; Git
;;;

(use-package magit
  :custom
  (magit-diff-paint-whitespace t)
  (magit-repository-directories `(("~/Repos" . 1)
                                  ("~/Upstream" . 1)
                                  ("~/Upstream/llvm/tools/clang" . 0)
                                  ("~/Upstream/llvm/tools/clang/tools/extra" . 0)))
  :bind (("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)
         ("C-c M-g" . nil))
  :config
  (setq git-commit-known-pseudo-headers '("Signed-off-by"
                                          "Suggested-by"
                                          "Reported-by"
                                          "Tested-by"
                                          "Reviewed-by"
                                          "Acked-by"
                                          "Fixes"
                                          "Cc"))

  ;; macOS pty setup is slow and magit spawns git constantly; pipe instead.
  ;; Loses in-buffer passphrase prompts, but ssh-agent + gpg-agent have us.
  (when (eq system-type 'darwin)
    (setq magit-process-connection-type nil))

  ;; Windows spawns are the worst; drop the priciest refresh work.
  (when (eq system-type 'windows-nt)
    (setq magit-refresh-status-buffer nil
          magit-revision-insert-related-refs nil)
    (dolist (fn '(magit-insert-unpushed-to-pushremote
                  magit-insert-unpushed-to-upstream-or-recent
                  magit-insert-unpulled-from-pushremote
                  magit-insert-unpulled-from-upstream))
      (remove-hook 'magit-status-sections-hook fn))
    (remove-hook 'server-switch-hook #'magit-commit-diff)
    (remove-hook 'with-editor-filter-visit-hook #'magit-commit-diff)))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :custom (diff-hl-draw-borders nil)
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package difftastic-bindings
  :ensure difftastic                       ; difft needed only at invocation
  :config (difftastic-bindings-mode))


;;;
;;; IDE
;;;

(use-package editorconfig :ensure nil
  :hook (after-init . editorconfig-mode))

(use-package eglot :ensure nil
  :hook
  ;; treesit-auto remaps to the -ts- modes when grammars are present, so hook both
  ((c-mode c++-mode python-mode go-mode
    c-ts-mode c++-ts-mode python-ts-mode go-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoreconnect nil)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :config
  (add-to-list
   'eglot-server-programs '((swift-mode objc-mode) . ("xcrun" "sourcekit-lsp"))))

(use-package project :ensure nil
  :init
  (defun project-try-compdb (dir)
    (when-let* ((match (locate-dominating-file dir "compile_commands.json")))
      (cons 'compdb (expand-file-name match))))
  (cl-defmethod project-root ((project (head compdb)))
    (cdr project))
  :config
  (add-to-list 'project-find-functions #'project-try-compdb))

(use-package flymake :ensure nil
  :custom
  (flymake-proc-allowed-file-name-masks nil)
  :bind (("C-c w" . flymake-show-buffer-diagnostics)))


;;;
;;; Tools
;;;

;; GUI Emacs misses the shell's PATH only where the build doesn't bake it in.
;; emacs-plus (macOS) doesn't, so sync there; Linux/Windows inherit it already.
(use-package exec-path-from-shell
  :when (eq system-type 'darwin)
  :init
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-shell-name "zsh")
  (exec-path-from-shell-arguments '("-l"))) ; not -i: env is in .zshenv; -i loads antidote every launch

(use-package envrc
  :when (executable-find "direnv")
  :hook (after-init . envrc-global-mode))

(use-package tramp :ensure nil
  :defer t
  :custom
  (tramp-verbose 2)
  (tramp-default-method "ssh")
  (tramp-chunksize 500)
  :config
  (add-to-list 'tramp-default-proxies-alist
               '("\\.jaalam\\.net\\'" "\\`root\\'" "/ssh:admin@%h:")))

(use-package man :ensure nil
  :defer t
  :custom
  (Man-header-file-path (list (expand-file-name "~/.local/include")
                              "/usr/include"
                              "/usr/local/include"
                              "/usr/include/x86_64-linux-gnu")))

(use-package ffap :ensure nil
  :defer t
  :custom
  ;; prevents Emacs from doing anything too fancy when C-x f
  ;; happens to point at a file when invoked
  (ffap-machine-p-unknown 'reject)
  (ffap-machine-p-local 'reject)
  (ffap-machine-p-known 'reject))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package jq-mode :defer t)


;;;
;;; Languages
;;;

(use-package cc-mode :ensure nil
  :defer t
  :custom
  (c-basic-offset 4)
  :config
  (defvar c-syntactic-element)          ; cc-mode binds it around lineup calls
  (defun c-lineup-arglist-tabs-only (_ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1) c-basic-offset)))
  (c-set-offset 'arglist-cont-nonempty
                '(c-lineup-gcc-asm-reg c-lineup-arglist-tabs-only)))

(use-package clang-format
  :commands (clang-format-region clang-format-buffer))

(use-package kotlin-mode
  :mode ("\\.kt" . kotlin-mode))

(use-package go-mode
  :mode ("\\.go" . go-mode)
  :config
  (add-hook
   'go-mode-hook (lambda ()
                   (add-hook 'before-save-hook 'gofmt-before-save nil 'local))))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :custom
  (rust-format-on-save t))

(use-package python :ensure nil
  :commands python-mode
  :custom (python-shell-interpreter "python3"))

(use-package ruby-mode :ensure nil
  :defer t
  :custom (ruby-indent-level 4))

(use-package markdown-mode :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  :custom
  (markdown-asymmetric-header t)
  (markdown-header-scaling t)
  :bind (:map markdown-mode-map ("C-c C-c l" . markdown-table-align)))

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :custom
  (web-mode-enable-current-element-highlight t))

(use-package systemd
  :mode (("\\.service" . systemd-mode)
         ("\\.path" . systemd-mode)))

(use-package nsis-mode
  :mode (("\\.nsi" . nsis-mode)))

(use-package org
  :config
  (setq org-startup-indented nil
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-whole-heading-line t
        org-list-allow-alphabetical t

        org-format-latex-options '(:foreground auto :background auto :scale 2)

        org-highlight-latex-and-related '(latex script entities)
        org-babel-load-languages '((emacs-lisp . t)
                                   (python . t))
        org-babel-python-command "python3")
  (setq python-shell-prompt-detect-failure-warning nil)

  (add-hook 'org-mode-hook #'visual-line-mode)

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c i" . org-capture)
         ("C-c b" . org-switchb)))


;;;
;;; Modern QOL
;;;

(pixel-scroll-precision-mode 1)
(repeat-mode 1)

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
  :ensure (combobulate :host github :repo "mickeynp/combobulate")
  :hook ((python-ts-mode go-ts-mode yaml-ts-mode json-ts-mode toml-ts-mode)
         . combobulate-mode))


;;;
;;; Agent
;;;
(require 'my-agent)

(provide 'init)
