;;; init.el --- custom emacs init file -*- lexical-binding: t; -*-
(add-to-list 'load-path (concat user-emacs-directory "conf"))
(require 'bootstrap)

(eval-when-compile
  (require 'use-package))
(use-package diminish)
(use-package bind-key :ensure nil)

(use-package abbrev :ensure nil
  :diminish abbrev-mode)

(use-package eldoc :ensure nil
  :diminish eldoc-mode
  :custom (eldoc-idle-delay 1.0))


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
  (defun jae--whitespace-prog-p ()
    (derived-mode-p 'prog-mode))
  (add-function :before-while whitespace-enable-predicate
                #'jae--whitespace-prog-p))

(use-package saveplace :ensure nil
  :init (save-place-mode 1)
  :custom
  (save-place-file (user-emacs-file "places")))

(savehist-mode 1)

(setq use-short-answers t)

;; remap modifier key on macOS
(when (eq window-system 'mac)
  (setq mac-command-modifier nil
        mac-option-modifier 'meta
        mac-control-modifier 'control)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; ensure access to git on Windows, plus other tweaks
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

      bookmark-default-file (user-emacs-file "bookmarks")
      bookmark-save-flag 1

      enable-recursive-minibuffers t
      disabled-command-function 'nil
      epa-pinentry-mode 'loopback
      x-underline-at-descent-line t

      scroll-conservatively 8
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1)

      show-paren-delay 0.1

      backup-directory-alist `(("." . ,(concat user-emacs-data "/backups")))
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
  "When a region is active, `kill-region'; otherwise, `backword-kill-word'"
  (interactive "*p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(bind-keys ("C-w" . unix-werase-or-kill))

(defun maybe-kill-this-buffer ()
  "`kill-this-buffer' when called without a prefix arg; otherwise, `kill-buffer'"
  (interactive)
  (if current-prefix-arg
      (call-interactively 'kill-buffer)
    (kill-this-buffer)))
(bind-keys ("C-x k" . maybe-kill-this-buffer))

(defun which-func-insert-at-point ()
  (interactive)
  (insert (which-function)))
(bind-keys ("C-c y" . which-func-insert-at-point))

;; these are annoying
(bind-keys ("<mouse-2>" . nil)
           ("<down-mouse-2>" . nil))

(global-font-lock-mode t)               ; syntax highlighting
(show-paren-mode t)                     ; show matching paren

(line-number-mode t)                    ; line number in mode line
(column-number-mode t)                  ; column number in mode line

(defun ring-bell-function-minimal ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function #'ring-bell-function-minimal)


;;;
;;; Theme
;;;
(require 'conf-theme)


;;;
;;; Evil
;;;
(require 'conf-evil)


;;;
;;; Editing
;;;

(defun jae--setup-prog-mode ()
  (setq-local show-trailing-whitespace t)
  (toggle-truncate-lines 1))
(add-hook 'prog-mode-hook #'jae--setup-prog-mode)

(defun jae--large-file-hook ()
  "Turn off expensive functions (font-lock, undo-mode) for large files"
  (when (> (buffer-size) (* 1024 1024))
    (setq-local buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook #'jae--large-file-hook)

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

(use-package company
  :init (global-company-mode)
  :diminish company-mode
  :custom
  (company-idle-delay nil)               ; only complete when asked (C-M-i, usually)
  (company-minimum-prefix-length 0)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  (company-backends '(company-capf company-dabbrev))
  :bind (([remap completion-at-point] . company-complete)
         ([remap complete-symbol] . company-complete)
         :map company-active-map
         ("C-w" . nil)
         ("M-." . company-show-location)))

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
  (defun jae--counsel-grep-use-swiper-p ()
    (or (not (file-exists-p (buffer-file-name)))
        (counsel-grep-use-swiper-p-default)))
  :custom
  (counsel-grep-use-swiper-p #'jae--counsel-grep-use-swiper-p)
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

(use-package company-prescient
  :after (company prescient)
  :init (company-prescient-mode 1))

(use-package avy
  :bind (("M-g h"   . avy-goto-char-2)
         ("M-g c"   . avy-goto-char)
         ("M-g g"   . avy-goto-line)
         ("M-g M-g" . avy-goto-line)))

(use-package ace-window
  :custom (aw-scope 'frame)
  :bind (([remap other-window] . ace-window)
         ("C-c o" . ace-window)
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

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

(use-package beginend
  :diminish (beginend-global-mode beginend-prog-mode)
  :config (beginend-global-mode t))


;;;
;;; Shell
;;;

(use-package eshell :ensure nil
  :preface
  (defun jae--setup-eshell ()
    (setenv "TERM" "emacs"))
  :hook (eshell-mode . jae--setup-eshell)
  :custom
  (eshell-destroy-buffer-when-process-dies t))

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
                                          "Cc")))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :custom (diff-hl-draw-borders nil)
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))


;;;
;;; IDE
;;;

(use-package editorconfig)

(use-package eglot :ensure nil
  :hook
  ((c-mode c++-mode python-mode go-mode) . eglot-ensure)
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
    (-when-let (match (locate-dominating-file dir "compile_commands.json"))
      (cons 'compdb (expand-file-name match))))
  (cl-defmethod project-root ((project (head compdb)))
    (cdr project))
  :config
  (add-to-list 'project-find-functions #'project-try-compdb))

(use-package flymake :ensure nil
  :custom
  (flymake-proc-allowed-file-name-masks nil)
  :bind (("C-c w" . flymake-show-buffer-diagnostics)))

(use-package transient)


;;;
;;; Tools
;;;

;; make sure PATH matches our shell path
(use-package exec-path-from-shell
  :when (not (eq system-type 'windows-nt))
  :init
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-shell-name "zsh")
  (exec-path-from-shell-arguments '("-l" "-i")))

(use-package tramp :ensure nil
  :custom
  (tramp-verbose 2)
  (tramp-default-method "ssh")
  (tramp-chunksize 500)
  :config
  (add-to-list 'tramp-default-proxies-alist
               '("\\.jaalam\\.net\\'" "\\`root\\'" "/ssh:admin@%h:")))

(use-package man :ensure nil
  :custom
  (Man-header-file-path (list (expand-file-name "~/.local/include")
                              "/usr/include"
                              "/usr/local/include"
                              "/usr/include/x86_64-linux-gnu")))

(use-package ffap :ensure nil
  :custom
  ;; prevents Emacs from doing anything too fancy when C-x f
  ;; happens to point at a file when invoked
  (ffap-machine-p-unknown 'reject)
  (ffap-machine-p-local 'reject)
  (ffap-machine-p-known 'reject))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package restclient
  :commands restclient-mode)

(use-package jq-mode :defer t)


;;;
;;; Languages
;;;

(use-package cc-mode :ensure nil
  :custom
  (c-basic-offset 4)
  :config
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1) c-basic-offset)))
  (c-set-offset 'arglist-cont-nonempty
                '(c-lineup-gcc-asm-reg c-lineup-arglist-tabs-only)))

(use-package kotlin-mode
  :mode ("\\.kt" . kotlin-mode))

(use-package go-mode
  :mode ("\\.go" . go-mode)
  :config
  (add-hook
   'go-mode-hook (lambda ()
                   (add-hook 'before-save-hook 'gofmt-before-save nil 'local))))

(use-package rust-mode
  :custom
  (rust-format-on-save t))

(use-package python :ensure nil
  :commands python-mode
  :custom (python-shell-interpreter "python3"))

(use-package ruby-mode :ensure nil
  :custom (ruby-indent-level 4))

(use-package haskell-mode
  :mode (("\\.hs" . haskell-mode))
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions
                        #'haskell-doc-current-info nil t))))

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

(use-package cmake-mode
  :mode (("CMakeLists\\.txt" . cmake-mode)))

(use-package json-mode
  :mode (("\\.json" . json-mode)))

(use-package systemd
  :mode (("\\.service" . systemd-mode)
         ("\\.path" . systemd-mode)))

(use-package dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode)))

(use-package yaml-mode
  :mode (("\\.yaml" . yaml-mode)
         ("\\.yml" . yaml-mode)))

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
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)))

(use-package tex :ensure auctex)


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

(provide 'init)
