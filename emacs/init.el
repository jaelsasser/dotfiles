;;; init.el --- custom emacs init file
(add-to-list 'load-path (concat user-emacs-directory "conf"))
(require 'bootstrap)

(eval-when-compile
  (require 'use-package))
(use-package diminish :pin melpa)
(use-package bind-key :pin melpa)
(use-package use-package :pin melpa)

(use-package abbrev :ensure nil
  :diminish abbrev-mode)

(use-package eldoc :ensure nil
  :diminish eldoc-mode
  :custom (eldoc-idle-delay 1.0))


;;;
;;; Color themes
;;;

(use-package solarized-theme :pin melpa :defer nil
  :custom
  (solarized-distinct-fringe-background nil)
  (solarized-high-contrast-mode-line nil)
  (solarized-scale-org-headlines t)
  (solarized-use-variable-pitch nil)
  (solarized-use-more-italic nil)
  :config
  (setq jae--current-theme 'jae-solarized-light)
  (deftheme jae--solarized-light)
  (deftheme jae--solarized-dark)
  (eval-when-compile
      (require 'solarized-palettes))
  (require 'solarized-theme)
  (let* ((jae--solarized-faces
          '("Customized solarized faces."
            (custom-theme-set-faces
             theme-name
             ;; font-lock: minimize color accents in source code
             `(font-lock-type-face ((,class (:foreground ,base0 :underline t))))
             `(font-lock-variable-name-face ((,class (:foreground ,blue))))
             `(font-lock-function-name-face ((,class (:foreground ,base0 :weight bold))))

             ;; softer lsp-mode highlights
             `(lsp-face-highlight-textual ((,class (:inherit highlight))))
             `(lsp-face-highlight-read ((,class (:inherit highlight))))
             `(lsp-face-highlight-write ((,class (:inherit highlight))))

             ;; info: don't scale faces
             `(info-menu-header ((,class (:inherit s-variable-pitch :weight ,s-maybe-bold))))
             `(Info-quoted ((,class (:inherit font-lock-constant-face))))

             ;; markdown: don't scale code blocks
             `(markdown-code-face ((,class (:inherit org-block))))

             ;; markdown: scale headings
             `(markdown-header-face-1 ((,class (:inherit markdown-header-face
                                                         ,@(when solarized-scale-org-headlines
                                                             (list :height solarized-height-plus-4))))))
             `(markdown-header-face-2 ((,class (:inherit markdown-header-face
                                                         ,@(when solarized-scale-org-headlines
                                                             (list :height solarized-height-plus-3))))))
             `(markdown-header-face-3 ((,class (:inherit markdown-header-face
                                                         ,@(when solarized-scale-org-headlines
                                                             (list :height solarized-height-plus-2))))))
             `(markdown-header-face-4 ((,class (:inherit markdown-header-face
                                                         ,@(when solarized-scale-org-headlines
                                                             (list :height solarized-height-plus-1))))))
             `(markdown-header-face-5 ((,class (:inherit markdown-header-face))))
             `(markdown-header-face-6 ((,class (:inherit markdown-header-face))))

             ;; erc: minimal color accents
             `(erc-nick-default-face ((,class (:foreground ,base0 :weight bold))))
             `(erc-notice-face ((,class (:foreground ,base01))))
             `(erc-timestamp-face ((,class (:foreground ,base01))))
             `(erc-action-face ((,class (:foreground ,base0 :underline t))))
             `(erc-my-nick-face ((,class (:foreground ,base00 :weight bold))))
             `(erc-input-face ((,class (:foreground ,base0))))

             ;; org: clarity
             `(org-block ((,class (:background ,base03 :foreground ,base00))))
             `(org-block-begin-line ((,class (:inherit font-lock-comment-face :underline t))))
             `(org-block-end-line ((,class (:inherit font-lock-comment-face :overline t))))))))
    (solarized-with-color-variables
      'light 'jae--solarized-light solarized-light-color-palette-alist jae--solarized-faces)
    (solarized-with-color-variables
      'dark 'jae--solarized-dark solarized-dark-color-palette-alist jae--solarized-faces))
  (defun invert-theme ()
    (interactive)
    (setq jae--current-theme (if (eq jae--current-theme 'jae--solarized-dark)
                                'jae--solarized-light
                              'jae--solarized-dark))
    (let* ((custom--inhibit-theme-enable nil))
      (enable-theme jae--current-theme)))
  (invert-theme)
  :bind (("C-c t" . invert-theme)))

;;;
;;; Evil-Mode, Company
;;;
(require 'conf-editor)

;;;
;;; Eshell and Friends
;;;
(require 'conf-shell)

;;;
;;; Gnus, SMTP, and Mail
;;;
(require 'conf-mail)


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

(defalias 'yes-or-no-p 'y-or-n-p)

;; remap modifier key on macOS
(use-package mac :ensure nil
  :when (eq (window-system) 'mac)
  :custom
  (mac-command-modifier nil)
  (mac-option-modifier 'meta)
  (mac-control-modified 'ctrl)
  :config
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

      enable-recursive-minibuffers nil
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
      select-enable-clibpoard t
      select-enable-primary t

      split-height-threshold 120
      split-width-threshold 160

      xref-prompt-for-identifier ()     ; don't prompt on cross-references
      help-window-select t              ; shift focus to help window on C-h
      inhibit-startup-screen t
      load-prefer-newer t)

(setq safe-local-variable-values
      '((glyphless-char-display . hex-code)
        (eval c-set-offset 'arglist-cont-nonempty '(c-lineup-arglist-intro-after-paren))))

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
;;; General Plugins
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
  ;; TODO: fixme
  ;(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-default-proxies-alist
               '("\\.jaalam\\.net\\'" "\\`root\\'" "/ssh:admin@%h:")))

(use-package epg :ensure nil
  :custom
  (epg-gpg-program "gpg2")
  (epa-pinentry-mode nil))

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

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

(use-package beginend
  :diminish (beginend-global-mode beginend-prog-mode)
  :config (beginend-global-mode t))

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode t)
  :custom
  (ivy-re-builders-alist '((counsel-descbinds . ivy--regex)
                           (t . ivy--regex-plus)))
  (ivy-use-virtual-buffers t)
  :bind (("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c r" . ivy-resume)))
(use-package counsel
  :after ivy
  :custom
  (counsel-grep-use-swiper-p #'jae--counsel-grep-use-swiper-p)
  (counsel-find-file-at-point t)
  :config
  (if (boundp 'counsel--git-grep-count-threshold)
      (setq counsel--git-grep-count-threshold 200))
  (defun jae--counsel-grep-use-swiper-p ()
    (or (not (file-exists-p (buffer-file-name))) (counsel-grep-use-swiper-p-default)))
  :bind (("M-x" . counsel-M-x)
         ("C-M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-git)
         ("C-c s" . counsel-git-grep)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-c j" . counsel-imenu)
         ("C-x r b" . counsel-bookmark)))
(use-package smex
  :after ivy
  :custom (smex-save-file (user-emacs-file "smex-items")))

(use-package ivy-xref :disabled
  :after (ivy xref)
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

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

(use-package avy
  :bind (("M-g h" . avy-goto-char-2)
         ("M-g c" . avy-goto-char)
         ("M-g g" . avy-goto-line)
         ("M-g M-g" . avy-goto-line)))

(use-package swiper
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-M-s" . search-forward)))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package flymake :ensure nil
  :custom
  (flymake-proc-allowed-file-name-masks nil))

(use-package flyspell :ensure nil :disabled
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-auto-correct-binding nil)
  (flyspell-use-meta-tab nil)
  :hook ((prog-mode . flyspell-prog-mode)
         (markdown-mode . flyspell-mode))
  :bind (:map flyspell-mode-map ("C-M-," . flyspell-auto-correct-word)))

(use-package magit :pin melpa
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-diff-paint-whitespace t)
  (magit-repository-directories `(("~/Repos" . 1)
                                  ("~/Upstream" . 1)
                                  ("~/Upstream/llvm/tools/clang" . 0)
                                  ("~/Upstream/llvm/tools/clang/tools/extra" . 0)))
  :bind (("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)
         ("C-c M-g" . nil))
  :config
  (transient-append-suffix 'magit-log
    '("m" "Omit merge commits" "--no-merges")
    '("1" "First parent" "--first-parent")))
(use-package git-commit :pin melpa
  :custom
  (git-commit-known-pseudo-headers '("Signed-off-by"
                                     "Suggsted-by"
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

(use-package project :ensure nil
  :init
  (defun project-try-compdb (dir)
    (-when-let (match (locate-dominating-file dir "compile_commands.json"))
      (cons 'compdb (expand-file-name match))))
  (cl-defmethod project-roots ((project (head compdb)))
    (list (cdr project)))
  :config
  (add-to-list 'project-find-functions #'project-try-compdb))

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

(use-package restclient
  :commands restclient-mode)
(use-package jq-mode :defer t)

(use-package ivy-bibtex :pin melpa
  :commands ivy-bibtex
  :custom
  (bibtex-completion-bibliography '("~/Documents/Papers/library.bib"))
  (bibtex-completion-library-path '("~/Documents/Papers")))


;:;
;;; LANGUAGES
;;;

(use-package eglot :pin melpa
  :hook
  ((c-mode c++-mode python-mode) . eglot-ensure)
  :custom
  (eglot-put-doc-in-help-buffer t)
  (eglot-autoreconnect nil)
  :config
  (add-to-list 'eglot-server-programs '((swift-mode objc-mode) . ("xcrun" "sourcekit-lsp")))
  (defun jae--eglot-c-c++-server (&optional interactive)
    (list (or (executable-find "ccls")
              (executable-find "clangd")
              (executable-find "cquery"))))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . jae--eglot-c-c++-server)))

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

(use-package disaster
  :commands disaster
  :custom
  (disaster-objdump "objdump -d -M att -Sl -r")
  (disaster-make-flags "-k")
  :bind (:map c-mode-map ("C-c w" . disaster)
	     :map c++-mode-map ("C-c w" . disaster)))

(use-package jsonrpc :pin melpa)
(use-package flymake :pin melpa)

(use-package csharp-mode
  :mode ("\\.cs" . csharp-mode))

(use-package kotlin-mode :pin melpa
  :mode ("\\.kt" . kotlin-mode))

(use-package go-mode
  :mode ("\\.go" . go-mode)
  :config
  (add-hook
   'go-mode-hook (lambda ()
                   (add-hook 'before-save-hook 'gofmt-before-save nil 'local))))
(use-package company-go
  :after go-mode
  :config
  (add-hook
   'go-mode-hook (lambda ()
                   (setq-local company-backends '(company-go company-capf)))))
(use-package go-eldoc
  :after go-mode
  :config (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package rust-mode
  :custom
  (rust-format-on-save t))
(use-package racer :disabled
  :after rust-mode
  :config (add-hook 'rust-mode-hook #'racer-mode))

(use-package tex :ensure auctex)
(use-package pdf-tools :disabled
  :init
  (pdf-tools-install)
  (add-hook 'pdf-tools-enabled-hook
            (lambda ()
              (blink-cursor-mode -1)))
  :config
  (setq-default pdf-view-display-size 'fit-width
                pdf-view-use-imagemagick t
                pdf-view-use-scaling t))

(use-package x86-lookup :pin melpa
  :custom
  (x86-lookup-browse-pdf-function #'x86-lookup-browse-pdf-pdf-tools)
  (x86-lookup-pdf "~/Documents/x86_architecture.pdf")
  :bind ("C-h x" . x86-lookup))

(use-package org
  :init
  (use-package ob-ipython :ensure t :after org) ; inline ipython in SRC blocks
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
         ("C-c b" . org-iswitchb)))

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
              (setq-local eldoc-documentation-function
                          'haskell-doc-current-info))))

(use-package markdown-mode :ensure t :pin melpa
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  :custom
  (markdown-asymmetric-header t)
  (markdown-header-scaling t)
  :bind (:map markdown-mode-map ("C-c C-c l" . markdown-table-align)))

(use-package js2-mode
  :commands js2-mode)
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.[tj]sx?\\'" . web-mode))
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


(provide 'init)
