;;; init.el --- custom emacs init file
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "conf"))
(require 'bootstrap)

(eval-when-compile
  (require 'use-package))
(use-package diminish)
(use-package bind-key)

(use-package abbrev :ensure nil
  :diminish abbrev-mode)

(use-package eldoc :ensure nil
  :diminish eldoc-mode
  :custom (eldoc-idle-delay 1.0))

;;;
;;; Color themes
;;;

(use-package solarized-theme :pin melpa
  :custom
  (solarized-high-contrast-mode-line t)
  (solarized-use-more-italic nil)
  (solarized-distinct-fringe-background nil)

  (solarized-use-variable-pitch nil)
  (solarized-scale-org-headlines nil)

  ;(solarized-height-minus-1 1.0)
  ;(solarized-height-plus-1 1.0)
  ;(solarized-height-plus-2 1.0)
  ;(solarized-height-plus-3 1.0)
  ;(solarized-height-plus-4 1.0)

  :init
  (defvar jae--dark-theme t
    "If non-nil, init.el will load dark theme.")

  (defun jae--solarized-theme (&optional dark)
    "Loads a tweaked dark or light variant of Solarized."
    (let* ((name (or (and dark 'solarized-dark) 'solarized-light))
           (variant (or (and dark 'dark) 'light)))
      (load-theme name t)
      (solarized-with-color-variables variant
        (custom-theme-set-faces
         name
         ;; font-lock: minimize color accents in source code
         `(font-lock-type-face ((,class (:foreground ,base0 :underline t))))
         `(font-lock-variable-name-face ((,class (:foreground ,blue))))
         `(font-lock-function-name-face ((,class (:weight bold))))

         ;; lsp-mode highlights
         `(lsp-face-highlight-textual ((,class (:underline ,green-lc))))

         ;; smerge-mode: don't make my eyes bleed
         `(smerge-base ((,class (:inherit magit-diff-base-highlight))))
         `(smerge-lower ((,class (:inherit magit-diff-their-highlight))))
         `(smerge-upper ((,class (:inherit magit-diff-our-highlight))))
         `(smerge-markers ((,class (:inherit magit-diff-conflict-heading))))

         ;; info: don't scale faces
         `(info-menu-header ((,class (:inherit s-variable-pitch :weight ,s-maybe-bold))))
         `(Info-quoted ((,class (:inherit font-lock-constant-face))))

         ;; markdown: don't scale code blocks
         `(markdown-code-face ((,class (:inherit org-block))))

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
  (jae--solarized-theme jae--dark-theme)

  (defun invert-theme ()
    (interactive)
    (setq jae--dark-theme (not jae--dark-theme))
    (jae--solarized-theme jae--dark-theme))
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
  :diminish whitespace-mode)

(use-package saveplace :ensure nil
  :init (save-place-mode 1)
  :custom
  (save-place-file (user-emacs-file "places")))

;; make sure PATH matches our shell path on macOS
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-shell-name "bash")
  (exec-path-from-shell-arguments '("-l" "-i")))

(defalias 'yes-or-no-p 'y-or-n-p)

;; remap modifier key on macOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-right-command-modifier 'meta))

(setq-default truncate-lines t
        indent-tabs-mode nil
		tab-width 4
        fill-column 120)

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

      xref-prompt-for-identifier ()     ; don't prompt on cross-references
      help-window-select t              ; shift focus to help window on C-h
      inhibit-startup-screen t
      load-prefer-newer t)

;; via EmacsWiki: KillingAndYanking
(defun unix-werase-or-kill (arg)
  "When a region is active, `kill-region'; otherwise, `backword-kill-word'"
  (interactive "*p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun maybe-kill-this-buffer ()
  "`kill-this-buffer' when called without a prefix 2arg; otherwise, `kill-buffer'"
  (interactive)
  (if current-prefix-arg
      (call-interactively 'kill-buffer)
    (kill-this-buffer)))

(bind-keys ("C-x k" . maybe-kill-this-buffer)
           ("C-w" . unix-werase-or-kill)
           ("<mouse-2>" . nil)
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

;; TRAMP
(use-package tramp :ensure nil
  :custom
  (tramp-verbose 2)
  (tramp-default-method "ssh")
  (tramp-chunksize 500)
  :config
  (add-to-list 'tramp-default-proxies-alist
               '("\\.jaalam\\.net\\'" "\\`root\\'" "/ssh:admin@%h:"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package epg :ensure nil
  :custom
  (epg-gpg-program "gpg2")
  (epa-pinentry-mode 'loopback))

;; Better Key Discovery
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

;; Better Beginning-Of-Buffer
(use-package beginend
  :diminish (beginend-global-mode beginend-prog-mode)
  :config (beginend-global-mode t))

;; Minibuffer Matching
(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode t)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-re-builders-alist '((counsel-descbinds . ivy--regex)
                           (t . ivy--regex-plus)))
  :bind (("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c r" . ivy-resume)))

(use-package counsel
  :after ivy
  :custom
  (counsel-find-file-at-point t)
  :bind (("M-x" . counsel-M-x)
         ("C-M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-git)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-c j" . counsel-imenu)
         ("C-x r b" . counsel-bookmark)))

(use-package smex           ; better M-x sort
  :after ivy
  :custom (smex-save-file (user-emacs-file "smex-items")))

(use-package ivy-xref :disabled
  :after (ivy xref)
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Window Manipulation
(use-package ace-window
  :custom (aw-scope 'frame)
  :bind (([remap other-window] . ace-window)
         ("C-c o" . ace-window)
         ("M-o" . ace-window)))

(use-package transpose-frame
  :commands transpose-frame
  :bind ("C-x 7" . transpose-frame))

;; Window Management
(use-package ibuffer :ensure nil
  :commands ibuffer
  :custom
  (ibuffer-expert t) ; don't prompt for confirmation on delete
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-title-face 'font-lock-type-face)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("M-o" . nil)))

;; Buffer Navigation
(use-package avy
  :bind (("M-g h" . avy-goto-char-2)
         ("M-g c" . avy-goto-char)
         ("M-g g" . avy-goto-line)
         ("M-g M-g" . avy-goto-line)))

(use-package swiper
  :after (counsel)
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-M-s" . search-forward)))

;; Misc Utilities
(use-package macrostep
  :commands macrostep-mode
  :bind ("C-c e" . macrostep-mode))

(use-package rainbow-mode
  :commands rainbow-mode)

;; Linting
(use-package flycheck
  :commands (flycheck-mode flycheck-select-checker)
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save newline))
  (flycheck-display-errors-function nil)
  (flycheck-help-echo-function nil))

(use-package magit :pin melpa
  :diminish magit-file-mode
  :init (global-magit-file-mode t)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-diff-paint-whitespace t)
  (magit-repository-directories '("~/Upstream" "~/Repos"))
  :bind (("C-c g" . magit-file-popup)
         ("C-c h" . magit)))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :custom (diff-hl-draw-borders nil)
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package projectile :pin melpa
  :hook (after-init . projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching nil)
  (projectile-use-git-grep t)
  (projectile-find-dir-includes-top-level t)
  (projectile-mode-line nil))

(use-package counsel-projectile
  :after projectile
  :init (counsel-projectile-mode))

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

;; expand regions by semantic regions
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Snippets
(use-package yasnippet
  :commands yas-minor-mode
  :diminish yas-minor-mode)

;; Rest APIs
(use-package restclient
  :commands restclient-mode)

;; BibTex
(use-package ivy-bibtex :pin melpa
  :commands ivy-bibtex
  :custom
  (bibtex-completion-bibliography '("~/Dropbox/library.bib"))
  (bibtex-completion-library-path '("~/Dropbox/Library")))


;:;
;;; LANGUAGES
;;;

(use-package lsp-mode :ensure nil
  ;; :load-path "~/Upstream/lsp-mode/"
  :custom
  (lsp-enable-codeaction t)
  (lsp-enable-completion-at-point t)
  (lsp-enable-eldoc t)
  (lsp-enable-indentation nil)
  (lsp-enable-indentation t)
  (lsp-highlight-symbol-at-point t)
  (lsp-project-blacklist '("~/Upstream/emacs/")))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t))

(use-package company-lsp :pin melpa
  :commands company-lsp
  :preface
  (defun jae--setup-company-lsp ()
    (yas-minor-mode 1)
    (setq-local company-backends '(company-lsp company-dabbrev)))
  :hook
  (lsp-mode . jae--setup-company-lsp)
  :custom
  (company-lsp-enable-snippet t))

(use-package cc-mode :ensure nil
  :init
  (defun jae--setup-c-mode ()
	(setq-local c-file-style "linux")
	(setq-local c-basic-offset 8))
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

(use-package cquery
  ;; :load-path "~/Upstream/emacs-cquery/"
  :after lsp-mode
  :preface
  (defun jae--setup-cquery ()
    ;; todo: make this check actually work...
    (unless magit-buffer-revision
      (lsp-cquery-enable)))
  :commands lsp-cquery-enable
  :hook
  ((c-mode c++-mode) . jae--setup-cquery)
  :custom
  (cquery-project-root-matchers '("compile_commands.json"))
  (cquery-executable "~/Upstream/cquery/build/cquery")
  (cquery-resource-dir "~/Upstream/cquery/clang_resource_dir")
  (cquery-cache-dir "~/.cache/cquery/")
  (cquery-sem-highlight-method nil))

(use-package disaster
  :commands disaster
  :custom
  (disaster-objdump "objdump -d -M att -Sl -h --no-show-raw-insn")
  (disaster-make-flags "-k")
  :bind (:map c-mode-map ("C-c t" . disaster)
	     :map c++-mode-map ("C-c t" . disaster)))

;; C#
(use-package csharp-mode
  :mode ("\\.cs" . csharp-mode))

;; Go
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


;; Rust
(use-package rust-mode
  :custom
  (rust-format-on-save t))
(use-package racer
    :after rust-mode
    :config (add-hook 'rust-mode-hook #'racer-mode))

;; LaTeX and PDF
(use-package tex :ensure auctex)

(use-package pdf-tools
  :init
  (pdf-tools-install)
  (add-hook 'pdf-tools-enabled-hook
            (lambda ()
              (blink-cursor-mode -1)))
  :config
  (setq-default pdf-view-display-size 'fit-width
                pdf-view-use-imagemagick t
                pdf-view-use-scaling t))

(use-package x86-lookup
  :custom
  (x86-lookup-browse-pdf-function #'x86-lookup-browse-pdf-pdf-tools)
  (x86-lookup-pdf "~/Documents/x86_architecture.pdf")
  :bind ("C-h x" . x86-lookup))

;; Org
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

;; Python
(use-package python :ensure nil
  :commands python-mode
  :custom
  (python-shell-interpreter (user-emacs-file "venv/bin/python")))

(use-package lsp-python :disabled
  :commands lsp-python-enable
  :init
  (add-hook 'python-mode-hook #'lsp-python-enable))

(use-package elpy :pin melpa
  :after python
  :init
  (add-hook 'elpy-mode-hook
            (lambda ()
              (setq-local company-backends '(elpy-company-backend))))
  :config
  (elpy-enable)
  :custom
  (elpy-rpc-python-command python-shell-interpreter)
  (elpy-modules '(elpy-module-sane-defaults
                  elpy-module-eldoc
                  elpy-module-pyvenv
                  elpy-module-yasnippet)))

;; Haskell
(use-package haskell-mode
  :mode (("\\.hs" . haskell-mode))
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local eldoc-documentation-function
                          'haskell-doc-current-info))))

;; Markdown
(use-package markdown-mode :ensure t :pin melpa
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  :custom
  (markdown-asymmetric-header t))

;; HTML/CSS/JS
(use-package js2-mode
  :commands js2-mode)
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.[tj]sx?\\'" . web-mode))
  :custom
  (web-mode-enable-current-element-highlight t))

;; Misc
(use-package systemd
  :commands systemd-mode)
(use-package json-mode
  :mode (("\\.json" . json-mode)))
(use-package yaml-mode
  :mode (("\\.yaml" . yaml-mode)
         ("\\.yml" . yaml-mode)))

(provide 'init)
