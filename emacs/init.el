;;; init.el --- custom emacs init file
(require 'package)

(defvar user-emacs-data "~/.local/share/emacs"
  "${XDG_CONFIG_HOME:-~/.local/share}/emacs")
(when (require 'xdg nil 'noerror)
  (setq user-emacs-data (concat (xdg-data-home) "/emacs")))

(setq custom-file (concat user-emacs-data "/custom.el")
      package-user-dir (format (concat user-emacs-data "/elpa-%s/")
                               emacs-major-version)

      ;; set package priorities: melpa-stable > elpa > melpa-nightly
      package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 1)
                                   ("elpa" . 2)
                                   ("melpa" . 0))
      package-enable-at-startup nil
      use-package-enable-imenu-support t
      use-package-always-ensure t)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "conf"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(diminish 'auto-revert-mode)
(diminish 'global-auto-revert-mode)
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)
(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode)

(require 'conf-editor)
(require 'conf-shell)


;;;
;;; Sensible defaults
;;;

(use-package uniquify :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(save-place-mode t)
(setq save-place-file (concat user-emacs-data "/places"))

;; make sure PATH matches our shell path on macOS
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-shell-name "bash")
  (exec-path-from-shell-arguments '("-l" "-i"))
  :init
  (exec-path-from-shell-initialize))

(defalias 'yes-or-no-p 'y-or-n-p)

;; remap modifier key on macOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-right-command-modifier 'meta))

(setq-default truncate-lines t
	      indent-tabs-mode t
	      c-file-style "linux"
	      indent-tabs-mode nil
	      tab-width 4
	      c-basic-offset 4
	      fill-column 120)

(setq auth-sources `((:source ,(concat user-emacs-data "/authinfo.gpg")))
      load-prefer-newer t
      frame-title-format "[Emacs] %b"
      auto-hscroll-mode 'current-line

      bookmark-default-file (concat user-emacs-data "/bookmarks")
      bookmark-save-flag 1

      enable-recursive-minibuffers nil
      disabled-command-function 'nil
      epa-pinentry-mode 'loopback
      x-underline-at-descent-line t
      tramp-default-method "ssh"
      tramp-chunksize 500

      scroll-conservatively 8
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1)

      eldoc-idle-delay 1.0         ; show eldoc when idle
      show-paren-delay 0           ; show parens when idle

      backup-directory-alist `(("." . ,(concat user-emacs-data "/backups")))
      auto-save-default nil
      version-control t
      delete-old-versions t
      backup-by-copying t
      create-lockfiles nil

      ediff-window-setup-function 'ediff-setup-windows-plain
      vc-follow-symlinks nil

      mouse-yank-at-point t
      save-interprogram-paste-before-kill t
      select-enable-clibpoard t
      select-enable-primary t

      xref-prompt-for-identifier () ; don't prompt on cross-references
      help-window-select t          ; shift focus to help window on C-h
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
  "`kill-this-buffer' when called without a prefix arg; otherwise, `kill-buffer'"
  (interactive)
  (if current-prefix-arg
      (call-interactively 'kill-buffer)
    (kill-this-buffer)))

(bind-keys ("C-x k" . maybe-kill-this-buffer)
           ("C-w" . unix-werase-or-kill)
           ([remap dabbrev-expand] . hippie-expand)
	       ("<mouse-2>" . nil)
	       ("<mouse-3>" . nil))

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
;;; Color themes
;;;

;; TODO: Integrate ideas from the listed themes to produce a cleaner,
;; lower-contrast Solarized variant
;;
;; - https://github.com/mswift42/soft-charcoal-theme/
;; - https://github.com/mswift42/warm-night-theme
(use-package solarized-theme :pin melpa
  :custom
  (solarized-high-contrast-mode-line t)
  (solarized-use-more-italic nil)
  (solarized-distinct-fringe-background nil)

  (solarized-use-variable-pitch nil)
  (solarized-scale-org-headlines nil)

  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)

  :config
  (load-theme 'solarized-dark t)
  ;; TODO: clean this up with the new upstream macro
  (solarized-with-color-variables
    'dark
    (custom-theme-set-faces
     'solarized-dark
     ;; font-lock: minimize color accents in source code
     `(font-lock-type-face ((,class (:foreground ,base0 :underline t))))
     `(font-lock-variable-name-face ((,class (:foreground ,blue))))
     `(font-lock-function-name-face ((,class (:weight bold))))

     ;; rtags: more subtle skiped-line face
     `(rtags-skippedline ((,class (:inherit font-lock-comment-face))))

     ;; lsp-mode highlights
     `(lsp-face-highlight-textual ((,class (:underline ,green-lc))))

     ;; smerge-mode: don't make my eyes bleed
     `(smerge-base ((,class (:inherit magit-diff-base-highlight))))
     `(smerge-lower ((,class (:inherit magit-diff-their-highlight))))
     `(smerge-upper ((,class (:inherit magit-diff-our-highlight))))
     `(smerge-markers ((,class (:inherit magit-diff-conflict-heading))))

     ;; mu4e
     `(mu4e-header-marks-face ((,class (:underline nil :foreground ,yellow))))
     `(mu4e-title-face ((,class (:inherit nil :foreground ,green))))

     `(mu4e-replied-face ((,class (:foreground ,blue :inherit nil))))
     `(mu4e-ok-face ((,class (:foreground ,green))))
     `(mu4e-view-attach-number-face ((,class (:inherit nil :foreground ,orange))))
     `(mu4e-highlight-face ((,class (:inherit highlight))))
     `(mu4e-title-face ((,class (:inherit nil :foreground ,green))))
     `(mu4e-modeline-face ((,class (:inherit nil :weight ,s-maybe-bold))))

     ;; info: don't scale faces
     `(info-menu-header ((,class (:inherit s-variable-pitch :weight ,s-maybe-bold))))
     `(Info-quoted ((,class (:inherit font-lock-constant-face))))

     ;; markdown: don't scale code blocks
     `(markdown-code-face ((,class (:inherit org-block))))

     ;; erc: minimize color accents
     `(erc-nick-default-face ((,class (:foreground ,base0 :weight bold))))
     `(erc-notice-face ((,class (:foreground ,base01))))
     `(erc-timestamp-face ((,class (:foreground ,base01))))
     `(erc-action-face ((,class (:foreground ,base0 :underline t))))
     `(erc-my-nick-face ((,class (:foreground ,base00 :weight bold))))
     `(erc-input-face ((,class (:foreground ,base0))))

     ;; org: clarity
     `(org-block ((,class (:background ,base03 :foreground ,base00))))
     `(org-block-begin-line ((,class (:inherit font-lock-comment-face :underline t))))
     `(org-block-end-line ((,class (:inherit font-lock-comment-face :overline t)))))))


;;;
;;; General Plugins
;;;

;; TRAMP
(use-package tramp :ensure nil
  :custom
  (tramp-verbose 2)
  :config
  (add-to-list 'tramp-default-proxies-alist
               '("\\.jaalam\\.net\\'" "\\`root\\'" "/ssh:admin@%h:"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Better Key Discovery
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

;; Better Beginning-Of-Buffer
(use-package beginend
  :diminish beginend-global-mode
  :commands beginend-global-mode
  :init (beginend-global-mode t))

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
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-c j" . counsel-imenu)
         ("C-x r b" . counsel-bookmark)))

(use-package smex           ; better M-x sort
  :after ivy
  :custom (smex-save-file (concat user-emacs-data "/smex-items")))

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
         ("/ '" . ibuffer-set-filter-groups-dynamic)
         ("M-o" . nil)))

;; Buffer Navigation
(use-package avy
  :bind (("M-g h" . avy-goto-char-2)
         ("M-g c" . avy-goto-char)
         ("M-g g" . avy-goto-line)
         ("M-g M-g" . avy-goto-line)))

(use-package swiper
  :after (:any ivy counsel)
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

;; Newsgroups + (Maybe) Mail
(use-package gnus :ensure nil
  :commands gnus
  :custom
  (gnus-select-method '(nntp "news.gmane.org"))
  (gnus-startup-file (concat user-emacs-data "/newsrc"))
  (gnus-always-read-dribble-file t)
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil))

;; Version Control
(use-package magit
  :diminish magit-file-mode
  :init (global-magit-file-mode t)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-diff-paint-whitespace t)
  (magit-repository-directories '("~/Upstream" "~/Repos"))
  :bind (("C-c g" . magit-file-popup)))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :custom
  (diff-hl-draw-borders nil)
  :config
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Projects
(use-package projectile :pin melpa
  :init (projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-use-git-grep t)
  (projectile-find-dir-includes-top-level t)
  (projectile-mode-line nil))

(use-package counsel-projectile
  :after projectile
  :init (counsel-projectile-on))

;; sexp editing everywhere
(use-package smartparens
  :init (smartparens-global-mode t)
  :custom
  (sp-base-key-bindings 'sp)
  (sp-highlight-wrap-overlay nil)
  (sp-show-pair-delay 0)
  :config
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  :bind (:map smartparens-mode-map ("C-]" . nil)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-(" . sp-forward-barf-sexp)))

;; expand regions by semantic regions
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Snippets
(use-package yasnippet
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
  :load-path "~/Upstream/lsp-mode/"
  :config
  (setq max-specpdl-size 32000) ;; HACK - fix bug in LSP
  :custom
  (lsp-enable-codeaction nil)
  (lsp-enable-completion-at-point t)
  (lsp-enable-eldoc t)
  (lsp-enable-flycheck t)
  (lsp-enable-indentation nil)
  (lsp-enable-indentation t)
  (lsp-highlight-symbol-at-point t))

(use-package company-lsp :ensure t :defer t :pin melpa
  :commands company-lsp
  :init
  (defun jae--setup-company-lsp ()
    (yas-minor-mode 1)
    (setq company-backends '(company-lsp company-dabbrev)))
  (add-hook 'lsp-mode #'jae--setup-company-lsp)
  :custom
  (company-lsp-enable-snippet t))

(use-package cquery :ensure nil
  :load-path "~/Upstream/cquery/emacs/"
  :commands lsp-cquery-enable
  :init
  (add-hook 'c-mode-hook #'lsp-cquery-enable)
  (add-hook 'c++-mode-hook #'lsp-cquery-enable)
  :custom
  (cquery-executable "~/Upstream/cquery/build/app")
  (cquery-resource_dir "~/Upstream/cquery/clang_resource_dir")
  (cquery-enable-sem-highlight nil))

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
                   (local company-backends '(company-go company-capf)))))

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

(use-package pdf-tools :ensure t
  :init
  (pdf-tools-install)
  (add-hook 'pdf-tools-enabled-hook
            (lambda ()
              (blink-cursor-mode -1)))
  :config
  (setq-default pdf-view-display-size 'fit-width
                pdf-view-use-imagemagick t
                pdf-view-use-scaling t))

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
  (python-shell-interpreter "python3"))

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
