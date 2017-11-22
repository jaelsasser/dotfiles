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
      package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/"))
      package-archive-priorities '(("melpa-stable" . 10)
                                   ("elpa" . 5)
                                   ("melpa" . 0)
                                   ("org" . 11))
      package-enable-at-startup nil
      use-package-enable-imenu-support t)

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(save-place-mode t)
(setq save-place-file (concat user-emacs-data "/places"))

;; make sure PATH matches our shell path on macOS
(use-package exec-path-from-shell :ensure t
  :init (setq exec-path-from-shell-check-startup-files nil
              exec-path-from-shell-shell-name "bash"
              exec-path-from-shell-arguments '("-l" "-i"))
  :config (exec-path-from-shell-initialize))

(defalias 'yes-or-no-p 'y-or-n-p)

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
(setq visible-bell       nil
      ring-bell-function #'ring-bell-function-minimal)


;;;
;;; Color themes
;;;

;; TODO: Integrate ideas from the listed themes to produce a cleaner,
;; lower-contrast Solarized variant
;;
;; - https://github.com/mswift42/soft-charcoal-theme/
;; - https://github.com/mswift42/warm-night-theme

(use-package solarized-theme :ensure t :pin melpa
  :init
  (setq solarized-high-contrast-mode-line t
        solarized-use-more-italic nil
        solarized-distinct-fringe-background nil

        solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil

        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)

  :config
  (load-theme 'solarized-dark t)
  ;; extend the solarized theme to my liking
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

(use-package leuven-theme :ensure t :pin melpa :disabled
  :config
  (setq leuven-scale-outline-headlines nil
        leuven-scale-org-agenda-structure nil)
  (load-theme 'leuven t)

  (let ((class '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'leuven
     `(erc-notice-face
       ((,class (:inherit font-lock-comment-face :slant normal))))
     `(erc-timestamp-face
       ((,class (:inherit font-lock-comment-face :slant normal))))
     `(erc-action-face
       ((,class (:inherit erc-default-face :underline t))))
     `(erc-input-face
       ((,class (:inherit org-list-dt :weight normal))))
     `(erc-my-nick-face
       ((,class (:inherit org-list-dt))))
     `(erc-prompt-face
       ((,class (:inherit erc-my-nick-face)))))))


;;;
;;; General Plugins
;;;

;; TRAMP
(use-package tramp :ensure nil
  :config
  (setq tramp-verbose 2)
  (add-to-list 'tramp-default-proxies-alist
               '("\\.jaalam\\.net\\'" "\\`root\\'" "/ssh:admin@%h:"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Better Key Discovery
(use-package which-key :ensure t
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

;; Better Beginning-Of-Buffer
(use-package beginend :ensure t
  :diminish beginend-global-mode
  :commands beginend-global-mode
  :init (beginend-global-mode t))

;; Minibuffer Matching
(use-package ivy :ensure t
  :init (ivy-mode t)
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist
        '((counsel-descbinds . ivy--regex)
          (t . ivy--regex-plus)))
  :bind (("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c r" . ivy-resume)))
(use-package counsel :ensure t :defer t
  :config
  (setq counsel-find-file-at-point t)
  :bind (("M-x" . counsel-M-x)
         ("C-M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-c j" . counsel-imenu)
         ("C-x r b" . counsel-bookmark)))
(use-package flx :ensure t :defer t :disabled) ; better fuzzy-match sort
(use-package smex :ensure t :defer t           ; better M-x sort
  :config (setq smex-save-file (concat user-emacs-data "/smex-items")))

;; Window Manipulation
(use-package ace-window :ensure t
  :config
  (setq aw-scope 'frame)
  :bind (([remap other-window] . ace-window)
         ("C-c o" . ace-window)
         ("M-o" . ace-window)))
(use-package transpose-frame :ensure t :defer t
  :bind ("C-x 7" . transpose-frame))

;; Window Management
(use-package ibuffer
  :config
  (use-package ibuffer-tramp :ensure t :defer t)
  (require 'erc-ibuffer)

  (setq ibuffer-expert t     ; don't prompt for confirmation on delete
        ibuffer-show-empty-filter-groups nil
        ibuffer-title-face 'font-lock-type-face)

  (defun ibuffer-set-filter-groups-dynamic ()
    (interactive)
    (setq ibuffer-filter-groups
          (append
           (mapcar (lambda (project)
                     `(,project (projectil1e-files . ,project)))
                   (projectile-open-projects))
           (ibuffer-tramp-generate-filter-groups-by-tramp-connection)
           '(("emacs" (or (name . "^\\*scratch\\*$")
                          (name . "^\\*Messages\\*$")
                          (name . "^\\*Backtrace\\*$")))
             ("IRC: Snoonet" (erc-server . "snoonet"))
             ("IRC: Freenode" (erc-server . "freenode")))))
    (ibuffer-update nil t))

  (add-hook 'ibuffer-mode-hook #'ibuffer-set-filter-groups-dynamic)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("/ '" . ibuffer-set-filter-groups-dynamic)
         ("M-o" . nil)))

;; Buffer Navigation
(use-package avy :ensure t :defer t
  :bind (("M-g h" . avy-goto-char-2)
         ("M-g c" . avy-goto-char)
         ("M-g g" . avy-goto-line)
         ("M-g M-g" . avy-goto-line)))
(use-package swiper :ensure t :defer t
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-M-s" . search-forward)))

;; Misc Utilities
(use-package macrostep :ensure t :defer t
  :bind ("C-c e" . macrostep-mode))
(use-package rainbow-mode :ensure t :defer t)

;; Linting
(use-package flycheck :ensure t
  :commands (flycheck-mode flycheck-select-checker)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save newline)
	flycheck-display-errors-function nil
	flycheck-help-echo-function nil))

;; Newsgroups + (Maybe) Mail
(use-package gnus :ensure nil :defer t
  :config
  (setq gnus-select-method '(nntp "news.gmane.org")
        gnus-startup-file (concat user-emacs-data "/newsrc")
        gnus-always-read-dribble-file t
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil))

;; Version Control
(use-package magit :ensure t
  :init (global-magit-file-mode t)
  :diminish magit-file-mode
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-paint-whitespace t)
  :bind (("C-c g" . magit-file-popup)))
(use-package diff-hl :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq diff-hl-draw-borders nil))

;; Projects
(use-package projectile :ensure t :pin melpa
  :init (projectile-mode)
  :config
  (use-package counsel-projectile :ensure
    :init (counsel-projectile-on))
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-use-git-grep t
        projectile-find-dir-includes-top-level t
        projectile-mode-line nil))

;; sexp editing everywhere
(use-package smartparens :ensure t :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (smartparens-global-mode t)
  :config
  (setq sp-base-key-bindings 'smartpaens
        sp-highlight-wrap-overlay nil)
  (require 'smartparens-config)
  (setq sp-show-pair-delay 0)
  :bind (:map smartparens-mode-map ("C-]" . nil)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-(" . sp-forward-barf-sexp)))

;; expand regions by semantic regions
(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

;; Snippets
(use-package yasnippet :ensure t :defer t
  :diminish yas-minor-mode)

;; Rest APIs
(use-package restclient :ensure t
  :commands restclient-mode)

;; BibTex
(use-package ivy-bibtex :ensure t :pin melpa
  :commands ivy-bibtex
  :config
  (setq bibtex-completion-bibliography '("~/Dropbox/library.bib")
        bibtex-completion-library-path '("~/Dropbox/Library")))


;:;
;;; LANGUAGES
;;;

(use-package lsp-mode :ensure nil
  :load-path "~/Upstream/lsp-mode/"
  :init
  (setq max-specpdl-size 32000) ;; HACK - fix bug in LSP
  (setq lsp-enable-codeaction nil
        lsp-enable-completion-at-point t
        lsp-enable-eldoc t
        lsp-enable-flycheck t
        lsp-enable-indentation nil
        lsp-enable-indentation t
        lsp-highlight-symbol-at-point t))

(use-package lsp-cquery :ensure nil :load-path "~/Upstream/cquery/emacs/"
  :init
  (setq cquery/root-dir "~/Upstream/cquery/"
        cquery/enable-sem-highlight nil)
  (add-hook 'c-mode-hook #'lsp-cquery-enable)
  (add-hook 'c++-mode-hook #'lsp-cquery-enable))

(use-package disaster :ensure t
  :bind
  (:map c-mode-map
	("C-c t" . disaster)
	:map c++-mode-map
	("C-c t" . disaster))
  :config
  (setq disaster-objdump "objdump -d -M att -Sl -h --no-show-raw-insn"
	disaster-make-flags "-k"))

;; C#
(use-package csharp-mode :ensure t :defer)

;; Go
(use-package go-mode :ensure t :defer t
  :init
  (use-package company-go :ensure t :defer t)
  (use-package go-eldoc :ensure t :defer t)
  :config
  (add-hook
   'go-mode-hook (lambda ()
                   (setq-local company-backends '(company-go company-capf))
                   (add-hook 'before-save-hook 'gofmt-before-save nil 'local)
                   (go-eldoc-setup))))

;; Rust
(use-package rust-mode :ensure t :defer t
  :init
  (use-package racer :ensure t
    :after rust-mode
    :config
    (add-hook 'rust-mode-hook #'racer-mode))
  :config (setq rust-format-on-save t))

;; LaTeX and PDF
(use-package tex :ensure auctex :defer t)
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
(use-package org :ensure org :defer t
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
(use-package python :ensure nil :defer t
  :init
  (setq python-shell-interpreter "python3"))

(use-package lsp-python :ensure t :defer t :disabled
  :commands lsp-python-enable
  :init
  (add-hook 'python-mode-hook #'lsp-python-enable))

(use-package elpy :ensure t :pin melpa :after python
    :init
    (add-hook 'elpy-mode-hook
              (lambda ()
                (setq-local company-backends '(elpy-company-backend))))
    :config
    (setq elpy-rpc-python-command "python3"
          elpy-modules '(elpy-module-sane-defaults
                         elpy-module-eldoc
                         elpy-module-pyvenv
                         elpy-module-yasnippet))
    (elpy-enable))

;; Haskell
(use-package haskell-mode :ensure t :defer t
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
  :config
  (setq markdown-asymmetric-header t))

;; HTML/CSS/JS
(use-package js2-mode :ensure t :defer t)
(use-package web-mode :ensure t :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.[tj]sx?\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))

;; Misc
(use-package systemd :ensure t :defer t)
(use-package json-mode :ensure t :defer t)

(provide 'init)
