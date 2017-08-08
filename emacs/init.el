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
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(diminish 'abbrev-mode)
(diminish 'eldoc-mode)
(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode)



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

(defun jae--setup-prog-mode ()
  (setq-local show-trailing-whitespace t)
  (setq-local whitespace-style '(face space-after-tab::space trailing lines-tail))
  (setq-local whitespace-line-column 80)
  (whitespace-mode 1)
  (toggle-truncate-lines 1))
(add-hook 'prog-mode-hook #'jae--setup-prog-mode)

(defun jae--large-file-hook ()
  "Turn off expensive functions (font-lock, undo-mode) for large files"
  (when (> (buffer-size) (* 1024 1024))
    (setq-local buffer-read-only t)
    (undo-tree-mode -1)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook #'jae--large-file-hook)

(setq-default truncate-lines t
	      indent-tabs-mode t)

(setq auth-sources `((:source ,(concat user-emacs-data "/authinfo.gpg")))
      load-prefer-newer t
      frame-title-format "[Emacs] %b"
      auto-hscroll-mode 'current-line

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

(when (executable-find "rg")
    (setq counsel-grep-base-command
	  "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

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

(global-auto-revert-mode t)             ; reload files from disk
(diminish 'auto-revert-mode)

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
  :if (not (eq system-type 'darwin))
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

(use-package leuven-theme :ensure t :pin melpa
  :if (eq system-type 'darwin)
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
               '("\\.jaalam\\.net\\'" "\\`root\\'" "/ssh:admin@%h:")))

;; Better Key Discovery
(use-package which-key :ensure t
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

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
         ("C-c j" . counsel-imenu)))
(use-package flx :ensure t :defer t :disabled) ; better fuzzy-match sort
(use-package smex :ensure t :defer t           ; better M-x sort
  :config (setq smex-save-file (concat user-emacs-data "/smex-items")))

;; Code Completion
(use-package company :ensure t
  :init (global-company-mode)
  :diminish company-mode
  :config
  (setq company-idle-delay nil		; only complete when asked (C-M-i, usually)
        company-minimum-prefix-length 0
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-backends '(company-capf company-dabbrev))
  :bind (([remap completion-at-point] . company-complete)
         ([remap complete-symbol] . company-complete)
         :map company-active-map
         ("C-w" . nil)
         ("M-." . company-show-location)))

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
                     `(,project (projectile-files . ,project)))
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
         ("C-M-s" . search-forward-regexp)))

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

;; Terminal
(use-package eshell :ensure nil
  :config
  (setq eshell-destroy-buffer-when-process-dies t))

;; Newsgroups + (Maybe) Mail
(use-package gnus :ensure nil :defer t
  :config
  (setq gnus-select-method '(nntp "news.gmane.org")
        gnus-startup-file (concat user-emacs-data "/newsrc")
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil))

;; Mail
(use-package mu4e :ensure nil :defer t
  :config
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-maildir "~/.local/mail"
        mu4e-attachments-dir "~/Downloads/Attachments"

        mu4e-completing-read-function 'ivy-completing-read
        mu4e-sent-messages-behavior 'delete
        mu4e-change-filenames-when-moving t
        message-kill-buffer-on-exit t

        mu4e-get-mail-command "mbsync -V -c ~/.config/mail/mbsyncrc gmail"

        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent"
        mu4e-trash-folder  "/Trash"
        mu4e-refile-folder "/All"

        mu4e-bookmarks
        '(("flag:unread AND NOT flag:trashed AND NOT m:/Lists/*" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("m:/Lists/.Emacs/.devel" "emacs-devel" ?e))

        mu4e-maildir-shortcuts
        '(("/Inbox" . ?i)
          ("/Sent" . ?s)
          ("/Trash" . ?t)
          ("/All" . ?a)))
  :bind ("C-c m" . mu4e))

;; IRC + Chat
(use-package erc :ensure nil :defer t :disabled
  :init
  (defun erc-switch-buffers-or-start ()
    "Connect to Snoonet/Freenode"
    (interactive)
    (defvar jae--erc-connected nil
      "Set to `t' after this emacs instance has connected to IRC")
    (when (and (not jae--erc-connected) (y-or-n-p "Connect to IRC?"))
      (erc-tls :server "jungle.fcker.ca" :port "6697" :nick "snoonet")
      (erc-tls :server "jungle.fcker.ca" :port "6697" :nick "freenode")
      (setq jae--erc-connected t))
    (counsel-erc))

  (defvar counsel-erc-map (make-sparse-keymap))
  (ivy-set-actions
   'counsel-erc
   '(("k"
      (lambda (x)
        (kill-buffer x)
        (ivy--reset-state ivy-last))
      "kill")
     ("j"
      ivy--switch-buffer-other-window-action
      "other window")))

  (defun counsel-erc ()
    "Quickly switch to between `erc' buffers"
    (interactive)
    (ivy-read "ERC: " (mapcar #'buffer-name (erc-buffer-list))
              :require-match t
              :action #'switch-to-buffer
              :keymap ivy-switch-buffer-map
              :sort t
              :caller 'counsel-erc))

  :config
  (setq erc-prompt-for-password nil
        erc-network-hide-list '(("card.freenode" "JOIN" "PART" "QUIT"))
        erc-button-nickname-face 'erc-nick-default-face

        erc-fill-function 'erc-fill-variable
        erc-fill-prefix "        "

        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-timestamp-format "[%H:%M] "
        erc-timestamp-only-if-changed-flag t
        erc-prompt ">>>"

        erc-format-nick-function 'erc-format-@nick
        erc-join-buffer 'bury

        erc-button-buttonize-nicks nil
        erc-button-wrap-long-urls t

        erc-modules '(netsplit
                      fill
                      button
                      capab-identify
                      completion
                      irccontrols
                      readonly
                      ring
                      move-to-prompt
                      stamp
                      scrolltobottom
                      truncate)
        erc-input-line-position -1

        erc-interpret-mirc-color t
        erc-rename-buffers t
        erc-kill-buffer-on-part t)

  :bind (("C-c i" . erc-switch-buffers-or-start)
         :map erc-mode-map
         ("<C-return>" . erc-send-current-line)
         ("RET" . nil)
         ("C-c TAB" . nil)))

;; Version Control
(use-package magit :ensure t
  :init (global-magit-file-mode t)
  :diminish magit-file-mode
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-paint-whitespace t)
  :bind (("M-]" . magit-file-popup)))
(use-package diff-hl :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq diff-hl-draw-borders nil))

;; Branching Undo
(use-package goto-chg :ensure t
  :bind ("M-]" . goto-last-change))
(use-package undo-tree :ensure t
  :init (global-undo-tree-mode t)
  :diminish undo-tree-mode)

;; Evil
(use-package evil :ensure t :pin melpa
  :init
  (setq evil-default-state 'insert
	evil-disable-insert-state-bindings t
        evil-toggle-key "C-\\"

	evil-normal-state-modes '(prog-mode text-mode)

        evil-echo-state t
        evil-mode-line-format nil
        evil-want-C-u-scroll nil
        evil-want-C-i-jump nil

        evil-highlight-closing-paren-at-point-states ()
        evil-move-beyond-eol t
        evil-track-eol nil

        evil-search-module 'isearch
        evil-magic 'very
        evil-want-fine-undo nil)

  ;; extra motions & operations
  (use-package evil-easymotion :ensure t :after evil
    :init (evilem-default-keybindings "SPC"))
  (use-package evil-smartparens :ensure t :after evil
    :diminish evil-smartparens-mode
    :init (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  (use-package evil-snipe :ensure t :after evil
    :diminish evil-snipe-local-mode
    :init (evil-snipe-mode t)
    :config
    (setq evil-snipe-scope 'visible
          evil-snipe-use-vim-sneak-bindings t)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode))
  (use-package evil-surround :ensure t :after evil
    :init (global-evil-surround-mode t))

  ;; extra textobjects
  (use-package evil-args :ensure t :after evil
    :bind (:map
           evil-inner-text-objects-map ("a" . evil-inner-arg)
           :map
           evil-outer-text-objects-map ("a" . evil-outer-map)))
  (use-package evil-indent-plus :ensure t :after evil
    :init (evil-indent-plus-default-bindings))

  ;; extra keybinds for various modes
  ;; TODO: implement without a depend on evil-leader
  (use-package evil-org :ensure t :after org :disabled
    :config
    (add-hook 'org-mode-hook #'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme '(operators navigation textobjects)))))

  ;; TODO: track upstream
  (use-package targets.el :ensure nil :disabled)

  :config
  (evil-mode t)

  :bind
  (("M-[" . evil-normal-state)
   :map evil-normal-state-map
   ("C-r" . nil)
   ("C-n" . nil)
   ("C-p" . nil)
   ("C-t" . nil)
   ("C-." . nil)
   ("M-." . nil)
   :map evil-motion-state-map
   ("C-\\" . evil-emacs-state)
   ("C-b" . nil)
   ("C-d" . nil)
   ("C-e" . nil)
   ("C-f" . nil)
   ("C-o" . nil)
   ("C-y" . nil)
   ("C-]" . nil)
   ("C-w" . nil)
   ("C-v" . nil)))

;; Projects
(use-package projectile :ensure t :pin melpa
  :init (projectile-mode)
  :config
  (use-package counsel-projectile :ensure
    :init (counsel-projectile-on))
  ;; initialize a buffer-local projectile-project-name variable to keep
  ;; projectile from re-walking up the directory tree with every redisplay
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-use-git-grep t
        projectile-find-dir-includes-top-level t
        projectile-mode-line nil))

(use-package deft :ensure t :defer t   ; lightweight text file indexing
  :config
  (setq deft-directory "~/.local/text")
  :bind (("C-c d" . deft)
         :map deft-mode-map
         ("C-w" . deft-filter-decrement-word)))

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

;; (use-package lsp-mode :ensure t :defer t :disabled
;;   :init
;;   (use-package lsp-python :ensure t :defer t))

(use-package rtags :ensure t :pin melpa
  :if (not (eq system-type 'darwin))
  :init

  (use-package ivy-rtags :ensure t :pin melpa
    :init (setq rtags-display-result-backend 'ivy))

  (use-package company-rtags :ensure t :pin melpa
    :init (setq rtags-completions-enabled t))

  (use-package flycheck-rtags :ensure t :pin melpa
    :init
    (defun jae--flycheck-rtags-setup-buffer ()
      (flycheck-mode 1)
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil))

    (add-hook 'c-mode-hook #'jae--flycheck-rtags-setup-buffer)
    (add-hook 'c++-mode-hook #'jae--flycheck-rtags-setup-buffer))

  (defun jae--rtags-setup-buffer ()
    (when (rtags-start-process-unless-running)
      (rtags-enable-standard-keybindings c-mode-base-map "C-c C-r")
      (setq-local company-backends
                  '(company-rtags company-capf))))

  (add-hook 'c++-mode-hook #'jae--rtags-setup-buffer)
  (add-hook 'c-mode-hook #'jae--rtags-setup-buffer)
  :config
  (setq rtags-jump-to-first-match nil
	rtags-display-current-error-as-message nil))


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
(use-package org :ensure org-plus-contrib :defer t
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

  (add-hook 'org-mode-hook #'visual-line-mode)

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)))
(use-package interleave :ensure t
  :after org)

;; Python
(use-package python :ensure nil :defer t
  :init
  (setq python-shell-interpreter "python3")

  (use-package anaconda-mode :ensure t :defer t :disabled
    :init
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

  (use-package company-anaconda :ensure t :after anaconda-mode
    :init
    (add-hook 'anaconda-mode-hook
              (lambda ()
                (setq-local company-backends
                            '(company-anaconda company-capf))))
    :bind (:map anaconda-mode-map
                ([remap anaconda-mode-complete] . company-complete)))

  (use-package elpy :ensure t :after python
    :init
    (add-hook 'elpy-mode-hook (lambda ()
                                (setq-local company-backends '(elpy-company-backend))))
    :config
    (setq elpy-rpc-python-command "python3"
          elpy-modules '(elpy-module-sane-defaults
                         elpy-module-eldoc
                         elpy-module-pyvenv
                         elpy-module-yasnippet))
    (elpy-enable)))

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
