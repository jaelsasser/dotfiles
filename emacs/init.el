;;; init.el --- custom emacs init file
(require 'package)
(require 'xdg)

(defvar user-emacs-data (concat (or (xdg-data-home) "~/.local/share") "/emacs")
  "${XDG_CONFIG_HOME:-~/.local/share}/emacs")

(setq custom-file (concat user-emacs-data "/custom.el")
      package-user-dir (format (concat user-emacs-data "/elpa-%s/")
                               emacs-major-version)

      ;; set package priorities: melpa-stable > elpa > melpa-nightly
      package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 10)
                                   ("elpa" . 5)
                                   ("melpa" . 0))
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


;;;
;;; Sensible defaults
;;;

(require 'uniquify)
(require 'saveplace)

;; make sure PATH matches our shell path on macOS
(use-package exec-path-from-shell :ensure t
  :init (setq exec-path-from-shell-check-startup-files nil
              exec-path-from-shell-shell-name "zsh"
              exec-path-from-shell-arguments '())
  :config (exec-path-from-shell-initialize))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              save-place t)

(setq save-place-file (concat user-emacs-data "/places")
      backup-directory-alist `(("." . ,(concat user-emacs-data "/backups")))
      auth-sources `((:source ,(concat user-emacs-data "/authinfo.gpg")))
      auto-save-default nil
      load-prefer-newer t

      epa-pinentry-mode 'loopback
      x-underline-at-descent-line t
      tramp-default-method "ssh"
      tramp-chunksize 512

      jit-lock-stealth-time 0.05   ; fontify buffers when idle
      jit-lock-context-time 0.05   ; fontify contextual changes sooner
      eldoc-idle-delay 0.05        ; show eldoc when idle
      show-paren-delay 0           ; show parens when idle

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

      xref-prompt-for-identifier () ; don't prompt on cross-references
      inhibit-startup-screen t
      load-prefer-newer t)

;; via EmacsWiki: KillingAndYanking
(defun unix-werase-or-kill (arg)
  "When a region is active, `kill-region'; otherwise, `backword-kill-word'"
  (interactive "*p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(bind-keys ("C-w" . unix-werase-or-kill)
           ("C-x k" . kill-this-buffer)

           ([remap dabbrev-expand] . hippie-expand)

           ("C-s" . isearch-forward-regexp)
           ("C-r" . isearch-backward-regexp)
           ("C-M-s" . isearch-forward)
           ("C-M-r" . isearch-backward))

(bind-keys :prefix "C-x \\"
           :prefix-map elisp-profiler-map
           ("[" . profiler-start)
           ("]" . profiler-stop)
           ("p" . profiler-report))

;;; Enable disabled commands
(put 'downcase-region             'disabled nil)   ; let downcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; let esc-esc work
(put 'narrow-to-page              'disabled nil)   ; let narrowing work
(put 'narrow-to-region            'disabled nil)   ; let narrowing work
(put 'set-goal-column             'disabled nil)   ; C-n and C-p respects goal-column
(put 'upcase-region               'disabled nil)   ; let upcasing work
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'dired-find-alternate-file   'disabled nil)

;; Frame and modeline customization

(global-auto-revert-mode t)             ; reload files on disk
(diminish 'auto-revert-mode)

(line-number-mode t)                    ; line number in mode line
(column-number-mode t)                  ; column number in mode line

(global-font-lock-mode t)		; syntax highlighting
(show-paren-mode t)                     ; highlight matching parens

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(let ((font (if (eq system-type 'darwin)
                '(font . "Menlo-13")
              '(font . "xos4 Terminus-14:antialias=none"))))
  (add-to-list 'default-frame-alist font))

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
  :init
  (setq solarized-high-contrast-mode-line t
        solarized-use-more-italic t
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
     ;; mu4e
     `(mu4e-header-marks-face ((,class (:underline nil :foreground ,yellow))))
     `(mu4e-title-face ((,class (:inherit nil :foreground ,green))))

     `(mu4e-replied-face ((,class (:foreground ,blue :inherit nil))))
     `(mu4e-ok-face ((,class (:foreground ,green))))
     `(mu4e-view-attach-number-face ((,class (:inherit nil :foreground ,orange))))
     `(mu4e-highlight-face ((,class (:inherit highlight))))
     `(mu4e-title-face ((,class (:inherit nil :foreground ,green))))
     `(mu4e-modeline-face ((,class (:inherit nil :weight bold))))

     ;; info view: don't scale faces
     `(info-menu-header ((,class (:inherit s-variable-pitch :weight bold))))
     `(Info-quoted ((,class (:inherit font-lock-constant-face))))

     ;; erc
     `(erc-nick-default-face ((,class (:foreground ,base0 :weight bold))))
     `(erc-notice-face ((,class (:foreground ,base01))))
     `(erc-timestamp-face ((,class (:foreground ,base01))))
     `(erc-action-face ((,class (:foreground ,green-d))))
     `(erc-my-nick-face ((,class (:foreground ,red-d :weight bold))))
     )))

(use-package leuven-theme :ensure t :disabled
  :config
  (load-theme 'leuven t))

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

;; Window Manipulation
(use-package ace-window :ensure t
  :bind ([remap other-window] . ace-window))
(use-package transpose-frame :ensure t :defer t
  :bind ("C-x 7" . transpose-frame))

;; Window Management
(use-package ibuffer
  :config
  (use-package ibuffer-tramp :ensure t :defer t)
  (require 'erc-ibuffer)

  (setq ibuffer-expert t     ; don't prompt for confirmation on delete
        ibuffer-show-empty-filter-groups nil)

  (defun ibuffer-set-filter-groups-dynamic (&optional silent)
    (interactive)
    (setq ibuffer-filter-groups
          (append
           (mapcar (lambda (project)
                     `(,project (projectile-files . ,project)))
                   (projectile-open-projects))
           (ibuffer-tramp-generate-filter-groups-by-tramp-connection)
           '(("emacs" (or (name . "^\\*scratch\\*$")
                          (name . "^\\*Messages\\*$")
                          (name . "^\\*Backtrace\\*$")
                          (directory . package-user-dir)))
             ("IRC: Snoonet" (erc-server . "snoonet"))
             ("IRC: Freenode" (erc-server . "freenode")))))
    (ibuffer-update t))

  (add-hook 'ibuffer-mode-hook #'ibuffer-set-filter-groups-dynamic)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("/ '" . ibuffer-set-filter-groups-dynamic)))

;; Buffer Navigation
(use-package avy :ensure t :defer t
  :bind ("C-:" . avy-goto-char-2))
(use-package swiper :ensure t :defer t :disabled
  :bind ("C-s" . swiper))

;; Misc Utilities
(use-package macrostep :ensure t :defer t)
(use-package rainbow-delimiters :ensure t :defer t)
(use-package rainbow-mode :ensure t :defer t)

;; Emacs Bug-Tracking / Development
(use-package debbugs :ensure t :defer t)

;; Linting
(use-package flycheck :ensure t :defer t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; Terminal
(use-package eshell :ensure nil
  :config
  (setq eshell-destroy-buffer-when-process-dies t))

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

        mu4e-maildir-shortcuts '(("/Inbox" . ?i)
                                 ("/Sent" . ?s)
                                 ("/Trash" . ?t)
                                 ("/All" . ?a))))

;; IRC + Chat
(use-package erc :ensure nil :defer t
  :init
  (defun erc-connect-mine ()
    "Connect to Snoonet/Freenode"
    (interactive)
    (when (y-or-n-p "Connect to IRC?")
      (erc-tls :server "jungle.fcker.ca" :port "6697" :nick "snoonet")
      (erc-tls :server "jungle.fcker.ca" :port "6697" :nick "freenode")))

  :config
  (setq erc-prompt-for-password nil
        erc-network-hide-list '(("freenode" "JOIN" "PART" "QUIT"))
        erc-button-nickname-face 'erc-nick-default-face

        erc-fill-function 'erc-fill-variable
        erc-fill-prefix "        "

        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-timestamp-format "[%H:%M] "
        erc-timestamp-only-if-changed-flag nil

        erc-format-nick-function 'erc-format-@nick

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

  :bind (("C-c e e" . erc-connect-mine)
         :map erc-mode-map
         ("C-<return>" . erc-send-current-line)
         ("<return>" . nil)))

;; Version Control
(use-package magit :ensure t :defer t
  :init (global-magit-file-mode t)
  :diminish magit-file-mode
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-diff-paint-whitespace t)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))
(use-package magithub :ensure t :disabled
  :after magit
  :config (magithub-feature-autoinject t))
(use-package diff-hl :ensure t       ; subtle git-gutter in the fringe
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
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

  ;; initialize a buffer-local projectile-project-name variable to keep
  ;; projectile from re-walking up the directory tree with every redisplay
  (add-hook 'find-file-hook
            (lambda ()
              (set (make-local-variable 'projectile-project-name)
                   (projectile-project-name))))

  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-use-git-grep t

        projectile-switch-project-action 'projectile-commander
        projectile-find-dir-includes-top-level t

        projectile-mode-line ; don't show an empty Projectile indicator
        '(:eval (when (and projectile-project-name
                           (not (string= projectile-project-name "-")))
                  (format " Projectile[%s]" projectile-project-name)))))
(use-package deft :ensure t :defer t   ; lightweight text file indexing
  :config
  (setq deft-directory "~/.local/text"))

;; sexp editing everywhere
(use-package smartparens :ensure t :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  :config
  (advice-add 'sp-backward-unwrap-sexp :after #'indent-for-tab-command)
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))

;; expand regions by semantic regions
(use-package expand-region :ensure t
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

(use-package lsp-mode :ensure t :defer t)

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
                      (make-local-variable 'company-backends)
                      (set (make-local-variable 'company-backends)
                           '(company-irony-c-headers company-irony company-capf)))))

(use-package modern-cpp-font-lock :ensure t :defer t
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package ggtags :ensure t :disabled
  :init
  (add-hook 'c-mode-common-hook 'ggtags-mode))

(use-package rtags :ensure t :pin melpa
  :init
  (add-hook
   'irony-mode-hook (lambda ()
                      (rtags-enable-standard-keybindings)
                      (set (make-local-variable 'eldoc-documentation-function)
                           'rtags-eldoc))))
(use-package disaster :ensure t)

;; Go
(use-package go-mode :ensure t :defer t
  :init
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
(use-package haskell-mode :ensure t
  :init
  (add-hook 'haskell-mode (lambda ()
                             (set (make-local-variable 'eldoc-documentation-function)
                                  'haskell-doc-current-info))))

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
