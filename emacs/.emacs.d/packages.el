(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

;; packages
(setq package-list '(base16-theme
                     better-defaults
                     company
                     deft
                     evil
                     evil-escape
                     evil-leader
                     evil-search-highlight-persist
                     helm
                     nlinum ;faster linum
                     powerline
                     powerline-evil
                     python-mode
                     magit
                     markdown-mode
                     org
                     which-key
                     zenburn-theme))

;; load in package cotents if it doesn't exist
(or (file-exists-p package-user-dir)
        (package-refresh-contents))

;; install on command
(dolist (package package-list)
    (package-initialize)
    (unless (package-installed-p package)
        (package-install package)))
