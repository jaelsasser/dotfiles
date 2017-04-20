(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Enabl use-package
(eval-when-compile
    (require 'use-package))
(require 'diminish)
(require 'bind-key)

(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;; Disable extra gui elements
(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; Sensible defaults
(setq-default
 vc-follow-symlinks nil      ; don't prompt to follow version-controlled symlinks
 inhibit-startup-screen t    ; disable startup screen
 x-select-enable-clibpoard t ; use the system clipboard
 )

;(global-whitepsace-mode t)
;(setq whitespace-style '(face trailing)) ; flag trailing whitespace

(global-font-lock-mode t) ; syntax-highlighting

;; Leuven theme is nice, but I eventually want to find a good dark theme
(use-package leuven-theme
  :ensure t
  :init (load-theme 'leuven t))

;; Set the path variable to match the system PATH
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Pull in pdf-tools
(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install)
          (setq-default pdf-view-display-size 'fit-page))

;; VIM emulation layer; toggle on with C-z
(use-package evil
  :ensure t
  :init (evil-mode t)
  :config (setq evil-default-state 'emacs
		evil-search-module 'evil-search
		evil-magic 'very-magic
		evil-want-fine-undo t
		evil-want-change-word-to-end t))

;; better completion with ivy, counsel, and friends
(use-package ivy
  :ensure t
  :init (ivy-mode 1))

(use-package swiper
  :bind
  (([remap isearch-forward]  . swiper)
   ([remap isearch-backward] . swiper))
  :config
  (setq swiper-action-recenter t))

(use-package counsel
  :defer t)

(provide 'init)
