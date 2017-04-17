;(add-to-list 'load-path (concat user-emacs-directory "config"))
;(add-to-list 'load-path (concat user-emacs-directory "config" "/lang"))

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/")))

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
	    

(provide 'init)

;; == END INIT.EL ==
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(deft-directory (expand-file-name "~/Dropbox/Text"))
 '(deft-extensions (quote ("txt" "md" "org")))
 '(deft-use-filename-as-title t)
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:hide-gutter t)
 '(git-gutter:modified-sign "*")
 '(git-gutter:update-interval 1)
 '(git-gutter:visual-line t)
 '(linum-relative-format " %s ")
 '(package-selected-packages
   (quote
    (pdf-tools stickyfunc-enhance fiplr deft markdown-mode helm-swoop helm-ag helm flx-ido popwin evil-surround evil-matchit evil-commentary evil-leader key-chord git-gutter linum-relative zenburn-theme use-package)))
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
