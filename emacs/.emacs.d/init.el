(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "config" "/lang"))

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(setq package-archives '(
             ("melpa" . "http://melpa.milkbox.net/packages/")
             ("org"   . "http://orgmode.org/elpa/")
             ("gnu"   . "http://elpa.gnu.org/packages/")
             ))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; byte-compile for speed
(byte-recompile-directory user-emacs-directory 0)

(require 'my-appearance)
(require 'my-core)
(require 'my-evil)

(require 'my-buffers)
(require 'my-notes)

(require 'my-ide)
(require 'my-c)


(provide 'init)
