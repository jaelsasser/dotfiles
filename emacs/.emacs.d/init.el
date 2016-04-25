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

(require 'my-core)

(require 'my-appearance)
(require 'my-evil)

(require 'my-semantic)
(require 'my-c)

(provide 'init)
