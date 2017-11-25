(require 'package)

(defvar user-emacs-data "/tmp/emacs-sandbox")

(setq custom-file "/tmp/emacs-sandbox/custom.el"
      package-user-dir "/tmp/emacs-sandbox/elpa/"
      package-archives `(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      package-enable-at-startup nil
      use-package-enable-imenu-support t)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/conf")

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
