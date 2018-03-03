;; bootstrap.el --- compatibility shims and core functions

;;;
;;; Shim for xdg-data-home on Emacs < 25.0.50
;;;
(defvar user-emacs-data "~/.local/share/emacs"
  "${XDG_CONFIG_HOME:-~/.local/share}/emacs")
(when (require 'xdg nil 'noerror)
  (setq user-emacs-data (concat (xdg-data-home) "/emacs")))

(defun user-emacs-file (file)
  (concat user-emacs-data "/" file))

(setq custom-file (concat user-emacs-data "/custom.el")
      package-user-dir (format (concat user-emacs-data "/elpa-%s/")
                               emacs-major-version)

      ;; set package priorities: melpa-stable > elpa > melpa-nightly
      package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("elpa" . 2)
				                   ("melpa-stable" . 1)
                                   ("melpa" . 0))
      use-package-enable-imenu-support t
      use-package-always-ensure t)

;; Spacemacs is kind enough to provide a nice MELPA / ELPA mirror for when those go down
;; (setq
;;  package-archives
;; '(("melpa-stable" . "https://raw.githubusercontent.com/syl20bnr/spacemacs-elpa-mirror/master/stable-melpa/")
;;   ("melpa" . "https://raw.githubusercontent.com/syl20bnr/spacemacs-elpa-mirror/master/melpa/")
;;   ("elpa" . "https://raw.githubusercontent.com/syl20bnr/spacemacs-elpa-mirror/master/gnu/")))

(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'bootstrap)
