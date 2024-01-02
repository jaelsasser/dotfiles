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

;; install straight.el because why not
(let ((straight-bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (straight-bootstrap-version 6))
  (unless (file-exists-p straight-bootstrap-file)
    (with-current-buffer
	    (url-retrieve-synchronously
	     "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load straight-bootstrap-file nil 'nomessage))

(require 'package)
(package-initialize)

(provide 'bootstrap)
