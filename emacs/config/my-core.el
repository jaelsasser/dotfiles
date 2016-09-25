(eval-when-compile (require 'cl))
(require 'cl-lib)

;; clean startup + scratch
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; don't scroll lines off edge
(global-visual-line-mode t)

;; use spaces instead of tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(global-set-key (kbd "TAB") 'self-insert-command) ;no auto-indent tabs for me

;; kill right-side fringe
(set-fringe-style '(8 . 0))

;; cleaner ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; enable syntax highlighting
(global-font-lock-mode t)

;; show parentheses
(show-paren-mode 1)

;; always follow symlinks
(setq vc-follow-symlinks nil)

;; no lockfiles
(setq create-lockfiles nil)

;; keep shit clean
(defvar my-auto-save-folder "~/.emacs.d/.saves/"
  "Directory used for Emacs backups.")

(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-folder t)))

(setq auto-save-default nil)
(setq make-backup-files nil)

;; whitespace
(global-whitespace-mode t)
(setq whitespace-style '(face trailing))

;; UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; only one space after sentences
(setq sentence-end-double-space nil)

;; only scroll one line at the bottom of the screen
(setq scroll-conservatively 9999
            scroll-preserve-screen-position t)

;; from <https://github.com/bling/dotemacs/>
(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
          '(progn ,@body)))

(provide 'my-core)
