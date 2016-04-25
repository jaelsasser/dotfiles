(require 'cl)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/evil.el")

;; theme
(load-theme 'zenburn t)

;; tabs and indents
(global-set-key (kbd "TAB") 'self-insert-command);
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent 'complete)

;; disable some visual bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; helm
(require 'helm-config)

;; which-key
(which-key-setup-minibuffer)

;; enable syntax highlighting
(global-font-lock-mode t)

;; enable line numbers
(global-nlinum-mode t)
(setq-default nlinum-format " %d ")
(add-hook 'nlinum-mode-hook ;pre-compute width
          (lambda ()
            (when nlinum-mode
              (setq nlinum--width
                    (2+ (length (number-to-string
                                (count-lines (point-min) (point-max)))))
              (nlinum--flush))))

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; disable symlink question
(setq vc-follow-symlinks nil)

;; whitespace
(global-whitespace-mode t)
(setq whitespace-style '(face tabs trailing))

;; org mode
(add-hook 'org-mode-hook
          (lambda ()
          (org-indent-mode t)))

;; deft
(setq deft-directory "~/Dropbox/Text")
(setq deft-use-filename-as-title t)

;; wraps the lines in org-mode
(setq org-startup-truncated nil)

