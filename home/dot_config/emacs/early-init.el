;;; early-init.el --- pre-frame startup tuning -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)

;; `gcmh' takes over `gc-cons-threshold', `after-init' is backup
(defvar els--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist els--file-name-handler-alist)))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

(setq native-comp-async-report-warnings-errors 'silent)
