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

;; macOS GUI launches (Dock/Spotlight) inherit launchd's bare PATH, so
;; native-comp's libgccjit can't find Homebrew's gcc driver and falls back to
;; clang -- which can't link GCC's runtime (ld: library 'emutls_w' not found).
;; `exec-path-from-shell' fixes PATH in init.el, but trampolines (recursive-edit
;; et al.) native-compile before it runs; seat the gcc driver here, pre-.eln.
(when (eq system-type 'darwin)
  (dolist (dir '("/opt/homebrew/bin" "/usr/local/bin"))
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (setenv "PATH" (concat dir path-separator (getenv "PATH"))))))
