;;; install.el --- eager elpaca install + byte-compile -*- lexical-binding: t; -*-

;; Loaded *after* init.el, never at startup. Drives elpaca to completion so the
;; first real launch is warm, then byte-compiles the config. The chezmoi
;; run_onchange bootstrap runs this in a -nw frame (live progress); emacs.bats
;; runs it under --batch, where `noninteractive' makes byte-compile warnings
;; fatal and any failed build a non-zero exit.

(require 'elpaca)

(unless noninteractive (elpaca-log))     ; live progress in the -nw frame
(elpaca-process-queues)                  ; the -q test path never fired the after-init hook
(elpaca-wait)

(let (failed)
  (dolist (cell (elpaca--queued))
    (when (eq (elpaca--status (cdr cell)) 'failed)
      (push (car cell) failed)))
  (when failed
    (message "elpaca: %d package(s) failed to build: %S" (length failed) failed)
    (when noninteractive (kill-emacs 1))))

;; Warnings are fatal only under --batch (the test); a real apply just shows them.
(let ((byte-compile-error-on-warn noninteractive)
      ;; file-exists-p drops dangling farm leftovers from renamed conf/*.el.
      (files (seq-filter
              #'file-exists-p
              (append
               (list (expand-file-name "early-init.el" user-emacs-directory)
                     (expand-file-name "init.el" user-emacs-directory))
               (directory-files (expand-file-name "conf" user-emacs-directory) t "\\.el\\'")))))
  (dolist (f files)
    ;; A signaled error would skip kill-emacs and hang the blocking -nw apply.
    (unless (condition-case err
                (byte-compile-file f)
              (error (message "byte-compile error: %s: %S" f err) nil))
      (when noninteractive (message "byte-compile failed: %s" f) (kill-emacs 1)))))

(kill-emacs 0)                           ; let chezmoi apply continue (blocking -nw)
