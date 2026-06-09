;;; emacs.el --- startup-time bench probe -*- lexical-binding: t; -*-
;; Loaded last by bench/emacs.sh, in one of two modes (auto-detected):
;;   total  — driver omits the stats var; use-package-statistics is empty, emit TOTAL.
;;   scoped — driver sets `use-package-compute-statistics' before init; stats populate,
;;            emit TOTAL + a per-package PKG line each.
;; `before-init-time' is bound at process start, so its delta to here is the whole
;; foreground load. Results go to $BENCH_OUT (a file) rather than stdout: the driver
;; reads them back, and it keeps the option open for a -nw frame that owns the TTY.
;; Caveat baked into the report, not here: `use-package-statistics-time' sums phase
;; timers that can overlap, so a single hot package may read above the wall total.

(let ((out (getenv "BENCH_OUT"))
      (total (* 1000 (float-time (time-subtract (current-time) before-init-time)))))
  (with-temp-buffer
    (insert (format "TOTAL %.1f\n" total))
    (when (and (boundp 'use-package-statistics)
               (> (hash-table-count use-package-statistics) 0))
      (require 'use-package)
      (let (rows)
        (maphash (lambda (name stats)
                   (push (cons name (use-package-statistics-time stats)) rows))
                 use-package-statistics)
        (dolist (r (sort rows (lambda (a b) (> (cdr a) (cdr b)))))
          (insert (format "PKG %.1f %s\n" (* 1000 (cdr r)) (car r))))))
    (write-region (point-min) (point-max) (or out "/dev/stdout"))))
(kill-emacs 0)
