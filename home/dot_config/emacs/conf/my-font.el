;;; my-font.el --- global font with graceful fallback -*- lexical-binding: t; -*-

;; Set the default + fixed-pitch faces to the first installed family in a
;; best-first stack. Runs once on the first graphical frame (daemon-safe) then
;; self-removes; ligatures engage editor-wide for families that resolve them.

(defvar my--font-stack
  ;; (FAMILY . LIGATURES-REACHABLE-IN-EMACS-P) — Monaspace's ligatures hide behind
  ;; ssNN stylistic sets ligature.el can't reach; only a Frozen cut (sets pre-baked
  ;; into calt) renders them, so stock "Monaspace Neon" carries nil.
  '(("Berkeley Mono"         . t)
    ("Monaspace Neon Frozen" . t)
    ("CommitMono"            . t)
    ("JetBrains Mono"        . t)
    ("Monaspace Neon"        . nil)
    ("Hack"                  . nil)
    ("Menlo"                 . nil))
  "Preferred fonts, best first.
The cdr records whether ligatures resolve in Emacs for that family.")

(defun my--apply-font (&optional frame)
  "Set the global font once a graphical FRAME exists; self-removing."
  (when (display-graphic-p frame)
    (let ((families (font-family-list frame)))
      (when-let* ((pick (seq-find (lambda (e) (member (car e) families))
                                  my--font-stack)))
        (set-face-attribute 'default     nil :family (car pick) :height 140)
        (set-face-attribute 'fixed-pitch nil :family (car pick))
        (when (and (cdr pick) (fboundp 'global-ligature-mode))
          (global-ligature-mode 1))
        (remove-hook 'after-make-frame-functions #'my--apply-font)))))

(use-package ligature
  :config
  ;; One union set for every buffer; the font decides which sequences have glyphs.
  ;; (my--apply-font owns the global toggle, so don't enable it here.)
  (ligature-set-ligatures
   t
   '("==" "===" "!=" "!==" "=~" "<=" ">=" "<=>" "<>" "</" "</>" "/>"
     "->" "=>" "->>" "<-" "<<-" "-->" "<--" "<->" "<~" "~>" "~~" "~~>"
     "|>" "<|" "||>" "<||" ":=" "::" ":::" ".." "..." "++" "+++" "--" "---"
     "**" "***" "//" "///" "/*" "*/" "/=" "&&" "||" "??" "?." "?:"
     "<<" ">>" "<<<" ">>>" "<<=" ">>=" "##" "###" "####" "<!--")))

;; Apply now for a live session; the hook catches the daemon's first GUI client.
(add-hook 'after-make-frame-functions #'my--apply-font)
(my--apply-font)

(provide 'my-font)
