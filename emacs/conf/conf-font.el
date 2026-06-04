;;; conf-font.el --- global coding font with graceful fallback -*- lexical-binding: t; -*-

;; Pick the first installed family from a best-first stack and set it globally on
;; the default + fixed-pitch faces. `set-face-attribute' with FRAME nil covers
;; existing and future frames, so the work happens exactly once: at load for a
;; normal session, or on the daemon's first GUI client (the family list is empty
;; until a graphical frame exists). The one-shot then drops its own frame hook,
;; so later emacsclient frames inherit the font without repeating the lookup.
;; Ligatures switch on only when the chosen family resolves them inside Emacs;
;; the common no-ligature font never touches ligature.el's per-buffer machinery.

(defvar els--coding-font-stack
  ;; (FAMILY . LIGATURES-REACHABLE-IN-EMACS-P)
  ;; Verify family strings with M-: (font-family-list), or `fc-list' on Linux.
  ;; Monaspace's ligatures hide behind ssNN stylistic sets that ligature.el can't
  ;; reach; only a Frozen cut (sets pre-baked into calt) renders them in Emacs, so
  ;; stock "Monaspace Neon" carries nil.
  '(("Berkeley Mono"         . t)
    ("Monaspace Neon Frozen" . t)
    ("CommitMono"            . t)
    ("JetBrains Mono"        . t)
    ("Monaspace Neon"        . nil)
    ("Hack"                  . nil)
    ("Menlo"                 . nil))
  "Preferred coding fonts, best first.
The cdr records whether ligatures resolve in Emacs for that family.")

(defun els--apply-coding-font (&optional frame)
  "Set the global coding font once a graphical FRAME exists, then stop.
`set-face-attribute' with FRAME nil makes the choice global, so this self-removes
from `after-make-frame-functions' after the first graphical frame and later
frames inherit it. One `font-family-list' enumeration drives the membership
test, so the missing heads of the stack cost a string compare apiece to skip."
  (when (display-graphic-p frame)
    (let ((families (font-family-list frame)))
      (when-let* ((pick (seq-find (lambda (e) (member (car e) families))
                                  els--coding-font-stack)))
        (set-face-attribute 'default     nil :family (car pick) :height 140)
        (set-face-attribute 'fixed-pitch nil :family (car pick))
        ;; ligatures are a global toggle; only engage the machinery when the
        ;; font actually resolves them
        (when (and (cdr pick) (fboundp 'global-ligature-mode))
          (global-ligature-mode 1))
        ;; the choice is global now; stop re-running on later frames
        (remove-hook 'after-make-frame-functions #'els--apply-coding-font)))))

(use-package ligature
  :config
  ;; One union set for prog-mode; the font decides which sequences have glyphs.
  ;; els--apply-coding-font owns global-ligature-mode, so don't enable it here.
  (ligature-set-ligatures
   'prog-mode
   '("==" "===" "!=" "!==" "=~" "<=" ">=" "<=>" "<>" "</" "</>" "/>"
     "->" "=>" "->>" "<-" "<<-" "-->" "<--" "<->" "<~" "~>" "~~" "~~>"
     "|>" "<|" "||>" "<||" ":=" "::" ":::" ".." "..." "++" "+++" "--" "---"
     "**" "***" "//" "///" "/*" "*/" "/=" "&&" "||" "??" "?." "?:"
     "<<" ">>" "<<<" ">>>" "<<=" ">>=" "##" "###" "####" "<!--")))

;; Apply now if a graphical frame already exists (normal session); otherwise the
;; hook catches the daemon's first GUI client.
(add-hook 'after-make-frame-functions #'els--apply-coding-font)
(els--apply-coding-font)

(provide 'conf-font)
