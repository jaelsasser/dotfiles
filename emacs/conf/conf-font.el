;;; conf-font.el --- global coding font with graceful fallback -*- lexical-binding: t; -*-

;; Pick the first installed family from a best-first stack and set it globally on
;; the default + fixed-pitch faces. `set-face-attribute' with FRAME nil covers
;; existing and future frames at once, so this runs once at load — no per-frame
;; hook. Ligatures switch on only when the chosen family resolves them inside
;; Emacs; the common no-ligature font never touches ligature.el's per-buffer
;; machinery. A non-GUI frame yields an empty family list and is left alone.

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

(defun els--apply-coding-font ()
  "Set the global coding font from `els--coding-font-stack'; match ligatures.
One `font-family-list' enumeration drives the membership test, so the missing
heads of the stack cost a string compare apiece to skip past."
  (let ((families (font-family-list)))
    (when-let* ((pick (seq-find (lambda (e) (member (car e) families))
                                els--coding-font-stack)))
      (set-face-attribute 'default     nil :family (car pick) :height 140)
      (set-face-attribute 'fixed-pitch nil :family (car pick))
      ;; ligatures are a global toggle; only engage the machinery when the font
      ;; actually resolves them
      (when (and (cdr pick) (fboundp 'global-ligature-mode))
        (global-ligature-mode 1)))))

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

(els--apply-coding-font)

(provide 'conf-font)
