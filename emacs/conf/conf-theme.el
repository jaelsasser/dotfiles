;;; conf-theme.el --- flexoki, with a switchable rack of dark/light pairs -*- lexical-binding: t; -*-

;; One light/dark pair is active at a time. The machinery below follows the macOS
;; system appearance (emacs-plus NS port) with C-c t as a manual override; both
;; routes flow through `els--enable-theme'. Each theme package's `:config' claims
;; the active slot via `els--use-pair', so auditioning an alternative is just
;; deleting its `:disabled'. Flexoki is the default; Selenized (the solarized-like,
;; riding the solarized-theme package) and the rest wait disabled until enabled.

(defvar els--light-theme nil "Light variant of the active pair.")
(defvar els--dark-theme  nil "Dark variant of the active pair.")
(defvar els--current-theme nil "The variant currently enabled.")

(defun els--enable-theme (theme)
  "Enable THEME, disabling every other enabled theme so they don't composite."
  (setq els--current-theme theme)
  (dolist (th (copy-sequence custom-enabled-themes))
    (unless (eq th theme) (disable-theme th)))
  (let ((custom--inhibit-theme-enable nil))
    (enable-theme theme)))

(defun els--theme-for-appearance (appearance)
  "Enable the active-pair variant matching APPEARANCE.
Light maps to light; dark and the terminal's nil both map to dark."
  (els--enable-theme (if (eq appearance 'light) els--light-theme els--dark-theme)))

(defun invert-theme ()
  "Flip between the light and dark variants of the active pair."
  (interactive)
  (els--enable-theme (if (eq els--current-theme els--dark-theme)
                         els--light-theme
                       els--dark-theme)))

(defun els--follow-system-appearance (&optional frame)
  "Theme the active pair once a graphical FRAME exists, then follow OS flips.
Self-removing after the first graphical frame; terminal frames are left bare so
they inherit the terminal's palette. Off the NS port appearance is unknown, so
this falls back to dark."
  (when (display-graphic-p frame)
    (els--theme-for-appearance (and (boundp 'ns-system-appearance) ns-system-appearance))
    (remove-hook 'after-make-frame-functions #'els--follow-system-appearance)))

(defun els--use-pair (light dark)
  "Make LIGHT/DARK the active pair; theme graphical frames only.
Discipline: a terminal Emacs stays bare and inherits the terminal's palette. A GUI
frame loads the variant for the live system appearance and follows later flips; a
daemon waits for its first graphical client. Themes are global, so a daemon serving
a GUI and a terminal client at once themes both — the pure-GUI and pure-terminal
sessions are what this gets right."
  (load-theme light t t)
  (load-theme dark  t t)
  (setq els--light-theme light
        els--dark-theme  dark)
  (when (boundp 'ns-system-appearance)
    (add-hook 'ns-system-appearance-change-functions #'els--theme-for-appearance))
  (if (display-graphic-p)
      (els--theme-for-appearance (and (boundp 'ns-system-appearance) ns-system-appearance))
    (add-hook 'after-make-frame-functions #'els--follow-system-appearance)))

(keymap-global-set "C-c t" #'invert-theme)

;; Active: Flexoki.
(use-package flexoki-themes :defer nil :no-require t
  :config (els--use-pair 'flexoki-themes-light 'flexoki-themes-dark))

;; Disabled rack — delete :disabled to audition a pair (the last non-disabled
;; block wins by load order, so disable Flexoki above or just let the chosen
;; block override). :no-require because we never `require' them — `els--use-pair'
;; drives activation through `load-theme'.
(use-package solarized-theme :disabled :no-require t  ; Selenized (the solarized-like)
  :config (els--use-pair 'solarized-selenized-light 'solarized-selenized-dark))

(use-package everforest :disabled :no-require t
  :ensure (everforest :host github :repo "Theory-of-Everything/everforest-emacs" :branch "master2")
  :config (els--use-pair 'everforest-hard-light 'everforest-hard-dark))

(use-package rose-pine :disabled :no-require t
  :ensure (rose-pine :host github :repo "thongpv87/rose-pine-emacs")
  :config (els--use-pair 'rose-pine-dawn 'rose-pine))

(use-package kanagawa-themes :disabled :no-require t
  :config (els--use-pair 'kanagawa-lotus 'kanagawa-wave))

(provide 'conf-theme)
