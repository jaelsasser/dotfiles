;;; conf-theme.el --- flexoki, with a switchable rack of dark/light pairs -*- lexical-binding: t; -*-

;; One light/dark pair is active at a time. The machinery below follows the macOS
;; system appearance (emacs-plus NS port) with C-c t as a manual override; both
;; routes flow through `my--enable-theme'. Each theme package's `:config' claims
;; the active slot via `my--use-pair', so auditioning an alternative is just
;; deleting its `:disabled'. Flexoki is the default; Selenized (the solarized-like,
;; riding the solarized-theme package) and the rest wait disabled until enabled.

(defvar my--light-theme nil "Light variant of the active pair.")
(defvar my--dark-theme  nil "Dark variant of the active pair.")
(defvar my--current-theme nil "The variant currently enabled.")

(defun my--enable-theme (theme)
  "Enable THEME, disabling every other enabled theme so they don't composite."
  (setq my--current-theme theme)
  (dolist (th (copy-sequence custom-enabled-themes))
    (unless (eq th theme) (disable-theme th)))
  (let ((custom--inhibit-theme-enable nil))
    (enable-theme theme)))

(defun my--theme-for-appearance (appearance)
  "Enable the active-pair variant matching APPEARANCE.
Light maps to light; dark and the terminal's nil both map to dark."
  (my--enable-theme (if (eq appearance 'light) my--light-theme my--dark-theme)))

(defun my/invert-theme ()
  "Flip between the light and dark variants of the active pair."
  (interactive)
  (my--enable-theme (if (eq my--current-theme my--dark-theme)
                         my--light-theme
                       my--dark-theme)))

(defun my--follow-system-appearance (&optional frame)
  "Theme the active pair once a graphical FRAME exists, then follow OS flips.
Self-removing after the first graphical frame; terminal frames are left bare so
they inherit the terminal's palette. Off the NS port appearance is unknown, so
this falls back to dark."
  (when (display-graphic-p frame)
    (my--theme-for-appearance (and (boundp 'ns-system-appearance) ns-system-appearance))
    (remove-hook 'after-make-frame-functions #'my--follow-system-appearance)))

(defun my--use-pair (light dark)
  "Make LIGHT/DARK the active pair; theme graphical frames only.
Discipline: a terminal Emacs stays bare and inherits the terminal's palette. A GUI
frame loads the variant for the live system appearance and follows later flips; a
daemon waits for its first graphical client. Themes are global, so a daemon serving
a GUI and a terminal client at once themes both — the pure-GUI and pure-terminal
sessions are what this gets right."
  (load-theme light t t)
  (load-theme dark  t t)
  (setq my--light-theme light
        my--dark-theme  dark)
  (when (boundp 'ns-system-appearance)
    (add-hook 'ns-system-appearance-change-functions #'my--theme-for-appearance))
  (if (display-graphic-p)
      (my--theme-for-appearance (and (boundp 'ns-system-appearance) ns-system-appearance))
    (add-hook 'after-make-frame-functions #'my--follow-system-appearance)))

(defvar my--theme-pairs
  '(("Flexoki"    flexoki-themes-light      flexoki-themes-dark)
    ("Selenized"  solarized-selenized-light solarized-selenized-dark)
    ("Everforest" everforest-hard-light     everforest-hard-dark)
    ("Rosé Pine"  rose-pine-dawn            rose-pine)
    ("Kanagawa"   kanagawa-lotus            kanagawa-wave))
  "The whole rack, named. The switcher offers only the *installed* pairs:
a `:disabled' package never lands on the load path, so un-disabling one to
audition it (per the top comment) is also what surfaces it in the switcher.")

(defun my/switch-theme-pair (name)
  "Pick an installed light/dark pair by NAME and make it the active pair.
Filtered against `custom-available-themes' so the dormant, uninstalled rack
entries don't show up and then bomb out in `load-theme'."
  (interactive
   (let ((available (custom-available-themes)))
     (list (completing-read
            "Theme pair: "
            (seq-filter (pcase-lambda (`(,_name ,light ,_dark))
                          (memq light available))
                        my--theme-pairs)
            nil t))))
  (pcase-let ((`(,light ,dark) (cdr (assoc name my--theme-pairs))))
    (my--use-pair light dark)))

(keymap-global-set "C-c t"   #'my/invert-theme)
(keymap-global-set "C-c T" #'my/switch-theme-pair)

(use-package flexoki-themes :defer nil :no-require t
  :config (my--use-pair 'flexoki-themes-light 'flexoki-themes-dark))

(use-package solarized-theme :disabled :no-require t  ; Selenized (the solarized-like)
  :config (my--use-pair 'solarized-selenized-light 'solarized-selenized-dark))

(use-package everforest :disabled :no-require t
  :ensure (everforest :host github :repo "Theory-of-Everything/everforest-emacs" :branch "master2")
  :config (my--use-pair 'everforest-hard-light 'everforest-hard-dark))

(use-package rose-pine :disabled :no-require t
  :ensure (rose-pine :host github :repo "thongpv87/rose-pine-emacs")
  :config (my--use-pair 'rose-pine-dawn 'rose-pine))

(use-package kanagawa-themes :disabled :no-require t
  :config (my--use-pair 'kanagawa-lotus 'kanagawa-wave))

(provide 'conf-theme)
