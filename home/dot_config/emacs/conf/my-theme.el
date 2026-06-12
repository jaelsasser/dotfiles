;;; my-theme.el --- system-appearance light/dark theme pairs -*- lexical-binding: t; -*-

;; A set of light/dark theme pairs, one active at a time. Only graphical frames
;; theme themselves — a terminal client inherits its ANSI palette (the repo-wide
;; rule) — and the active pair follows `ns-system-appearance'. `my/load-theme-pair'
;; switches pairs (installing on first pick); C-c t flips light<->dark.

(defvar my-theme-pairs
  ;; NAME         LIGHT                     DARK                      RECIPE (nil = built-in)
  '(("Modus"      modus-operandi            modus-vivendi             nil)
    ("Flexoki"    flexoki-themes-light      flexoki-themes-dark       flexoki-themes)
    ("Selenized"  solarized-selenized-light solarized-selenized-dark  solarized-theme)
    ("Everforest" everforest-hard-light     everforest-hard-dark      (everforest :host github :repo "Theory-of-Everything/everforest-emacs" :branch "master2"))
    ("Rosé Pine"  rose-pine-dawn            rose-pine                 (rose-pine :host github :repo "thongpv87/rose-pine-emacs"))
    ("Kanagawa"   kanagawa-lotus            kanagawa-wave             kanagawa-themes))
  "Light/dark theme pairs: (NAME LIGHT DARK RECIPE).
RECIPE is an elpaca order installed on first selection, or nil for a built-in.")

(defvar my--light-theme nil "Light variant of the active pair.")
(defvar my--dark-theme  nil "Dark variant of the active pair.")
(defvar my--current-theme nil "The variant currently enabled.")

(defun my--enable-theme (theme)
  "Enable THEME alone, disabling the others so they don't composite."
  (setq my--current-theme theme)
  (mapc #'disable-theme (remq theme custom-enabled-themes))
  (enable-theme theme))

(defun my--theme-for-appearance (appearance)
  "Enable the active-pair variant matching APPEARANCE (light, else dark)."
  (my--enable-theme (if (eq appearance 'light) my--light-theme my--dark-theme)))

(defun my/invert-theme ()
  "Flip between the light and dark variants of the active pair."
  (interactive)
  (my--enable-theme (if (eq my--current-theme my--dark-theme)
                        my--light-theme my--dark-theme)))

(defun my--apply-theme (&optional frame)
  "Enable the live-appearance variant once a graphical FRAME exists; self-removing.
Daemon/terminal startup has no frame to theme yet; this fires on the first one."
  (when (display-graphic-p frame)
    (my--theme-for-appearance (and (boundp 'ns-system-appearance) ns-system-appearance))
    (remove-hook 'after-make-frame-functions #'my--apply-theme)))

(defun my/load-theme-pair (name)
  "Install (if needed), load, and activate theme pair NAME from `my-theme-pairs'.
A recipe stays off `load-path' until installed, so `custom-available-themes'
gates the one-time elpaca fetch."
  (interactive (list (completing-read "Theme pair: " my-theme-pairs nil t)))
  (pcase-let ((`(,light ,dark ,recipe) (cdr (assoc name my-theme-pairs))))
    (when (and recipe (not (memq light (custom-available-themes))))
      (elpaca-try recipe)
      (elpaca-wait))
    (load-theme light t t)
    (load-theme dark  t t)
    (setq my--light-theme light my--dark-theme dark)
    (my--apply-theme)))

(when (boundp 'ns-system-appearance)
  (add-hook 'ns-system-appearance-change-functions #'my--theme-for-appearance))
(keymap-global-set "C-c t" #'my/invert-theme)
(add-hook 'after-make-frame-functions #'my--apply-theme)
(my/load-theme-pair "Modus")

(provide 'my-theme)
