;;; conf-theme.el --- a lazily-installed rack of light/dark theme pairs -*- lexical-binding: t; -*-

;; One light/dark pair is active at a time, chosen by `my-theme-pair' —
;; `setopt' it to switch, live. Only the *selected* pair is ever elpaca-installed;
;; the rack's other entries stay off disk until picked. The terminal stays bare
;; on purpose — only graphical frames load a theme, so a terminal client inherits
;; its own ANSI palette (the repo-wide theming rule). C-c t flips light<->dark.

(defvar my-theme-rack
  ;; NAME         LIGHT                     DARK                      RECIPE (nil = built-in)
  '(("Modus"      modus-operandi            modus-vivendi             nil)
    ("Flexoki"    flexoki-themes-light      flexoki-themes-dark       flexoki-themes)
    ("Selenized"  solarized-selenized-light solarized-selenized-dark  solarized-theme)
    ("Everforest" everforest-hard-light     everforest-hard-dark      (everforest :host github :repo "Theory-of-Everything/everforest-emacs" :branch "master2"))
    ("Rosé Pine"  rose-pine-dawn            rose-pine                 (rose-pine :host github :repo "thongpv87/rose-pine-emacs"))
    ("Kanagawa"   kanagawa-lotus            kanagawa-wave             kanagawa-themes))
  "Switchable light/dark pairs: (NAME LIGHT DARK RECIPE).
RECIPE is an elpaca order installed on first selection, or nil for a built-in.")

(defvar my--light-theme nil "Light variant of the active pair.")
(defvar my--dark-theme  nil "Dark variant of the active pair.")
(defvar my--current-theme nil "The variant currently enabled.")

(defun my--enable-theme (theme)
  "Enable THEME, disabling every other enabled theme so they don't composite."
  (setq my--current-theme theme)
  (mapc #'disable-theme (remq theme custom-enabled-themes))
  (let ((custom--inhibit-theme-enable nil))
    (enable-theme theme)))

(defun my--theme-for-appearance (appearance)
  "Enable the active-pair variant matching APPEARANCE (light, else dark)."
  (my--enable-theme (if (eq appearance 'light) my--light-theme my--dark-theme)))

(defun my/invert-theme ()
  "Flip between the light and dark variants of the active pair."
  (interactive)
  (my--enable-theme (if (eq my--current-theme my--dark-theme)
                        my--light-theme my--dark-theme)))

(defun my--follow-system-appearance (&optional frame)
  "Theme the active pair on the first graphical FRAME, then stop watching.
Daemon/terminal startup has no frame to theme yet; this fires once one exists."
  (when (display-graphic-p frame)
    (my--theme-for-appearance (and (boundp 'ns-system-appearance) ns-system-appearance))
    (remove-hook 'after-make-frame-functions #'my--follow-system-appearance)))

(defun my--activate-pair (name)
  "Install (if needed), load, and make the rack pair NAME active.
`custom-available-themes' is the install gate: a recipe not yet activated this
session isn't on `load-path', so its themes are absent until `elpaca-try' lands
them. A graphical frame gets the variant for the live appearance and follows OS
flips; terminal/daemon startup defers to the first graphical frame."
  (pcase-let ((`(,light ,dark ,recipe) (cdr (assoc name my-theme-rack))))
    (when (and recipe (not (memq light (custom-available-themes))))
      (elpaca-try recipe)
      (elpaca-wait))
    (load-theme light t t)
    (load-theme dark  t t)
    (setq my--light-theme light my--dark-theme dark)
    (when (boundp 'ns-system-appearance)
      (add-hook 'ns-system-appearance-change-functions #'my--theme-for-appearance))
    (if (display-graphic-p)
        (my--theme-for-appearance (and (boundp 'ns-system-appearance) ns-system-appearance))
      (add-hook 'after-make-frame-functions #'my--follow-system-appearance))))

(defcustom my-theme-pair "Modus"
  "Name of the active light/dark pair, keyed into `my-theme-rack'.
`setopt' it to lazily install and activate that pair.

Set it with `setopt'/`setq' only — never `customize-set-variable', the
Customize UI, or a saved `custom-file'. Those record this option under
the `user' theme, and from then on every `enable-theme' recalculates it,
re-entering this `:set', which enables a theme, which recalculates… stack
overflow. `setopt' never touches the `user' theme, so activate-on-set is
safe — and the poisoning is sticky, surviving a later `setopt'."
  :type 'string
  :group 'faces
  :set (lambda (sym name) (set-default sym name) (my--activate-pair name)))

(keymap-global-set "C-c t" #'my/invert-theme)

;; defcustom doesn't run :set for its standard value, so kick the default by hand.
(my--activate-pair my-theme-pair)

(provide 'conf-theme)
