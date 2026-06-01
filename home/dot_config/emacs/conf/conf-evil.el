;;; conf-evil.el --- evil-mode and friends -*- lexical-binding: t; -*-

(use-package evil :demand t
  :custom
  (evil-default-state 'insert)
  (evil-disable-insert-state-bindings t)
  (evil-toggle-key "C-\\")

  (evil-echo-state t)
  (evil-mode-line-format nil)
  (evil-want-C-u-scroll nil)
  (evil-want-C-i-jump nil)

  (evil-highlight-closing-paren-at-point-states ())
  (evil-move-beyond-eol t)
  (evil-track-eol nil)

  (evil-search-module 'isearch)
  (evil-magic 'very)
  (evil-want-fine-undo nil)

  :config
  (evil-update-insert-state-bindings :force :remove)
  (evil-set-initial-state 'special-mode 'emacs)

  (evil-mode 1)

  ;; Avoid overriding default Emacs key chords
  ;; TODO: upstream this as a defcustom
  :bind (("M-[" . evil-normal-state)
         :map evil-normal-state-map
         ("C-r" . nil)
         ("C-n" . nil)
         ("C-p" . nil)
         ("C-t" . nil)
         ("C-." . nil)
         ("M-." . nil)
         :map evil-motion-state-map
         ("C-\\" . evil-emacs-state)
         ("C-b" . nil)
         ("C-d" . nil)
         ("C-e" . nil)
         ("C-f" . nil)
         ("C-o" . nil)
         ("C-y" . nil)
         ("C-]" . nil)
         ("C-w" . nil)
         ("C-v" . nil)))

;; extra motions & operations
(use-package evil-easymotion
  :after evil
  :init (evilem-default-keybindings "SPC"))

(use-package evil-smartparens
  :after (evil smartparens)
  :diminish evil-smartparens-mode
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package evil-snipe
  :after evil
  :diminish evil-snipe-local-mode
  :init
  (evil-snipe-mode t)
  :hook (magit-mode . turn-off-evil-snipe-mode)
  :custom
  (evil-snipe-scope 'visible)
  (evil-snipe-use-vim-sneak-bindings t))

(use-package evil-surround
  :after evil
  :init (global-evil-surround-mode t))

;; extra textobjects
(use-package evil-args
  :after evil
  :bind (:map evil-inner-text-objects-map ("a" . evil-inner-arg)
         :map evil-outer-text-objects-map ("a" . evil-outer-map)))

(use-package evil-indent-plus
  :after evil
  :init (evil-indent-plus-default-bindings))

(provide 'conf-evil)
