;; Code Completion
(use-package company :ensure t
  :init (global-company-mode)
  :diminish company-mode
  :config
  (setq company-idle-delay nil		; only complete when asked (C-M-i, usually)
        company-minimum-prefix-length 0
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-backends '(company-capf company-dabbrev))
  :bind (([remap completion-at-point] . company-complete)
         ([remap complete-symbol] . company-complete)
         :map company-active-map
         ("C-w" . nil)
         ("M-." . company-show-location)))

(use-package evil :ensure t :pin melpa
  :init
  (setq evil-default-state 'insert
	evil-disable-insert-state-bindings t
        evil-toggle-key "C-\\"

        evil-echo-state t
        evil-mode-line-format nil
        evil-want-C-u-scroll nil
        evil-want-C-i-jump nil

        evil-highlight-closing-paren-at-point-states ()
        evil-move-beyond-eol t
        evil-track-eol nil

        evil-search-module 'isearch
        evil-magic 'very
        evil-want-fine-undo nil)

  ;; extra motions & operations
  (use-package evil-easymotion :ensure t :after evil
    :init (evilem-default-keybindings "SPC"))
  (use-package evil-smartparens :ensure t :after evil
    :diminish evil-smartparens-mode
    :init (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  (use-package evil-snipe :ensure t :after evil
    :diminish evil-snipe-local-mode
    :init (evil-snipe-mode t)
    :config
    (setq evil-snipe-scope 'visible
          evil-snipe-use-vim-sneak-bindings t)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode))
  (use-package evil-surround :ensure t :after evil
    :init (global-evil-surround-mode t))

  ;; extra textobjects
  (use-package evil-args :ensure t :after evil
    :bind (:map evil-inner-text-objects-map ("a" . evil-inner-arg)
           :map evil-outer-text-objects-map ("a" . evil-outer-map)))
  (use-package evil-indent-plus :ensure t :after evil
    :init (evil-indent-plus-default-bindings))

  ;; TODO: track upstream, eventually integrate
  (use-package targets.el :ensure nil :disabled)

  :config
  (evil-mode t)

  ;; Avoid overriding default Emacs key chords
  :bind
  (("M-[" . evil-normal-state)
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

(use-package goto-chg :ensure t
  :bind ("M-]" . goto-last-change))

(provide 'conf-editor)
