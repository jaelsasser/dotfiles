(defun jae--setup-prog-mode ()
  (setq-local show-trailing-whitespace t)
  (toggle-truncate-lines 1))
(add-hook 'prog-mode-hook #'jae--setup-prog-mode)

(defun jae--large-file-hook ()
  "Turn off expensive functions (font-lock, undo-mode) for large files"
  (when (> (buffer-size) (* 1024 1024))
    (setq-local buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook #'jae--large-file-hook)

(defun move-line-up ()
  "Move the current line up"
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move the current line down"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(bind-keys ("C-c p" . move-line-up)
           ("C-c n" . move-line-down))

(use-package company
  :init (global-company-mode)
  :diminish company-mode
  :custom
  (company-idle-delay nil)		     ; only complete when asked (C-M-i, usually)
  (company-minimum-prefix-length 0)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  (company-backends '(company-capf company-dabbrev))
  :bind (([remap completion-at-point] . company-complete)
         ([remap complete-symbol] . company-complete)
         :map company-active-map
         ("C-w" . nil)
         ("M-." . company-show-location)))
(use-package company-box :disabled
  :hook (company-mode . company-box-mode))

(use-package goto-chg
  :bind ("M-]" . goto-last-change))

(use-package evil :pin melpa-stable :demand t
  :hook (after-init . evil-mode)
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

;; TODO: track upstream, eventually integrate
(use-package targets.el :disabled
  :after evil)

(provide 'conf-editor)
