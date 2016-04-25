(use-package evil-leader
  :ensure t
  :config
  (progn
    (evil-leader/set-leader ",")
    (global-evil-leader-mode t)

    (evil-leader/set-key "b" 'switch-to-buffer
      "x" 'helm-M-x
      "q" 'kill-buffer-and-window
      "v" 'split-window-right
      )
    )
  )

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config
  (progn
    (setq evil-search-module 'evil-search)
    (setq evil-magic 'very-magic)

    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-w-in-emacs-state t)

    (setq evil-want-fine-undo t)
    (setq evil-want-change-word-to-end t)

    (use-package key-chord
      :ensure key-chord
      :config
      (progn
        (key-chord-mode 1)
        )
      )

    (define-key evil-normal-state-map (kbd "SPC a") 'ag)
    (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)

    (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)

    (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)

    (define-key evil-motion-state-map "h" 'evil-backward-char)
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
    (define-key evil-motion-state-map "l" 'evil-forward-char)
    (define-key evil-motion-state-map "$" 'evil-end-of-line)
    (define-key evil-motion-state-map "0" 'evil-beginning-of-line)

    (define-key evil-normal-state-map "/"           'evil-search-forward)
    (define-key evil-normal-state-map (kbd "SPC /") 'helm-swoop)
    (define-key evil-motion-state-map "/"           'evil-search-forward)
    (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

  )
)

(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode t)
)

; ;; escape maps
; (evil-escape-mode t)
; (setq-default evil-escape-inhibit t)
(define-key evil-insert-state-map (kbd "C-c") 'evil-escape)
(define-key evil-normal-state-map (kbd "C-z") 'suspend-emacs)

; ;; persistent search hl
; (global-evil-search-highlight-persist t)

(provide 'my-evil)
