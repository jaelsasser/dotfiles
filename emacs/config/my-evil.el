(use-package key-chord
  :ensure key-chord
  :config
  (progn
    (key-chord-mode 1)
    )
)

(use-package evil-leader
  :ensure t
  :config
  (progn
    (evil-leader/set-leader "SPC")
    (global-evil-leader-mode t)

    (evil-leader/set-key "b" 'switch-to-buffer
      "x" 'helm-M-x
      "/" 'helm-swoop
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

    (setq evil-want-C-u-scroll t
          evil-want-C-w-in-emacs-state t
          evil-want-C-w-delete t
          evil-want-fine-undo t
          evil-want-change-word-to-end t
          )
    (global-set-key (kbd "C-q") 'universal-argument)

    ;; (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
    ;; (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
    ;; (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
    ;; (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)

    (define-key evil-motion-state-map "h" 'evil-backward-char)
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
    (define-key evil-motion-state-map "l" 'evil-forward-char)
    (define-key evil-motion-state-map "$" 'evil-end-of-line)
    (define-key evil-motion-state-map "0" 'evil-beginning-of-line)

    (define-key evil-normal-state-map "/"           'evil-search-forward)
    (define-key evil-motion-state-map "/"           'evil-search-forward)
    (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

    (define-key evil-insert-state-map (kbd "C-c") 'evil-escape)
    (define-key evil-normal-state-map (kbd "C-z") 'suspend-emacs)
  )
)

(use-package evil-commentary
  :ensure t
  :config
  (progn
    (evil-commentary-mode t)
  )
)

(use-package evil-matchit
  :ensure t
  :config
  (progn
    (global-evil-matchit-mode t)
    )
  )


(use-package evil-surround
  :ensure t
  :config
  (progn
    (global-evil-surround-mode t)
    )
  )

;; (use-package evil-search-highlight-persist
;;   :ensure t
;;   :config
;;   (progn
;;     (global-evil-search-highlight-persist t)
;;     (global-set-key (kbd "C-l") 'evil-search-highlight-persist-remove-all)
;;     )
;;  )

(provide 'my-evil)
