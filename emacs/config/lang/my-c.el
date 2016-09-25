(setq c-default-style "linux")
(setq c-hungry-delete-key t)

(after 'evil
  ;(evil-define-key 'insert c-mode-map (kbd "TAB") 'c-indent-line-or-region)
  (evil-define-key 'insert c-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)
)

(defun my-c-mode-setup ()
  (setq hide-ifdef-shadow t)
  (hide-ifdef-mode)
  ;; TODO: load in persistent hide-if database
)
(add-hook 'c-mode-common-hook 'my-c-mode-setup)

(provide 'my-c)
