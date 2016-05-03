(use-package popwin
  :ensure t
  :config (popwin-mode 1)
)

(use-package flx-ido
  :ensure t
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t
          ido-use-faces nil
          )
    )
)

;; (use-package ag
;;   :ensure t
;;   :config
;;   (progn
;;     (setq ag-highlight-search t)
;;     (push '("\\*ag search.*\\*" :height 25 :regexp t) popwin:special-display-config)
;;     )
;; )

(provide 'my-buffers)
