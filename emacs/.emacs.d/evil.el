(use-pacakge 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "b" 'switch-to-buffer)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "q" 'kill-buffer-and-window)
(evil-leader/set-key "v" 'split-window-right)

(require 'evil)
(evil-mode t)

(setq evil-want-C-u-scroll t) ; fix C-u scrolling for evil
(setq evil-want-C-i-jump nil) ; fix TAB for evil

;; escape maps
(evil-escape-mode t)
(setq-default evil-escape-inhibit t)
(define-key evil-insert-state-map (kbd "C-c") 'evil-escape)
(define-key evil-visual-state-map (kbd "C-c") 'evil-escape)
(define-key evil-normal-state-map (kbd "C-z") 'suspend-emacs)

;; vi undo
(global-undo-tree-mode -1)
(setq evil-want-fine-undo t)

;; persistent search hl
(global-evil-search-highlight-persist t)

