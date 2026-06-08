;;; my-agent.el --- Claude Code + Monet (ghostel backend) -*- lexical-binding: t; -*-

;; ghostel: libghostty terminal backend. The native module is NOT built here — it
;; auto-downloads a prebuilt binary on first `claude-code' launch (one-time prompt,
;; ghostel-module-auto-install defaults to `ask').
(use-package ghostel)

;; monet: Claude Code IDE-protocol server. Not on MELPA -> explicit recipe.
(use-package monet
  :ensure (monet :host github :repo "stevemolitor/monet"))

;; claude-code.el. MELPA's `claude-code' is a DIFFERENT package
;; (yuya373/claude-code-emacs); pin the repo and `:inherit nil' to ignore that menu.
(use-package claude-code
  :ensure (claude-code :inherit nil :type git :host github
                       :repo "stevemolitor/claude-code.el"
                       :files (:defaults (:exclude "images/*")))
  :after monet
  :custom
  (claude-code-terminal-backend 'ghostel)
  :init
  (add-hook 'claude-code-process-environment-functions
            #'monet-start-server-function)
  :config
  (monet-mode 1)
  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))

(provide 'my-agent)
