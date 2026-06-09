;;; my-agent.el --- Claude Code + Monet (ghostel backend) -*- lexical-binding: t; -*-

;; ghostel: libghostty terminal backend
(use-package ghostel :defer t)

;; monet: Claude Code IDE-protocol server. Not on MELPA -> explicit recipe.
(use-package monet
  :ensure (monet :host github :repo "stevemolitor/monet")
  :hook (after-init . monet-mode))

;; claude-code.el. MELPA's `claude-code' is a DIFFERENT package
(use-package claude-code
  :ensure (claude-code :inherit nil :type git :host github
                       :repo "stevemolitor/claude-code.el"
                       :files (:defaults (:exclude "images/*")))
  :defer t
  :bind-keymap ("C-c c" . claude-code-command-map)
  :custom
  (claude-code-terminal-backend 'ghostel)
  :config
  (add-hook 'claude-code-process-environment-functions
            #'monet-start-server-function)
  (claude-code-mode))

(provide 'my-agent)
