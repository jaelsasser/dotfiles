;; IRC + Chat
(use-package erc :ensure nil :defer t :disabled
  :init
  (defun erc-switch-buffers-or-start ()
    "Connect to Snoonet/Freenode"
    (interactive)
    (defvar jae--erc-connected nil
      "Set to `t' after this emacs instance has connected to IRC")
    (when (and (not jae--erc-connected) (y-or-n-p "Connect to IRC?"))
      (erc-tls :server "jungle.fcker.ca" :port "6697" :nick "snoonet")
      (erc-tls :server "jungle.fcker.ca" :port "6697" :nick "freenode")
      (setq jae--erc-connected t))
    (counsel-erc))

  (defvar counsel-erc-map (make-sparse-keymap))
  (ivy-set-actions
   'counsel-erc
   '(("k"
      (lambda (x)
        (kill-buffer x)
        (ivy--reset-state ivy-last))
      "kill")
     ("j"
      ivy--switch-buffer-other-window-action
      "other window")))

  (defun counsel-erc ()
    "Quickly switch to between `erc' buffers"
    (interactive)
    (ivy-read "ERC: " (mapcar #'buffer-name (erc-buffer-list))
              :require-match t
              :action #'switch-to-buffer
              :keymap ivy-switch-buffer-map
              :sort t
              :caller 'counsel-erc))

  :config
  (setq erc-prompt-for-password nil
        erc-network-hide-list '(("card.freenode" "JOIN" "PART" "QUIT"))
        erc-button-nickname-face 'erc-nick-default-face

        erc-fill-function 'erc-fill-variable
        erc-fill-prefix "        "

        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-timestamp-format "[%H:%M] "
        erc-timestamp-only-if-changed-flag t
        erc-prompt ">>>"

        erc-format-nick-function 'erc-format-@nick
        erc-join-buffer 'bury

        erc-button-buttonize-nicks nil
        erc-button-wrap-long-urls t

        erc-modules '(netsplit
                      fill
                      button
                      capab-identify
                      completion
                      irccontrols
                      readonly
                      ring
                      move-to-prompt
                      stamp
                      scrolltobottom
                      truncate)
        erc-input-line-position -1

        erc-interpret-mirc-color t
        erc-rename-buffers t
        erc-kill-buffer-on-part t)

  :bind (("C-c i" . erc-switch-buffers-or-start)
         :map erc-mode-map
         ("<C-return>" . erc-send-current-line)
         ("RET" . nil)
         ("C-c TAB" . nil)))
