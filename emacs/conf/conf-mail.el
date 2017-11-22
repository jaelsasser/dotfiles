;; Mail
(use-package mu4e :ensure nil :disabled
  :config
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-maildir "~/.local/mail"
        mu4e-attachments-dir "~/Downloads/Attachments"

        mu4e-completing-read-function 'ivy-completing-read
        mu4e-sent-messages-behavior 'delete
        mu4e-change-filenames-when-moving t
        message-kill-buffer-on-exit t

        mu4e-get-mail-command "mbsync -V -c ~/.config/mail/mbsyncrc gmail"

        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent"
        mu4e-trash-folder  "/Trash"
        mu4e-refile-folder "/All"

        mu4e-bookmarks
        '(("flag:unread AND NOT flag:trashed AND NOT m:/Lists/*" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("m:/Lists/.Emacs/.devel" "emacs-devel" ?e))

        mu4e-maildir-shortcuts
        '(("/Inbox" . ?i)
          ("/Sent" . ?s)
          ("/Trash" . ?t)
          ("/All" . ?a)))
  :bind ("C-c m" . mu4e))
