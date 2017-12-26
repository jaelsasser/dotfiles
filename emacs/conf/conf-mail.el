(require 'bootstrap)

(use-package gnus :ensure nil
  :commands gnus
  :hook (gnus-group-mode . gnus-topic-mode)
  :custom
  (gnus-init-file (user-emacs-file "gnus.el"))
  (gnus-startup-file (user-emacs-file "newsrc"))
  ;; use gnus-cloud to sync private config, setup over IMAP
  ;; TODO: unbreak (assumes that my personal email is set up)
  (gnus-cloud-method "personal")
  (gnus-cloud-synced-files `(,gnus-init-file
                             ,gnus-startup-file))
  ;; personal/work Google Apps accounts
  (gnus-select-method
   '(nnimap "personal"
            (nnimap-address "imap.googlemail.com")
            (nnimap-server-port "imaps")
            (nnimap-stresm ssl)))
  (gnus-secondary-select-methods
   '((nnimap "work"
             (nnimap-address "imap.googlemail.com")
             (nnimap-server-port "imaps")
             (nnimap-stresm ssl))))

  (gnus-check-new-newsgroups nil)
  (gnus-use-cache t)

  ;; respect gnus--group-name-maps
  (gnus-group-line-format
   "%M\ %S\ %p\ %P\ %5y:%B%(%uG%)\n")

  (gnus-always-read-dribble-file t)
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)

  :config
  (setq
   gnus--group-name-map
   '(("INBOX" . "Inbox")
     ("[Gmail]/Starred" . "Starred")
     ("[Gmail]/Sent Mail" . "Sent")
     ("[Gmail]/Important" . "Important")
     ))
  (defun gnus-user-format-function-G (arg)
    (let ((mapped-name (assoc gnus-tmp-group gnus--group-name-map)))
      (if (null mapped-name)
          gnus-tmp-group
        (cdr mapped-name)))))

(use-package smtpmail :ensure nil
  :commands smtpmail-send-it
  :custom
  (smtpmail-smtp-server "smtp.googlemail.com")
  (smtpmail-smtp-service 465))

(provide 'conf-mail)
