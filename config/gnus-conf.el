;; prevent encoding errors while writes Gnus messages containing
;; Emoji characters
;; (add-to-list 'auto-coding-alist '("/#[^/]+#\\'" . utf-8))

;; Summary buffer format
(setq gnus-summary-line-format
      "%U%R%z %&user-date; %I%(%[%4L: %-15,15n%]%) %s\n"
      gnus-thread-indent-level 1
      gnus-user-date-format-alist '((t . " %Y-%m-%d")))
(setq nntp-open-connection-function 'nntp-open-plain-stream)
;; Completion of email adresses
(setq message-mail-alias-type 'ecomplete)

;; Always read the file
;; (setq gnus-always-read-dribble-file t)

(setq ;; You need to replace this email address with your own!
      user-mail-address "alex@alexschroeder.ch"
      ;; You need to replace this key ID with your own key ID!
      mml2015-signers '("0DD06A9B3268")
      ;; Set to '(nnnil) if you don't have local news!
      ;; Alternatively: '(nntp "localhost")
      gnus-select-method '(nnnil)
      ;; These are other news servers and IMAP accounts.
      gnus-secondary-select-methods
      '((nntp "cosmic.voyage"))
        ;; (nntp "news.tilde.club")
	;; (nnimap "migadu"
	;; 	(nnimap-user "alex@alexschroeder.ch")
	;;   	(nnimap-address "imap.migadu.com")
	;; 	(nnimap-server-port 993)
	;; 	(nnimap-stream ssl))
      
      ;; This tells Gnus to use the Migadu SMTP server.
      smtpmail-smtp-server "smtp.migadu.com"
      smtpmail-smtp-service 587
      ;; Tell message mode to use SMTP.
      message-send-mail-function 'smtpmail-send-it
      ;; This is where we store the password.
      nntp-authinfo-file "~/.authinfo.gpg"
      ;; We don't want local, unencrypted copies of emails we write.
      ;; gnus-message-archive-group "nnimap+migadu:Sent"
      ;; We want to be able to read the emails we wrote.
      mml-secure-openpgp-encrypt-to-self t)

;; Attempt to encrypt all the mails we'll be sending.
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)
