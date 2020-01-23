;; prevent encoding errors while writing Gnus messages containing
;; Emoji characters
(add-to-list 'auto-coding-alist '("/#[^/]+#\\'" . utf-8))

;; Summary buffer format
(setq gnus-summary-line-format
      "%U%R%z %&user-date; %I%(%[%4L: %-15,15n%]%) %s\n"
      gnus-thread-indent-level 1
      gnus-user-date-format-alist '((t . " %Y-%m-%d")))

;; Completion of email adresses
(setq message-mail-alias-type 'ecomplete)

;; Always read the file
(setq gnus-always-read-dribble-file t)

;; (add-hook 'message-mode-hook 'longlines-mode)

;; The following is based on my Gmail Gnus GPG Guide (GGGG):
;; https://github.com/kensanata/ggg#gmail-gnus-gpg-guide-gggg

(setq ;; You need to replace this email address with your own!
      ;; user-mail-address "kensanata@gmail.com"
      user-mail-address "alex@alexschroeder.ch"
      ;; You need to replace this key ID with your own key ID!
      mml2015-signers '("ACECFEAE")
      ;; Gnus has a "primary" server and a ton of secondary servers.
      ;; These days such an idea seems outdated and so we use just the
      ;; secondary select methods and all of them are configured the
      ;; same way. If I don't set this select method, Gnus will
      ;; attempt to fetch local news from somewhere but I don't have
      ;; access to news. That's why we set it to the "nil" method.
      gnus-select-method '(nnnil)
      ;; These are now all the IMAP accounts.
      gnus-secondary-select-methods
      '((nnimap "migadu"
		(nnimap-user "alex@alexschroeder.ch")
	  	(nnimap-address "imap.migadu.com")
		(nnimap-server-port 993)
		(nnimap-stream ssl))
	(nnimap "notifications"
		(nnimap-user "notifications@alexschroeder.ch")
		(nnimap-address "imap.migadu.com")
		(nnimap-server-port 993)
		(nnimap-stream ssl))
	(nnimap "gmail"
		;; It could also be imap.googlemail.com if that's your
		;; server. The nnimap-user is determined by the fact
		;; that there is no other matching entry in the
		;; ~/.authinfo.gpg file
		(nnimap-address "imap.gmail.com")
		(nnimap-server-port 993)
		(nnimap-stream ssl)))
      ;; This tells Gnus to use the Migadu SMTP server.
      smtpmail-smtp-server "smtp.migadu.com"
      smtpmail-smtp-service 587
      ;; Tell message mode to use SMTP.
      message-send-mail-function 'smtpmail-send-it
      ;; This is where we store the password.
      nntp-authinfo-file "~/.authinfo.gpg"
      ;; Gmail system labels have the prefix [Gmail], which matches
      ;; the default value of gnus-ignored-newsgroups. That's why we
      ;; redefine it.
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
      ;; The agent seems to confuse nnimap, therefore we'll disable it.
      gnus-agent nil
      ;; We don't want local, unencrypted copies of emails we write.
      gnus-message-archive-group nil
      ;; We want to be able to read the emails we wrote.
      mml-secure-openpgp-encrypt-to-self t)

;; Attempt to encrypt all the mails we'll be sending.
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

;; Add two key bindings for your Gmail experience.
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

(defun my-gnus-summary-keys ()
  (local-set-key "y" 'asc:archive)
  (local-set-key "d" 'asc:trash)
  (local-set-key "$" 'asc:report-spam))

(defun asc:archive ()
  "Archive the current or marked mails.
This moves them into the (non-Gmail) Archive folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+migadu:Archive"))

(defun asc:report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Junk or Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+migadu:Junk"))

(defun asc:trash ()
  "Delete the current or marked mails.
This moves them into the Trash folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+migadu:Trash"))
