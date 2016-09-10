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

;; (add-hook 'message-mode-hook 'longlines-mode)

;; The following is based on my Gmail Gnus GPG Guide (GGGG):
;; https://github.com/kensanata/ggg#gmail-gnus-gpg-guide-gggg

(setq ;; You need to replace this email address with your own!
      user-mail-address "kensanata@gmail.com"
      ;; You need to replace this key ID with your own key ID!
      mml2015-signers '("4529A45C")
      ;; This tells Gnus to get email from Gmail via IMAP.
      gnus-select-method
      '(nnimap "gmail"
               ;; It could also be imap.googlemail.com if that's your server.
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl))
      ;; This tells Gnus to use the Gmail SMTP server. This
      ;; automatically leaves a copy in the Gmail Sent folder.
      smtpmail-smtp-server "smtp.gmail.com"
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
      mml2015-encrypt-to-self t)

;; Attempt to encrypt all the mails we'll be sending.
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

;; Add two key bindings for your Gmail experience.
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

(defun my-gnus-summary-keys ()
  (local-set-key "y" 'gmail-archive)
  (local-set-key "$" 'gmail-report-spam))

(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))
