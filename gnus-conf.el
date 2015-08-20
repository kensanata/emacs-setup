;; prevent encoding errors while writing Gnus messages containing
;; Emoji characters
(add-to-list 'auto-coding-alist '("/#[^/]+#\\'" . utf-8))

;; (add-to-list 'Info-default-directory-list (expand-file-name "~/src/gnus/texi/"))
;; (add-to-list 'load-path (expand-file-name "~/src/gnus/lisp"));; beware the bleeding edge
;; (require 'gnus-load)

(setq gnus-select-method
      '(nnimap "raspberrypi"
	       (nnimap-address "mail.alexschroeder.ch")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl))
      nntp-authinfo-file "~/.authinfo.gpg"
      gnus-agent nil;; that seems to confuse nnimap
      gnus-summary-line-format
      "%U%R%z %&user-date; %I%(%[%4L: %-15,15n%]%) %s\n"
      gnus-thread-indent-level 1
      gnus-user-date-format-alist '((t . " %Y-%m-%d"))
      ;; Unnecessary when using Gmail
      ;; gnus-message-archive-group "nnml+mail:mail.misc"
      gnus-message-archive-group nil
      gnus-gcc-mark-as-read t
      mml2015-encrypt-to-self t
      mml2015-signers '("4529A45C"))

(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

(defun my-gnus-summary-keys ()
  (local-set-key "e" 'gmail-archive)
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

;; ManageSieve says 2000 is deprecated
;; uploading the sieve doesn't work with STARTTLS?
;; (setq sieve-manage-default-port 4190)

(setq gnus-spotlight-mail-spool "/Volumes/Extern/Archives/Mail")

;; If search finds nothing, perhaps you didn't have /Volumnes/Extern
;; mounted when you started Gnus and now your table is borked. Call
;; M-x gnus-spotlight/make-directory-table to fix it.

(eval-after-load "gnus"
  '(progn
     (require 'gnus-spotlight)
     (gnus-spotlight-insinuate)))

(defun read-until-not-in-mail-misc ()
  "Start reading messages in a Gnus Summary buffer
until the article is not available in nnml+mail:mail.misc.
You could call this from nnimap+imap.gmail.com:[Gmail]/All Mail, for example."
  (interactive)
  (find-in-mail-misc); error if not found
  (while (and (forward-line) (not (looking-at "$")))
    (find-in-mail-misc)))

(defun find-in-mail-misc ()
  "Figure out if this mail is also available in nnml+mail:mail.misc.
Call from a Gnus Summary buffer."
  (interactive)
  (let (id number)
    (gnus-summary-select-article)
    (gnus-summary-toggle-header 1)
    (gnus-with-article-headers
      (message-narrow-to-head)
      (goto-char (point-min))
      (if (re-search-forward "^Message-ID: *\\(.*\\)" nil t)
	  (setq id (match-string 1))
	(error "Unable to find Message-ID header")))
    (gnus-summary-toggle-header -1)
    (with-current-buffer (get-buffer-create " *nnml id*")
      (setq number (nnml-find-id "mail.misc" id "nnml+mail")))
    (if number
	(message "Found %d" number)
      (case (read-char "Not found in mail.misc: (e)xpire, (m)ove there, (i)gnore or (q)uit? ")
	((?e) (gnus-summary-mark-as-expirable 1)
	 ;; avoid skipping a line...
	 (forward-line -1))
	((?i) (message "Ignoring..."))
	((?m) (gnus-summary-move-article 1 "nnml+mail:mail.misc"))
	((?q) (error "Quit"))))))

;;; Mail writing, sending, SMTP
(setq mail-user-agent 'gnus-user-agent
      message-mail-alias-type 'ecomplete
      ;; obsolete -- using `gnus-message-archive-group' instead
      ;; gnus-outgoing-message-group "nnimap+imap.gmail.com:[Gmail]/Sent Mail"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      ;; user-mail-address "alex@gnu.org"
      user-mail-address "kensanata@gmail.com"
      ;; smtpmail-default-smtp-server "fencepost.gnu.org"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      ;; smtpmail-smtp-server "smtp.gmail.com"
      ;; smtpmail-smtp-server "fencepost.gnu.org"
      ;; requires an entry in ~/.authinfo.gpg
      ;; machine fencepost.gnu.org login alex password "*secret*" port 25
      ;; using smtpmail-smtp-server "alexschroeder.ch"
      ;; results in 550 relay not permitted
      smtpmail-starttls-credentials 'starttls
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; (add-hook 'message-mode-hook 'longlines-mode)
