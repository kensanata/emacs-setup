;; I use aerc to read mail and emacsclient as my mail editor
(server-start)

(define-derived-mode eml-mode mail-mode "Email"
  "Write emails via emacsclient."
  (when (buffer-file-name)
    (set (make-local-variable 'server-temp-file-regexp) ""))
  (local-set-key (kbd "C-c C-c") 'server-edit)
  (auto-fill-mode 1))

(add-to-list 'auto-mode-alist '("\\.eml\\'" . eml-mode))

;; I also like RMAIL.
;; See cadadr's blog post: https://cadadr.dreamwidth.org/828.html
(setq
 ;; where I read mail
 rmail-file-name "/home/alex/mail/current"
 ;; where the other mbox files are
 rmail-secondary-file-directory "/home/alex/mail"
 ;; the interesting other folders (without archive-001 and so on)
 rmail-secondary-file-regexp "spam\\|sent\\|archive$"
 ;; where mail I want to keep goes
 rmail-default-file "/home/alex/mail/archive"
 ;; I don't differentiate between spam and trash anymore because I'm
 ;; relying on my provider's spam filter. I'm not running a local spam
 ;; filter.
 rmail-trash-file "/home/alex/mail/trash"
 ;; delete mail when moving it
 rmail-delete-after-output t)

(defvar rmail-summary-font-lock-keywords
  '(("^.....D.*" . font-lock-comment-face)                ;; deleted
    ("^.....-.*" . 'bold)                                 ;; Unread
    ;; Neither of the below will be highlighted if either of the above are:
    ("^.....[^D-] \\(......\\)" 1 font-lock-keyword-face) ;; Date.
    ("{ \\([^\n}]+\\) }" 1 font-lock-comment-face))	  ;; Labels.
  "Additional expressions to highlight in Rmail Summary mode.")

(eval-after-load "rmail"
  '(progn
     (define-key rmail-mode-map "<" 'asc:fetch-mail)
     (define-key rmail-mode-map "A" 'asc:archive-mail)
     (define-key rmail-mode-map "d" 'asc:trash-mail)
     (define-key rmail-mode-map "$" 'asc:trash-mail)
     (define-key rmail-mode-map "S" 'mairix-search)))

(eval-after-load "message"
  '(progn
     (define-key message-mode-map (kbd "C-c C-c") 'message-send)))

(add-hook 'message-mode-hook
	  'asc:message-mode-init)

(defun asc:message-mode-init ()
  (typo-mode 1)
  (english))

;; Redefining quit
(eval-after-load "rmailsum"
  '(progn
     (define-key rmail-mode-map "<" 'asc:fetch-mail)
     (define-key rmail-summary-mode-map "A" 'asc:archive-mail)
     (define-key rmail-summary-mode-map "d" 'asc:trash-mail)
     (define-key rmail-summary-mode-map "$" 'asc:trash-mail)
     (define-key rmail-summary-mode-map "S" 'mairix-search)))

(global-set-key (kbd "C-c <") 'asc:fetch-mail)

(defun asc:fetch-mail ()
  "Run mail retrieval scripts."
  (interactive)
  (make-process
   :name "fetch-mail" :buffer (get-buffer-create "*Fetch Mail*")
   :command (list "mpop" "-Q" "-a")
   :sentinel
   (lambda (process event)
     (unless (process-live-p process)
       (when (zerop (process-exit-status process))
         (if (> (file-attribute-size
		 (file-attributes
		  (concat rmail-spool-directory (user-login-name))))
		0)
	     (message "New mail!")
	   (message "No new mail.")))))))

(defun asc:archive-mail ()
  "It will move the current message to ‘rmail-default-file’
This command will not run unless in an RMAIL buffer visiting
‘rmail-file-name’."
  (interactive)
  (rmail-output rmail-default-file))

(defun asc:trash-mail ()
  "It will move the current message to ‘rmail-trash-file’
If you're currently visiting the trash file, then it will be
deleted."
  (interactive)
  (if (or (string= (buffer-file-name)
		       rmail-trash-file)
	      (and rmail-buffer
		   (string= (buffer-file-name rmail-buffer)
			    rmail-trash-file)))
      (rmail-delete-forward)
    (rmail-output rmail-trash-file)))

(setq mairix-file-path "/home/alex/mail/mairix"
      mairix-search-file "search")

(autoload 'mairix-search "mairix" "Call Mairix with SEARCH.")

(require 'cl-lib)

(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-envelope-from 'header
      sendmail-program "msmtp"
      message-default-headers "Fcc: /home/alex/mail/sent"
      message-auto-save-directory "/home/alex/mail/drafts"
      message-confirm-send t
      message-hidden-headers '("^References:" "^X-Draft-From:" "^In-Reply-To:"))

(defun asc:ecomplete-add-mail (email expansion)
  "Add EMAIL with EXPANSION for future mail completion."
  (interactive (list (read-string "Email address: ")
		     (read-string "Expansion: ")))
  (ecomplete-add-item email 'mail expansion))

(defun asc:ecomplete-pick-item (prompt type)
  "Choose an item of TYPE."
  (let* ((elems (cdr (assq type ecomplete-database))))
    (completing-read prompt elems)))

(defun asc:ecomplete-add-mail-group (email recipients)
  "Add EMAIL with RECIPIENTS for future mail completion.
RECIPIENTS is a list of email addresses"
  (interactive (list (read-string "Email address: ")
		     (let (recipient recipients)
		       (while (not (string= "" (setq recipient (asc:ecomplete-pick-item "Email: " 'mail))))
			 (push recipient recipients))
		       recipients)))
  (ecomplete-add-item email 'mail (string-join recipients ", ")))

(defun asc:rmail-fixup-mime-multipart ()
  "Fix up decryption of a MIME multipart message.

If you've used `rmail-epa-decrypt' and the decrypted section
turns out to be a MIME multipart message (i.e. OpenPGP/MIME),
then the display it messed up: the new multipart content-type
header just sits there alone in the middle of a part, and
everything belonging to this multipart message is not decoded
correctly. This function fixes the situation."
  (interactive)
  (rmail-edit-current-message)
  (when (search-forward "\n\nContent-Type: multipart" nil t)
    (let ((multipart-start (line-beginning-position)))
      (re-search-backward "^--.*\n")
      (delete-region (match-end 0) multipart-start)
      (insert "Content-Description: encrypted message\n")
      (rmail-cease-edit))))
