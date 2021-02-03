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
 ;; where mail comes from, delivered by mpop
 rmail-primary-inbox-list '("/home/alex/mail/inbox")
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
 rmail-trash-file "/home/alex/mail/trash")

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
     (define-key rmail-mode-map "S" 'mairix-search)
     (define-key rmail-mode-map (kbd "<tab>") 'asc:rmail-forward-link-or-button)
     (define-key rmail-mode-map (kbd "<backtab>") 'asc:rmail-backward-link-or-button)))

(eval-after-load "message"
  '(progn
     (define-key message-mode-map (kbd "C-c C-c") 'message-send)))

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
         (if (> (file-attribute-size (file-attributes (car rmail-primary-inbox-list))) 0)
	     (message "New mail!")
	   (message "No new mail.")))))))

(defun asc:archive-mail ()
  "It will move the current message to ‘rmail-default-file’
This command will not run unless in an RMAIL buffer visiting
‘rmail-file-name’."
  (interactive)
  (unless (string= (buffer-file-name) rmail-file-name)
    (user-error
     "This is not your default RMAIL file"))
  (rmail-output rmail-default-file)
  (rmail-delete-forward))

(defun asc:trash-mail ()
  "It will move the current message to ‘rmail-trash-file’
If you're currently visiting the trash file, then it will be
deleted."
  (interactive)
  (unless (string= (buffer-file-name) rmail-trash-file)
    (rmail-output rmail-trash-file))
  (rmail-delete-forward))

(setq mairix-file-path "/home/alex/mail/mairix"
      mairix-search-file "search")

(autoload 'mairix-search "mairix" "Call Mairix with SEARCH.")

(require 'cl-lib)

(defun asc:rmail-forward-link-or-button (p)
  "Navigate both links and buttons in Rmail in a ring.
This replaces the use of ‘forward-button’ which only traverses
buttons and skips over links."
  (interactive (list (point)))
  (let (positions)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (memq (car (overlay-properties overlay))
                  '(goto-address button))
        (pushnew (overlay-start overlay) positions)))
    (setq positions (sort positions #'<))
    (if (>= p (car (last positions)))
        (goto-char (first positions))
      (goto-char (find-if (lambda (x) (> x p)) positions)))))

(defun asc:rmail-backward-link-or-button (p)
  "Navigate both links and buttons in Rmail in a ring.
This replaces the use of ‘forward-button’ which only traverses
buttons and skips over links.
This is the reverse counterpart of
‘gk-rmail-forward-link-or-button’."
  (interactive (list (point)))
  (let (positions)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (memq (car (overlay-properties overlay))
                  '(goto-address button))
        (pushnew (overlay-start overlay) positions)))
    (setq positions (sort positions #'<))
    (if (<= p (first positions))
        (goto-char (car (last positions)))
      (goto-char (find-if (lambda (x) (< x p)) positions :from-end t)))))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-envelope-from 'header
      sendmail-program "msmtp"
      message-default-headers "Fcc: /home/alex/mail/sent"
      message-auto-save-directory "/home/alex/mail/drafts"
      message-confirm-send t
      message-hidden-headers '("^References:" "^X-Draft-From:" "^In-Reply-To:"))

(add-hook 'message-sent-hook 'bury-buffer)
