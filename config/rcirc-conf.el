;;; rcirc, write such as not to require rcirc at startup
;; (autoload 'rcirc "~/src/emacs/lisp/net/rcirc" t)
(require 'rcirc-emojis)

(use-package rcirc-menu :after rcirc)
(use-package rcirc-styles :after rcirc)

(global-set-key (kbd "C-c e") 'asc:rcirc-start)

(defun asc:rcirc-start ()
  "Start `rcirc'."
  (interactive)
  (rcirc nil)
  (rcirc-menu))

(setq rcirc-prompt "%n> "; list nick
      rcirc-fill-prefix "    "
      rcirc-fill-column 79; side-by-side on my laptop
      rcirc-fill-column 65; using a large font
      rcirc-max-message-length 0; live dangerously!
      rcirc-default-nick "kensanata"
      rcirc-keywords '("ken" "kens" "kensa" "alex"; IRC
		       "Alex_Schroeder"; Discord
		       )
      rcirc-nick-prefix-chars "~&@%+!"
      rcirc-timeout-seconds 1200
      rcirc-authinfo (with-temp-buffer
		       (when (file-readable-p "~/.rcirc-authinfo")
			 (insert-file-contents-literally "~/.rcirc-authinfo")
			 (read (current-buffer))))
      rcirc-server-alist
      ;; host chat.freenode.net but see https://alexschroeder.ch/wiki/2017-07-15_Freenode_IPv6
      ;; sometimes we have to use 71.11.84.232
      ;; port 6697 7000 7070 according to http://freenode.net/kb/answer/chat
      '(("chat.freenode.net"
	 :port 6697 :encryption tls
	 :channels ("#emacs"
		    "#emacs-ops"
		    "#org-mode"
		    "#rcirc"
		    "#wiki"
		    "#oddmuse"
		    "##emacs.de"
		    "#perl"
		    "#xelf"
		    "#raku"
		    "#bussard"
		    "#fennel"
		    "#jsoftware"))
	;; SDF gopher moved to tilde.chat
	;; ("irc.sdf.org" :channels ("#gopher"))
	;; Tilde chat is often unreachable
	("irc.tilde.chat"
	 :port 6697 :encryption tls
	 :channels ())
	;; "#gopher" "#cosmic"))
	;; ("flame.de.eu.darkmyst.org"
	;;  :port 6697 :encryption tls
	;;  :channels ("#rpg-hub"))
	;; ("campaignwiki.org"
	;;  :port 6697 :encryption tls
	;;  :channels ("#chat"))
	("irc.oftc.net"
	 :port 6697 :encryption tls
	 :channels ("#bitlbee"))
	;; ("irc.gitter.im" :port 6697 :encryption tls
	;;  :password ,(nth 3 (assoc "gitter" rcirc-authinfo))
	;;  :channels ("#kensanata/elisp"
	;; 	    "#kensanata/oddmuse"))
	("localhost"
	 :channels ("&bitlbee")))
      rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
      rcirc-decode-coding-system 'undecided
      rcirc-ignore-list '("consolers" "enometh" "ams" "jordanb" "Nihplod"
			  "raela" "krisfremen" "dustpuppy" "rudybot" "GumbyPAN"
			  "urlinfo"))

;; at the office...
(when (string-match "^[A-Z]+[0-9]+$" system-name)
  (let ((channels (plist-get (cdr (assoc "chat.freenode.net" rcirc-server-alist)) :channels)))
    (when channels
      (dolist (channel '("#sql"))
	(unless (member channel channels)
	  ;; modify in place
	  (nconc channels (list channel)))))))

;; no more splitting of messages at 420 characters
(eval-after-load 'rcirc
  '(defalias 'rcirc-split-message 'list))

;; colors
(use-package rcirc-color :after rcirc)
(setq rcirc-color-other-attributes '(:weight bold))

;; prepare a suitable list of colors
;; https://stackoverflow.com/questions/3116260/given-a-background-color-how-to-get-a-foreground-color-that-makes-it-readable-o#3118280
(require 'cl-lib)
(defun rcirc-colors-Y (color)
  (let ((gamma 2.2))
    (destructuring-bind (r g b) (color-name-to-rgb color)
      (let* ((r1 (expt r gamma))
	     (g1 (expt g gamma))
	     (b1 (expt b gamma)))
	(+ (* r1 0.2126)
	   (* 0.7151 g1)
	   (* 0.0721 b1))))))

;; To check out the list, evaluate (list-colors-display rcirc-colors)
;; To reset map (setq rcirc-color-mapping (make-hash-table :test 'equal))
;; Delay setting the colors until the background color is defined.

(defun rcirc-color-reset ()
  "Reset the colours."
  (interactive)
  (setq rcirc-colors
	(let* ((candidates nil)
	       (color nil)
	       (y1 (rcirc-colors-Y (face-background 'default)))
	       (dark (< y1 0.5))
	       (upper-limit (if dark 0.8 0.3))
	       (lower-limit (if dark 0.6 0.0)))
	  (dolist (item color-name-rgb-alist)
	    (setq color (car item))
	    (unless (color-gray-p color)
	      ;; Contrast ration is (Y(b) + 0.05) / (Y(d) + 0.05) where
	      ;; Y(b) is the brightness (luminance) of the brighter
	      ;; color and Y(d) is the brightness of the darker color.
	      (let* ((y2 (rcirc-colors-Y color))
		     (r (if (> y1 y2)
			    (/ (+ y1 0.05) (+ y2 0.05))
			  (/ (+ y2 0.05) (+ y1 0.05)))))
		(when (and (> r 4.5) (> y2 lower-limit) (< y2 upper-limit))
		  (setq candidates (cons color candidates))))))
	  candidates))
  (setq rcirc-color-mapping (make-hash-table :test 'equal))
  ;; make sure the nick "s" gets no colour
  (dolist (c '("a" "s"))
    (puthash c '() rcirc-color-mapping)))

(eval-after-load 'rcirc '(rcirc-color-reset))

(eval-after-load 'rcirc
  '(defun-rcirc-command encoding (arg)
     "Change the encoding coding system
`rcirc-encode-coding-system' for the current buffer only."
     (interactive)
     (if (string= arg "")
	 (rcirc-print process (rcirc-nick process) "NOTICE" target
		      (symbol-name rcirc-encode-coding-system))
       (set (make-local-variable 'rcirc-encode-coding-system)
	    (intern-soft arg)))))

(eval-after-load 'rcirc
  '(defun-rcirc-command sv (arg)
     "Tell everybody about `emacs-version'."
     (interactive)
     (rcirc-send-message process target
			 (replace-regexp-in-string
			  "\n *" " "
			  (concat "I use " (emacs-version))))))

(defun rcirc-unfill ()
  (interactive)
  (save-excursion
    (goto-char rcirc-prompt-end-marker)
    (while (re-search-forward "\\s-+" nil t)
      (replace-match " "))))

(add-hook 'rcirc-mode-hook
	  (lambda ()
	    (if (help-function-arglist 'rcirc-omit-mode)
		(rcirc-omit-mode 1)
	      (rcirc-omit-mode))
	    (rcirc-track-minor-mode 1)
	    (local-set-key (kbd "M-q") 'rcirc-unfill)))

(eval-after-load 'rcirc
  '(define-key rcirc-mode-map (kbd "SPC")
     (lambda ()
       (interactive)
       (if (< (point) rcirc-prompt-start-marker)
	   (scroll-up)
	 (call-interactively 'self-insert-command)))))

(eval-after-load 'rcirc
  '(define-key rcirc-mode-map (kbd "DEL")
     (lambda ()
       (interactive)
       (if (< (point) rcirc-prompt-end-marker)
	   (scroll-down)
	 (backward-delete-char-untabify 1)))))

;;; mode invisible

(defadvice my-invisible-preference (after rcirc-connect activate)
  "When connecting to a server, set the user mode to +i (invisible)."
  (let ((process ad-return-value)
	(nick (or nick rcirc-default-nick)))
    (rcirc-send-string process (concat "MODE " nick " +i"))))

;; rcirc /op

(eval-after-load 'rcirc
  '(defun-rcirc-command op (input)
     "Op myself on the current channel."
     (interactive "s")
     (rcirc-send-message process "chanserv"
			 (concat "op " target))))

;; rcirc /deop

(eval-after-load 'rcirc
  '(defun-rcirc-command deop (input)
     "Deop myself on the current channel."
     (interactive "s")
     (rcirc-send-message process "chanserv"
			 (concat "deop " target))))

;; rcirc /mute

(eval-after-load 'rcirc
  '(defun-rcirc-command mute (input)
     "Mute nick"
     (interactive "s")
     (rcirc-send-string process (format "MODE %s +q %s!*@*"
					target input))))

;; rcirc /unmute

(eval-after-load 'rcirc
  '(defun-rcirc-command unmute (input)
     "Mute nick"
     (interactive "s")
     (rcirc-send-string process (format "MODE %s -q %s!*@*"
					target input))))

;; rcirc /kickban

(eval-after-load 'rcirc
  '(defun-rcirc-command kickban (input)
     "Kickban a nick for two hours."
     (interactive "s")
     ;; (rcirc-send-string process (format "MODE %s +b %s!*@*" target input))
     ;; (rcirc-send-string process (format "KICK %s %s kickban!" target input))
     (rcirc-send-privmsg process "chanserv" (format "AKICK %s ADD %s !T 120 banned for 2h -- kensanata" target input))))

;; rcirc /unban

(eval-after-load 'rcirc
  '(defun-rcirc-command unban (input)
     "Unban a nick."
     (interactive "s")
     ;; (rcirc-send-string process (format "MODE %s -b %s!*@*" target input))
     (rcirc-send-privmsg process "chanserv" (format "AKICK %s DEL %s!*@*" target input))))

;; rcirc /occur

(eval-after-load 'rcirc
  '(defun-rcirc-command occur (regexp)
     "Run `multi-occur' for all buffers in `rcirc-mode'."
     (interactive "sList lines matching regexp: ")
     (multi-occur (let (result)
		    (dolist (buf (buffer-list))
		      (with-current-buffer buf
			(when (eq major-mode 'rcirc-mode)
			  (setq result (cons buf result)))))
		    result) regexp)))

;; other

(defun irc-version-summary ()
  "Run this after you've sent a /CTCP #test VERSION."
  (interactive)
  (save-excursion
    (let ((agents))
      (while (re-search-forward "CTCP VERSION \\([a-z]\\([^- \n:/]+\\|[^- \n:/]*[-/][^- \n:/0-9]+\\)\\)" nil t)
	(let* ((key (replace-regexp-in-string "-" ""
		       (downcase (match-string-no-properties 1))))
	       (cell (or (assoc key agents)
			 (car (setq agents (cons (cons key 0) agents))))))
	  (setcdr cell (1+ (cdr cell)))))
      (setq agents (sort agents (lambda (a b) (> (cdr a) (cdr b)))))
      (pop-to-buffer (get-buffer-create "*IRC Clients*"))
      (erase-buffer)
      (dolist (item agents)
	(insert (format "%4d %s\n" (cdr item) (car item))))
      (insert "----\n"
	      (format "%4d total\n"
		      (apply '+ (mapcar 'cdr agents)))))))

;; no more visible URLs
(eval-after-load 'rcirc
  (lambda ()
    (setq rcirc-markup-text-functions
	  (delq 'rcirc-markup-urls rcirc-markup-text-functions))
    (add-to-list 'rcirc-markup-text-functions 'rcirc-url-buttons)))

(defvar asc:rcirc-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "w" 'asc:rcirc-copy-url)
    (define-key map "&" 'asc:rcirc-browse-url)
    (set-keymap-parent map button-map)
    map)
  "The keymap to use for URLs in rcirc.")

(defun asc:rcirc-browse-url (url)
  "Browse the URL under point using `browse-url-default-browser'."
  (interactive (list (or (get-text-property (point) 'rcirc-url)
			 (ffap-url-at-point))))
  (browse-url-default-browser url))

(defun asc:rcirc-copy-url (url)
  "Copy the URL under point to the kill ring."
  (interactive (list (or (get-text-property (point) 'rcirc-url)
			 (ffap-url-at-point))))
  (if (not url)
      (message "No URL at this position.")
    (kill-new url)
    (message "Copied %s" url)))

(defun rcirc-url-buttons (sender response)
  "Turn URLs in into buttons.
This is a function to add to `rcirc-markup-text-functions'
instead of `rcirc-markup-urls'."
  (while (re-search-forward rcirc-url-regexp nil t)
    (let* ((start (match-beginning 0))
           (end (match-end 0))
           (url (match-string-no-properties 0))
	   (text (cond ((string-match "\\.\\(png\\|jpe?g\\)$" url)
			"PIC")
		       ((string-match "\\.gif$" url)
			"GIF");; these are usually worse than pics
		       ((string-match "^https://twitter\\.com" url)
			"TWIT");; these are usually worse
		       ((string-match "::" url)
			nil);; bogus Perl module
		       (t "LINK"))))
      (when text
	(make-text-button start end
			  'face 'rcirc-url
			  'follow-link t
			  'rcirc-url url
			  'help-echo url;; seems to have no effect
			  'display text
			  'keymap asc:rcirc-link-keymap
			  'action (lambda (button)
				    (browse-url (button-get button 'rcirc-url))))
	;; record the url if it is not already the latest stored url
	(when (not (string= url (caar rcirc-urls)))
	  (push (cons url start) rcirc-urls))))))

;; Sadly, these buttons with the display property set no longer work
;; for mouse buttons. (rcirc-markup-urls nil nil) http://example.html
;; http://example.png http://example.jpg http://example.gif
;; http://example

(eval-after-load 'rcirc
  '(add-to-list 'rcirc-markup-text-functions 'asc:rcirc-dim-keywords))

(defvar asc:rcirc-dim-keywords
  '("favourited your status: "
    "boosted your status: "
    " followed you"
    "You\\(, \\(direct\\|private\\|unlisted\\|public\\)\\)?: ")
  "Keywords which result in the entire message being dimmed.")

(defun asc:rcirc-dim-keywords (_senders _response)
  "Dim message if it contains particular phrases.
Phrases to take are from `asc:rcirc-dim-keywords'.
Each function takes two arguments, SENDER, and RESPONSE.  The
buffer is narrowed with the text to be printed and the point is
at the beginning of the ‘rcirc-text’ propertized text."
  (dolist (str asc:rcirc-dim-keywords)
    (goto-char (point-min))
    (when (search-forward str nil t)
      (rcirc-add-face (point-min) (point-max) 'rcirc-dim-nick))))

;; when looking at the data in rcirc-color-mapping, for example

(defun describe-hash (variable &optional buffer)
  "Display the full documentation of VARIABLE (a symbol).
    Returns the documentation as a string, also.
    If VARIABLE has a buffer-local value in BUFFER (default to the current buffer),
    it is displayed along with the global value."
  (interactive
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read
		(if (and (symbolp v)
			 (hash-table-p (symbol-value v)))
		    (format
		     "Describe hash-map (default %s): " v)
		  "Describe hash-map: ")
		obarray
		(lambda (atom) (and (boundp atom)
				    (hash-table-p (symbol-value atom))))
		t nil nil
		(if (hash-table-p v) (symbol-name v))))
     (list (if (equal val "")
	       v (intern val)))))
  (with-output-to-temp-buffer (help-buffer)
    (maphash (lambda (key value)
	       (pp key)
	       (princ " => ")
	       (pp value)
	       (terpri))
	     (symbol-value variable))))

