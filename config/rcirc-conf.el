;;; rcirc, write such as not to require rcirc at startup
;; (autoload 'rcirc "~/src/emacs/lisp/net/rcirc" t)
(asc:package-install 'rcirc-color)
(asc:package-install 'rcirc-styles)
(asc:package-install 'rcirc-notify)
(asc:package-install 'rcirc-menu)
(require 'rcirc-emojis)

;;; bitlbee
(cond ((file-exists-p "/usr/sbin/bitlbee");; PureOS
       (global-set-key (kbd "C-c e") 'asc:rcirc-start))
      ((file-exists-p "/usr/local/sbin/bitlbee");; Mac
       (autoload 'bitlbee-start "bitlbee" t)
       (setq bitlbee-executable "/usr/local/sbin/bitlbee")
       (global-set-key (kbd "C-c e") 'asc:rcirc-and-bitlbee-start))
      ((file-exists-p "c:/cygwin64/usr/local/sbin/bitlbee.exe");; Cygwin
       (autoload 'bitlbee-start "bitlbee" t)
       (setq bitlbee-executable "c:/cygwin64/usr/local/sbin/bitlbee.exe"
	     bitlbee-options "-n -D -v -d /home/asc/.bitlbee")
       "c:/cygwin64/var/lib/bitlbee"
       (global-set-key (kbd "C-c e") 'asc:rcirc-and-bitlbee-start))
      (t
       (global-set-key (kbd "C-c e") 'asc:rcirc-start)))

(defun asc:rcirc-and-bitlbee-start ()
  "Start both bitlbee and `rcirc'."
  (interactive)
  (if (bitlbee-start); needs time to start up
      (run-with-idle-timer
       1 nil
       'asc:rcirc-start)
    ;; alternatively, just run it
    'asc:rcirc-start))

(defun asc:rcirc-start ()
  "Start `rcirc'."
  (interactive)
  (rcirc nil)
  (rcirc-menu))

(setq rcirc-prompt "%n> "; list nick
      rcirc-fill-prefix "    "
      rcirc-fill-column 79; side-by-side on my laptop
      rcirc-max-message-length 0; live dangerously!
      rcirc-default-nick "kensanata"
      rcirc-keywords '("ken" "kens" "kensa" "alex")
      rcirc-nick-prefix-chars "~&@%+!"
      rcirc-authinfo (with-temp-buffer
		       (when (file-readable-p "~/.rcirc-authinfo")
			 (insert-file-contents-literally "~/.rcirc-authinfo")
			 (read (current-buffer))))
      rcirc-server-alist
      ;; host chat.freenode.net but see https://alexschroeder.ch/wiki/2017-07-15_Freenode_IPv6
      ;; sometimes we have to use 71.11.84.232
      ;; port 6697 7000 7070 according to http://freenode.net/kb/answer/chat
      `(("chat.freenode.net"
	 :port 7000 :encryption tls
	 :channels ("#emacs"
		    "#emacs-ops"
		    "#rcirc"
		    "#wiki"
		    "#oddmuse"
		    "##emacs.de"
		    "#emacswiki"
		    "#perl"
		    "#bussard"
		    "#mastodon"
		    "#purism"
		    "#gopherproject"
		    "#tokipona"
		    ,@(when (eq (window-system) 'w32)
			'("#sql" "#eclipse-scout"))))
	("irc.sdf.org" ;; no TLS
	 :channels ("#gopher"))
	("irc.oftc.net"
	 :port 6697 :encryption tls
	 :channels ("#bitlbee"))
	;; ("irc.gitter.im" :port 6697 :encryption tls
	;;  :password ,(nth 3 (assoc "gitter" rcirc-authinfo))
	;;  :channels ("#kensanata/elisp"
	;; 	    "#kensanata/oddmuse"))
	("localhost"
	 :channels ("&bitlbee"
		    "#rpg-traveller"
		    "#rpg-osr"
		    "#rpg-game-design"
		    "#rpg-indie-games"
		    "#rpg-game-masters"
		    "#rpg-announcements"
		    "#rpg-general"
		    "#osr-general")))
      rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
      rcirc-decode-coding-system 'undecided
      rcirc-ignore-list '("consolers" "enometh" "ams" "jordanb" "Nihplod"
			  "raela" "krisfremen" "dustpuppy" "rudybot" "GumbyPAN"
			  "urlinfo"))

;; no more splitting of messages at 420 characters
(eval-after-load 'rcirc
  '(defalias 'rcirc-split-message 'list))

;; prepare a suitable list of colors
;; https://stackoverflow.com/questions/3116260/given-a-background-color-how-to-get-a-foreground-color-that-makes-it-readable-o#3118280
(require 'cl)
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

(defconst rcirc-colors
  (let* ((candidates nil)
	 (color nil)
	 (y1 (rcirc-colors-Y (face-background 'default)))
	 (upper-limit (* 0.8 (- 1 y1)))
	 (lower-limit (* 0.6 (- 1 y1))))
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

(eval-after-load 'rcirc '(require 'rcirc-color))
(eval-after-load 'rcirc '(require 'rcirc-menu))

(eval-after-load 'rcirc '(require 'rcirc-styles))
(eval-after-load 'rcirc '(require 'rcirc-notify))

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

;; define faces

;; (defface rcirc-nick-in-message
;;   '((((background dark)) :background "dim gray")
;;     (t :background "lemon chiffon"))
;;   "My nick when mentioned by others.")
;; (defface rcirc-my-nick '((t :foreground "plum"))
;;   "My own nick for rcirc.")
;; (defface rcirc-track-nick '((t :inherit rcirc-my-nick))
;;   "The face used indicate activity directed at you.")
;; (defface rcirc-nick-in-message-full-line '((t))
;;   "The face used emphasize the entire message when your nick is mentioned.")
;; (defface rcirc-track-keyword '((t (:inherit bold)))
;;   "The face used indicate activity directed at you.")
;; (defface rcirc-prompt '((t :foreground "orchid"))
;;   "My prompt for rcirc.")

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
