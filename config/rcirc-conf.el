;;; rcirc, write such as not to require rcirc at startup
(autoload 'rcirc "rcirc" t)

;;; bitlbee
(autoload 'bitlbee-start "bitlbee" t)
(setq bitlbee-executable "/usr/local/sbin/bitlbee")

(global-set-key (kbd "C-c e")
		(lambda ()
		  (interactive)
		  (bitlbee-start); needs time to start up
		  (run-with-idle-timer 1 nil 'rcirc nil)))

(setq rcirc-prompt "%n> "; list nick
      rcirc-fill-prefix "    "
      rcirc-fill-column 79; side-by-side on my laptop
      rcirc-default-nick "kensanata"
      rcirc-keywords '("ken" "kens" "kensa" "alex")
      rcirc-nick-prefix-chars "~&@%+!"
      rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls
	 :channels ("#emacs" "#emacs-ops" "#rcirc" "#wiki" "#oddmuse"
		    "#epfarms" "#emacswiki" "#perl" "#org-mode"
		    ;; "#indieweb" "#raspbian" "#machomebrew"
		    ;; "#julia" "#clojure" "#leiningen" "#clojurescript" "#go-nuts"
		    ))
	("megabombus.local" :channels ("&bitlbee" "&roleplaying"
                                       "&emacs" "&bsi"))
	;; ("irc.gimp.org" :channels ("#gimp-users"))
	;; "#eclipse" "#eclipse-de" "#osgi" "#perl6" "#inkscape"
	;; "#ubuntu" "#hcoop" "#biology" "#clojure" "#clojure.de"
	;; ("ircnet.eversible.com" :channels ("#drsrm"))
	;; ("irc.euirc.net" :channels ("#tanelorn"))
	;; ("irc.lugs.ch"   :channels ("#hackerfunk"))
	;; ("ssl.irc.perl.org" :port 7062 :encryption tls
	;;  :channels ("#perlde" "#mojo"))
	;; ("irc.sorcery.net" :channels ("#Stargazer"))
	;; ("irc.otherworlders.org" :channels ("#pathfinder"))
	;; "#enworld"
	;; ("irc.sorcery.net" :channels ("#StargazersWorld"))
	;; ("irc2.magicstar.net" :channels ("#magicstar" "#rpgnet"))
	;; ("irc.oftc.net" :channels ("#bitlbee"))
	)
      rcirc-decode-coding-system 'undecided
      rcirc-coding-system-alist
      '(("#nihongo" undecided . iso-2022-jp))
      rcirc-ignore-list '("consolers" "enometh" "ams" "jordanb" "Nihplod"
			  "raela" "krisfremen" "dustpuppy" "rudybot" "GumbyPAN")
      rcirc-log-flag t
      rcirc-authinfo (with-temp-buffer
		       (insert-file-contents-literally "~/.rcirc-authinfo")
		       (read (current-buffer))))

(require 'cl)
(setq rcirc-colors
      (let (candidates)
        (dolist (item color-name-rgb-alist)
	  (destructuring-bind (color r g b) item
	    (let ((d (sqrt (+ (* (/ r 512) (/ r 512))
			      (* (/ g 512) (/ g 512))
			      (* (/ b 512) (/ b 512))))))
	      (if (and (not (= r g))
		       (not (= r b)); grey
		       (> d 10)
		       (< d 150))
		(setq candidates (cons color candidates))))))
	candidates))

(eval-after-load 'rcirc '(require 'rcirc-color))
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

(defface rcirc-nick-in-message
  '((((background dark)) :background "dim gray")
    (t :background "lemon chiffon"))
  "My nick when mentioned by others.")
(defface rcirc-my-nick '((t :foreground "purple"))
  "My own nick for rcirc.")
(defface rcirc-track-nick '((t :inherit rcirc-my-nick))
  "The face used indicate activity directed at you.")
(defface rcirc-nick-in-message-full-line '((t))
  "The face used emphasize the entire message when your nick is mentioned.")
(defface rcirc-track-keyword '((t (:inherit bold)))
  "The face used indicate activity directed at you.")
(defface rcirc-prompt '((t :foreground "orchid"))
  "My prompt for rcirc.")

;;; mode invisible

(defadvice my-invisible-preference (after rcirc-connect activate)
  "When connecting to a server, set the user mode to +i (invisible)."
  (let ((process ad-return-value)
	(nick (or nick rcirc-default-nick)))
    (rcirc-send-string process (concat "MODE " nick " +i"))))