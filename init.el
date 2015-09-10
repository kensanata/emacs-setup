(defvar as/init-dir "~/.emacs.d/alex"
  "The init dir where all the -conf.el files are.")

;; this is where to put elisp libraries from the Internet
(add-to-list 'load-path (expand-file-name "lib" as/init-dir))

;; this is where to put my own elisp libraries
(add-to-list 'load-path "~/src/elisp")

;; load all the *-conf.el files 
(dolist (file (directory-files as/init-dir t "-conf\\.el\\'"))
  (load-file file))
	      
(server-start)

(defun one-of (&rest items)
  "Return a random item from ITEMS."
  (nth (random (length items)) items))

(setq initial-scratch-message
      (concat (one-of "Hello" "Greetings" "Cheers" "Hoi")
	      ", "
	      (one-of (capitalize user-login-name)
		      user-full-name
		      "flesh man"
		      "Bob"
		      "user")
	      "! "
	      "I'm "
	      (one-of "happy"
		      "not unhappy"
		      "surprised"
		      "elated")
	      " to see you again."
	      "\n"
	      "Today will be "
	      (one-of "a good" "an excellent"
		      "an interesting"
		      "an ordinary")
	      " day. "
	      "Use it well.\n")
      inhibit-startup-screen t)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (with-current-buffer "*scratch*"
	      (comment-region (point-min) (point-max)))))

;; fix read-only bug
(defadvice isearch-occur (after isearch-occur-fix activate)
  "Fix bug #20971: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20971"
  (setq buffer-read-only nil))
