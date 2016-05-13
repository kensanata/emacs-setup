(defvar as/init-dir "~/.emacs.d/emacs-setup"
  "The init dir where all the -conf.el files are.")

;; this is where to put elisp libraries from the Internet (including
;; my own)
(add-to-list 'load-path (expand-file-name "lib" as/init-dir))

;; load all the *-conf.el files, in order
(dolist (file (directory-files (concat as/init-dir "/config")
			       t "-conf\\.el\\'"))
  (load-file file))

(server-start)

;; fix read-only bug
(defadvice isearch-occur (after isearch-occur-fix activate)
  "Fix bug #20971: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20971"
  (setq buffer-read-only nil))

(setq initial-scratch-message
      (concat ";; This Emacs is a config installed from\n"
	      ";; https://github.com/kensanata/emacs-setup\n")
      inhibit-startup-screen t)
