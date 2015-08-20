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

;; isearch occur
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; fix read-only bug
(defadvice isearch-occur (after isearch-occur-fix activate)
  "Fix bug #20971: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20971"
  (setq buffer-read-only nil))
