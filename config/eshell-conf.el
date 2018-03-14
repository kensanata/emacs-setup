;; Setting up some directories, but only for certain of my systems.

(when (file-directory-p "/Users/alex/Source/oddmuse/test-data")
  (setenv "WikiDataDir" "/Users/alex/Source/oddmuse/test-data"))

(when (file-directory-p "C:\\Program Files (x86)\\GNU\\GnuPG")
  (add-to-list 'exec-path "C:\\Program Files (x86)\\GNU\\GnuPG"))

(when (file-directory-p "C:\\Program Files\\nodejs")
  (add-to-list 'exec-path "C:\\Program Files\\nodejs"))

(when (file-directory-p "C:\\Users\\asc\\AppData\\Roaming\\bin")
  (add-to-list 'exec-path "C:\\Users\\asc\\AppData\\Roaming\\bin"))
;; (executable-find "lein")

(global-set-key (kbd "C-z") 'eshell-here)

(defun eshell-here (&optional arg)
  (interactive "P")
  (if (and arg (buffer-file-name))
      (let ((dir (file-name-directory (buffer-file-name))))
	(eshell)
	(cd dir))
    (eshell arg)))

(global-set-key (kbd "C-x 4 C-z") 'eshell-other-window)

(defun eshell-other-window (&optional arg)
  (interactive "P")
  (if (one-window-p)
      (split-window)
    (other-window 1))
  (eshell arg))

(setq eshell-history-size 500
      eshell-save-history-on-exit t
      eshell-hist-ignoredups t
      eshell-last-dir-ring-size 500
      ;; match eshell prompts to ssh connection with a port (include `#')
      eshell-prompt-regexp "^[^$\n]* [#$] ")

;; eldoc
;; (asc:package-install 'esh-help)
;; (eval-after-load "eshell" '(setup-esh-help-eldoc))

;; completion
;; (asc:package-install 'bash-completion)
;; (setq eshell-default-completion-function 'eshell-bash-completion)

;; plan 9
(autoload 'eshell-smart-initialize "em-smart" "Setup Eshell smart display." t)

(defun eshell-bash-completion ()
  (setq-local bash-completion-nospace t)
  (while (pcomplete-here
	  (nth 2 (bash-completion-dynamic-complete-nocomint
		  (save-excursion (eshell-bol) (point)) (point))))))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-z") 'bury-buffer)
	    (local-set-key (kbd "C-a") 'eshell-bol)
	    (local-set-key (kbd "C-w") 'asc:kill-region)
	    (local-set-key (kbd "<up>") 'previous-line)
	    (local-set-key (kbd "<down>") 'next-line)
	    (idle-highlight-mode 1)
	    (eldoc-mode 1)
	    (eshell-smart-initialize)
	    (setenv "PAGER" "cat")
	    (setenv "EDITOR" "emacsclient")))

(defalias 'eshell/emacs 'find-file)
(defalias 'eshell/less 'find-file)
(defun eshell/dired () (dired (eshell/pwd)))

(defun asc:kill-region (begin end)
  "Since `eshell' adds read-only prompts, I need to override this."
  (interactive "r")
  (let ((inhibit-read-only t))
    (kill-region (region-beginning) (region-end))))
