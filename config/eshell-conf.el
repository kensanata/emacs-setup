(global-set-key (kbd "C-z") 'eshell-here)

(defun eshell-here (&optional arg)
  (interactive "P")
  (cond ((and arg (buffer-file-name))
         (let ((dir (file-name-directory (buffer-file-name))))
	   (eshell)
	   (eshell/cd dir)
           (eshell-reset)))
        ((process-live-p (get-buffer-process (get-buffer "*compilation*")))
         (pop-to-buffer "*compilation*"))
        (t (eshell arg))))

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

;; plan 9
;; (autoload 'eshell-smart-initialize "em-smart" "Setup Eshell smart display." t)

(defun eshell-bash-completion ()
  (setq-local bash-completion-nospace t)
  (while (pcomplete-here
	  (nth 2 (bash-completion-dynamic-complete-nocomint
		  (save-excursion (eshell-bol) (point)) (point))))))

(add-hook 'eshell-mode-hook 'asc:eshell-mode-init)

(defun asc:eshell-mode-init ()
  (local-set-key (kbd "C-z") 'bury-buffer)
  (local-set-key (kbd "C-a") 'eshell-bol)
  (local-set-key (kbd "C-w") 'asc:kill-region)
  (eldoc-mode 1)
  (idle-highlight-mode 1)
  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient"))

(add-hook 'eshell-hist-mode-hook 'asc:eshell-hist-mode-init)

(defun asc:eshell-hist-mode-init ()
  (define-key eshell-hist-mode-map (kbd "<up>") 'previous-line)
  (define-key eshell-hist-mode-map (kbd "<down>") 'next-line))

(defalias 'eshell/emacs 'find-file)
(defalias 'eshell/less 'find-file)
(defun eshell/dired () (dired (eshell/pwd)))

(defun asc:kill-region (begin end)
  "Since `eshell' adds read-only prompts, I need to override this."
  (interactive "r")
  (let ((inhibit-read-only t))
    (kill-region (region-beginning) (region-end))))

(defun supershell ()
  "Quickly toggle back and forth as root using tramp and su.
NOTE: Place in ~/.authinfo or ~/.authinfo.gpg something like
what follows to get a sudo-like experience:

machine HOSTNAME login root password PASSWORD"
  (if (not (string-match "/su:root@" (pwd)))
      (progn
        (insert "cd /su::$PWD")
        (eshell-send-input))
    (progn
      (insert "cd $OLDPWD")
      (eshell-send-input))))
