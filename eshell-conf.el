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

(setq eshell-save-history-on-exit t)

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-z") 'bury-buffer)
	    (local-set-key (kbd "C-a") 'eshell-bol)
	    (local-set-key (kbd "C-w") 'asc:kill-region)
	    (local-set-key (kbd "<up>") 'previous-line)
	    (local-set-key (kbd "<down>") 'next-line)))

(defalias 'eshell/emacs 'find-file)
(defalias 'eshell/less 'find-file)
(defun eshell/dired () (dired (eshell/pwd)))

(defun asc:kill-region (begin end)
  "Since `eshell' adds read-only prompts, I need to override this."
  (interactive "r")
  (let ((inhibit-read-only t))
    (kill-region (region-beginning) (region-end))))

(setq eshell-last-dir-ring-size 500)

(setenv "WikiDataDir" "/Users/alex/Source/oddmuse/test-data")
