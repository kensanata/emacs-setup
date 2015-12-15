;; Emacs, Cygwin, whatever... Magit learns to live with Cygwin names.
;; https://github.com/magit/magit/issues/1318

(defadvice magit-expand-git-file-name
    (before magit-expand-git-file-name-cygwin activate)
  "Handle Cygwin directory names such as /cygdrive/c/*
by changing them to C:/*"
  (when (string-match "^/cygdrive/\\([a-z]\\)/\\(.*\\)" filename)
    (setq filename (concat (match-string 1 filename) ":/"
			   (match-string 2 filename)))))

(defun un-cygwin-buffer-file-name ()
  (when (string-match "^\\([a-z]\\):/cygdrive/\\([a-z]\\)/\\(.*\\)" buffer-file-name)
    ;; assertion:  filename should look like "c:/cygwin/c/Users..." i.e. the drive is repeated
    (when (equal (match-string 1 buffer-file-name) (match-string 2 buffer-file-name))
      (set-visited-file-name
       (concat (match-string 1 buffer-file-name) ":/"
	       (match-string 3 buffer-file-name)) 't))))

(add-hook 'git-commit-mode-hook 'un-cygwin-buffer-file-name)
