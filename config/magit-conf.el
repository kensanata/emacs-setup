;; Emacs, Cygwin, whatever... Magit learns to live with Cygwin names.
;; https://github.com/magit/magit/issues/1318

(asc:package-install 'magit)

(when (file-executable-p "c:/Program Files/Git/cmd/git.exe")
  (setq magit-git-executable "c:/Program Files/Git/cmd/git.exe"))

;; New option for diff

;; (eval-after-load "magit-diff"
;;   '(asc:add-magit-diff-text-option))

;; (defun asc:add-magit-diff-text-option ()
;;   (dolist (popup '(magit-diff-popup
;; 		   magit-diff-refresh-popup
;; 		   magit-diff-mode-refresh-popup
;; 		   magit-revision-mode-refresh-popup))
;;     (let ((switches (plist-member popup :switches)))
;;       (when switches
;; 	(nconc (cadr switches) (list (list ?t "Treat as text files" "--text")))))))

;; Cygwin

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

;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
;; FIXME: should work on that!
