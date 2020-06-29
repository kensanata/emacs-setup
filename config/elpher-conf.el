(let ((version (emacs-version)))
  (when (and (string-match "^GNU Emacs \\([0-9]+\\)\.\\([0-9]+\\)" version)
	     (or (> (string-to-number (match-string 1 version)) 26)
		 (and (= (string-to-number (match-string 1 version)) 26)
		      (> (string-to-number (match-string 2 version)) 2))))
    ;; requires Emacs 26.2 these days
    (asc:package-install 'elpher)))

(add-hook 'elpher-mode-hook
	  (lambda ()
	    (local-set-key (kbd "u") 'elpher-up)
	    (local-set-key (kbd "b") 'elpher-back)
	    (local-set-key (kbd "l") 'elpher-back)))

(defun elpher-node-up (node)
  "Does the node have a selector that looks like a path?"
  (let* ((address (elpher-node-address node))
	 (selector (elpher-address-selector address))
	 (up (file-name-directory (directory-file-name selector))))
    (when (not (string= up selector))
      (elpher-make-node (concat "Up from " (elpher-node-display-string elpher-current-node))
			(elpher-make-address ?1
					     up
					     (elpher-address-host address)
					     (elpher-address-port address)
					     (elpher-address-use-tls-p address))
			elpher-current-node))))

(defun elpher-up ()
  "Go up in a gopher site."
  (interactive)
  (let ((up (elpher-node-up elpher-current-node)))
    (if up
	(elpher-visit-node up)
      (error "No way up"))))
