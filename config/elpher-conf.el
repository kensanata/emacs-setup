;;; -*- lexical-binding:t -*-

(unless (fboundp 'read-answer)
  (load-file (expand-file-name "~/src/emacs/lisp/emacs-lisp/map-ynp.el")))
(unless (fboundp 'assoc-delete-all)
  (defun assoc-delete-all (key alist &optional test)
    "Delete from ALIST all elements whose car is KEY.
Compare keys with TEST.  Defaults to `equal'.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
    (unless test (setq test #'equal))
    (while (and (consp (car alist))
		(funcall test (caar alist) key))
      (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
	(if (and (consp (car tail-cdr))
		 (funcall test (caar tail-cdr) key))
	    (setcdr tail (cdr tail-cdr))
	  (setq tail tail-cdr))))
    alist))

(global-set-key (kbd "C-c q") 'elpher-menu)

;; sometimes I'm in places with very bad connectivity
(setq elpher-connection-timeout 20)

(add-hook 'elpher-menu-mode-hook
	  (lambda ()
	    (local-set-key (kbd "e")
			   (lambda ()
			     (interactive)
			     (elpher 1)))))

(let ((version (emacs-version)))
  (when (and (string-match "^GNU Emacs \\([0-9]+\\)\.\\([0-9]+\\)" version)
	     (or (> (string-to-number (match-string 1 version)) 26)
		 (and (= (string-to-number (match-string 1 version)) 26)
		      (> (string-to-number (match-string 2 version)) 2))))
    ;; requires Emacs 26.2 these days
    (asc:package-install 'elpher)))

(add-to-list 'load-path "/home/alex/src/elpher")
(autoload 'elpher "elpher" "Gopher and Gemini client" t)

(add-to-list 'load-path "/home/alex/src/gemini-write")
(autoload 'elpher-edit "gemini-write" "Edit a Gemini page" t)
(eval-after-load "elpher"
  '(define-key elpher-mode-map (kbd "e") 'elpher-edit))

(add-to-list 'load-path "/home/alex/src/gemini.el")
(autoload 'gemini-mode "gemini-mode" "Gemini Mode" t)

(add-hook 'gemini-mode-hook 'typo-mode)

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
