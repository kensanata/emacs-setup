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

(add-hook 'elpher-menu-mode-hook
	  (lambda ()
	    (local-set-key (kbd "n") (lambda () (interactive) (elpher 1)))
	    (local-set-key (kbd "a") 'asc:elpher-match-alexschroeder.ch)
	    (local-set-key (kbd "w") 'asc:elpher-search-en.wikipedia.org)))

(defun asc:elpher-match-alexschroeder.ch (terms)
  (interactive "sTerms: ")
  (let* ((name (format "*alexschroeder.ch*" terms))
	 (buf (get-buffer-create name)))
    (pop-to-buffer-same-window buf)
    (elpher-mode)
    (elpher-visit-page
     (elpher-make-page
      (format "Alex Schroeder: %s" terms)
      (elpher-address-from-url
       (concat "gemini://alexschroeder.ch/do/match?"
	       (url-hexify-string terms)))))))

(defun asc:elpher-search-en.wikipedia.org (terms)
  (interactive "sTerms: ")
  (let* ((name (format "*en.wikipedia.org*" terms))
	 (buf (get-buffer-create name)))
    (pop-to-buffer-same-window buf)
    (elpher-mode)
    (elpher-visit-page
     (elpher-make-page
      (format "English Wikipedia: %s" terms)
      (elpher-address-from-url
       (concat "gemini://vault.transjovian.org/text/en/"
	       (url-hexify-string terms)))))))

;; sometimes I'm in places with very bad connectivity
(setq elpher-connection-timeout 20)

(let ((version (emacs-version)))
  (when (and (string-match "^GNU Emacs \\([0-9]+\\)\.\\([0-9]+\\)" version)
	     (or (> (string-to-number (match-string 1 version)) 26)
		 (and (= (string-to-number (match-string 1 version)) 26)
		      (> (string-to-number (match-string 2 version)) 2))))
    ;; requires Emacs 26.2 these days
    (asc:package-install 'elpher)))

(add-to-list 'load-path "/home/alex/src/elpher")
(autoload 'elpher "elpher" "Gopher and Gemini client" t)
(autoload 'elpher-menu "elpher" "An overview of all Elpher activities" t)

(add-to-list 'load-path "/home/alex/src/gemini-write")
(autoload 'elpher-edit "gemini-write" "Edit a Gemini page" t)
;; cannot delay loading until the user uses e to edit a page because
;; we need the advice for elpher-render-gemini-plain-text
(eval-after-load "elpher" '(load-library "gemini-write"))

(add-to-list 'load-path "/home/alex/src/gemini.el")
(autoload 'gemini-mode "gemini-mode" "Gemini Mode" t)

(add-hook 'gemini-mode-hook 'typo-mode)
(add-hook 'gemini-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c i") 'gemini-insert-link-to-oddmuse-wiki)
	    (local-set-key (kbd "C-c l") 'gemini-insert-local-link)))

(add-hook 'elpher-mode-hook
	  (lambda ()
	    (local-set-key (kbd "u") 'elpher-up)
	    (local-set-key (kbd "b") 'elpher-back)
	    (local-set-key (kbd "l") 'elpher-back)))

(defun gemini-insert-local-link (pagename)
  "Insert a link to a new page on this wiki."
  (interactive "sPage name: ")
  (let ((escaped (url-hexify-string pagename)))
    (if (string= pagename escaped)
	(insert "=> " pagename)
      (insert "=> " escaped " " pagename))))

(defun gemini-insert-link-to-oddmuse-wiki (pagename)
  "Insert a link to PAGENAME of the current wiki with completion."
  (interactive (list (oddmuse-read-pagename oddmuse-wiki)))
  (insert "=> " pagename " " (replace-regexp-in-string "_" " " pagename)))
  
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
