;;; -*- lexical-binding:t -*-

;; prefer my own copies
(add-to-list 'load-path "/home/alex/src/elpher")
(add-to-list 'load-path "/home/alex/src/gemini-write")

(use-package elpher :defer t
  ;; add Elpher as a web client via ../lib/elpher-http.el
  :config (require 'elpher-http)
  ;; also creates an autoload for elpher
  :bind ("C-c e" . elpher))

(use-package gemini-mode :defer t :commands gemini-mode :mode "\\.gmi\\'")

(add-hook 'elpher-mode-hook
	  (lambda ()
	    (local-set-key "u" #'elpher-up)
	    (local-set-key "b" #'elpher-back)
	    (local-set-key "l" #'elpher-back)
	    (local-set-key "j" #'bookmark-jump)
	    (local-set-key "n" (lambda () (interactive) (elpher 1)))
	    (local-set-key "W" #'asc:elpher-search-en.wikipedia.org)))

(defun elpher-up ()
  "Go up in a gopher site."
  (interactive)
  (let* ((address (elpher-page-address elpher-current-page))
	 (url (elpher-address-to-url address))
	 (urlobj (url-generic-parse-url url))
	 (path (car (url-path-and-query urlobj)))
	 (elems (and path (split-string path "/")))
	 (up (and elems (string-join (reverse (cdr (reverse elems))) "/"))))
    (unless up
      (error "Cannot go up from here"))
    (setf (url-filename urlobj) up)
    (elpher-go (url-recreate-url urlobj))))

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

;; to edit my wikis (cannot use-package because it's not on MELPA)
(autoload 'elpher-edit "gemini-write" "Edit a Gemini page" t)
;; cannot delay loading until the user uses e to edit a page because
;; we need the advice for elpher-render-gemini-plain-text
(eval-after-load "elpher" '(load-library "gemini-write"))

(setq elpher-gemini-tokens
  '(("alexschroeder.ch" . "hello")
    ("communitywiki.org" . "fSu1Fbsa")
    ("transjovian.org" . "hello")
    ("toki.transjovian.org" . "hello")
    ("xn--vxagggm5c.transjovian.org" . "hello")
    ("next.oddmuse.org" . "hello")
    ("emacswiki.org" . "emacs")
    ("127.0.0.1" . "hello")
    ("localhost" . "hello")
    ("campaignwiki.org" . "hello")))

(add-hook 'gemini-mode-hook 'typo-mode)
(add-hook 'gemini-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c i") 'gemini-insert-link-to-oddmuse-wiki)
	    (local-set-key (kbd "C-c l") 'gemini-insert-local-link)))

(defun gemini-insert-local-link (pagename)
  "Insert a link to a new page on this wiki."
  (interactive "sPage name: ")
  (let ((escaped (url-hexify-string pagename)))
    (if (string= pagename escaped)
	(insert "=> " pagename)
      (insert "=> " escaped " " pagename))))

(defun gemini-insert-link-to-oddmuse-wiki (wiki pagename)
  "Insert a link to WIKI and PAGENAME with completion."
  (interactive (oddmuse-pagename))
  (setq-local oddmuse-wiki wiki)
  (insert "=> " pagename " " (replace-regexp-in-string "_" " " pagename)))
  
;; Utilities

(defun gemini-url-percent-encode-title (start end)
  "Percent-encode the region and insert it at START."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (insert (url-encode-url (buffer-substring-no-properties start end)) " ")))

;; Render *foo* as italics, **foo** as bold, /foo/ as italics, and _foo_ as underline.

(advice-add 'elpher-process-text-for-display :filter-return #'asc:add-emphasis)

(defun asc:add-emphasis (str)
  "Highlight **foo**, *foo*, /foo/, and _foo_ in STR.
Don't do this if STR already has a text property at position 0
(most likely preformatted text)."
  (unless (get-text-property 0 'face str)
    (dolist (rule '(("\\(\\*\\*\\)\\<\\(.*?\\)\\>\\(\\*\\*\\)" . bold)
		    ("\\(\\*\\)\\<\\(.*?\\)\\>\\(\\*\\)" . italic)
		    ("\\(/\\)\\<\\(.*?\\)\\>\\(/\\)" . italic)
		    ("\\(_\\)\\<\\(.*?\\)\\>\\(_\\)" . underline)))
      (let ((re (car rule))
	    (face (cdr rule))
	    (start 0))
	(while (string-match re str start)
	  (unless (get-text-property (match-beginning 0) 'invisible str)
            (add-text-properties (match-beginning 1) (match-end 1) `(invisible t) str)
            (add-text-properties (match-beginning 2) (match-end 2) `(face ,face) str)
            (add-text-properties (match-beginning 3) (match-end 3) `(invisible t) str))
	  (setq start (match-end 0))))))
  str)
