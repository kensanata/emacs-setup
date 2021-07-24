;;; -*- lexical-binding:t -*-

(use-package elpher :defer t
  :config (require 'elpher-http)
  :config (require 'elpher-bookmark-integration))

(add-hook 'elpher-menu-mode-hook
	  (lambda ()
	    (local-set-key (kbd "n") (lambda () (interactive) (elpher 1)))
	    (local-set-key (kbd "a") 'asc:elpher-match-alexschroeder.ch)
	    (local-set-key (kbd "w") 'asc:elpher-search-en.wikipedia.org)))

(autoload 'elpher-mode "elpher" "Put buffer in Elpher Mode")

(defun asc:elpher-new-alexschroeder.ch (title)
  (interactive "sTitle: ")
  (switch-to-buffer
   (get-buffer-create
    (generate-new-buffer-name "*alexschroeder.ch new*")))
  (elpher-mode)
  (setq-local gemini-write-text-p t)
  (elpher-visit-page
   (elpher-make-page
    (format "Alex Schroeder: %s" title)
    (elpher-address-from-url
     (concat "gemini://alexschroeder.ch/raw/"
	     (url-hexify-string title))))))

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

(setq elpher-gemini-tokens
  '(("alexschroeder.ch" . "hello")
    ("communitywiki.org" . "hello")
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

(defun gemini-insert-link-to-oddmuse-wiki (wiki pagename)
  "Insert a link to WIKI and PAGENAME with completion."
  (interactive (oddmuse-pagename))
  (setq-local oddmuse-wiki wiki)
  (insert "=> " pagename " " (replace-regexp-in-string "_" " " pagename)))

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
  
;; Utilities

(defun gemini-url-percent-encode-title (start end)
  "Percent-encode the region and insert it at START."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (insert (url-encode-url (buffer-substring-no-properties start end)) " ")))
