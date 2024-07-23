;;; -*- lexical-binding:t -*-

(use-package elpher :defer t)

(add-hook 'elpher-mode-hook
	  (lambda ()
	    (local-set-key "u" #'elpher-up)
	    (local-set-key "b" #'elpher-back)
	    (local-set-key "l" #'elpher-back)
	    (local-set-key "j" #'bookmark-jump)
	    (local-set-key "n" (lambda () (interactive) (elpher 1)))
	    (local-set-key "W" #'asc:elpher-search-en.wikipedia.org)
	    (local-set-key "e" #'gemini-write-text)
	    (local-set-key "w" #'gemini-write-file)
	    (local-set-key "F" #'elpher-certificate-toggle)
	    (setq-local xterm-color-preserve-properties nil)))

(add-hook 'gemini-mode-hook 'typo-mode)

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

(defun elpher-certificate-toggle ()
  "Choose a client certificate, or forget the current one."
  (interactive)
  (if elpher-client-certificate
      (elpher-forget-current-certificate)
    (let ((chosen-certificate
           (with-local-quit
             (elpher-acquire-client-certificate
              (elpher-address-to-url (elpher-page-address elpher-current-page))))))
      (unless chosen-certificate
        (error "Gemini server requires a client certificate and none was provided"))
      (setq-local elpher-client-certificate chosen-certificate))))

;; sometimes I'm in places with very bad connectivity
(setq elpher-connection-timeout 20)
