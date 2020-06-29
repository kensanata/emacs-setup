;; no need for cookies within Emacs
;; use M-x url-cookie-list to check
(setq url-cookie-trusted-urls '()
      url-cookie-untrusted-urls '(".*"))

(advice-add 'eww-browse-url :around 'asc:eww-browse-url)

(defun asc:eww-browse-url (original url &optional new-window)
  "Handle gemini links."
  (cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url)
	 (require 'elpher)
	 (elpher-go url))
	(t (funcall original url new-window))))

