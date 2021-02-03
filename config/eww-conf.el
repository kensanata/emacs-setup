;; I want to use eww.
(global-set-key (kbd "C-c w") 'eww)
(global-set-key (kbd "C-c W") 'eww-list-bookmarks)

;; no need for cookies within Emacs
;; use M-x url-cookie-list to check
(setq url-cookie-trusted-urls '()
      url-cookie-untrusted-urls '(".*")
      ;; ↓ run (url-setup-privacy-info) after changing
      url-privacy-level 'paranoid
      browse-url-browser-function 'eww-browse-url)

(advice-add 'eww-browse-url :around 'asc:eww-browse-url)

(defun asc:eww-browse-url (original url &optional new-window)
  "Handle gemini links."
  (cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url)
	 (require 'elpher)
	 (elpher-go url))
	(t (funcall original url new-window))))

;; labels don't come in paragraphs
(eval-after-load "shr"
  '(defun shr-tag-label (dom)
     (shr-generic dom)))
