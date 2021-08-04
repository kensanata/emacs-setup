;;; elpher-http.el --- adding HTTP/HTTPS support to elpher  -*- lexical-binding:t -*-

;; Copyright (C) 2021 Alex Schroeder <alex@gnu.org>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Elpher aims to provide a practical and friendly gopher and gemini
;; client for GNU Emacs. It's annoying if following a HTTP/HTTPS link
;; takes you to eww or some other browser with different keybindings,
;; though. To provide a simple example: to go "back" from a HTTPS
;; link, you need to use `q' in order to bury the eww buffer…

;; This code allows Elpher to render HTML returned via HTTP/HTTPS.

(require 'elpher)

(define-key elpher-mode-map "&" #'elpher-browse-with-external-browser)

(defun elpher-browse-with-external-browser (&optional url)
  "Browse the current URL with an external browser.
The browser to used is specified by the
`browse-url-secondary-browser-function' variable."
  (interactive)
  (funcall browse-url-secondary-browser-function
           (or url (elpher-address-to-url
		    (elpher-page-address elpher-current-page)))))

(defface elpher-http
  '((t :inherit font-lock-comment-face))
  "Face used for HTTP/HTTPS type directory records.")

(defun elpher-http-remove-redundant-ports (orig address &rest args)
  "Handle HTTP/HTTPS ports."
  (setq address (apply orig address args))
  (when (and (not (elpher-address-about-p address))
             (eq (url-portspec address)
		 (pcase (url-type address)
                   ("http" 80)
                   ("https" 443)
                   (_ -1))))
    (setf (url-portspec address) nil))
  address)

(advice-add 'elpher-remove-redundant-ports :around #'elpher-http-remove-redundant-ports)

;; (url-portspec (elpher-remove-redundant-ports (url-generic-parse-url "https://alexschroeder.ch:443"))) → nil
;; (url-portspec (elpher-remove-redundant-ports (url-generic-parse-url "https://transjovian.org:1965"))) → 1965

(defun elpher-http-address-type (orig address &rest args)
  "Handle HTTP/HTTPS scheme."
  (let ((type (apply orig address args)))
    (if (eq type 'other-url)
	(let ((protocol (url-type address)))
	  (cond ((equal protocol "http")
		 'http)
		((equal protocol "https")
		 'http)
		(t type)))
      type)))

(advice-add 'elpher-address-type :around #'elpher-http-address-type)

;; (elpher-address-type (url-generic-parse-url "http://alexschroeder.ch"))

;; add the HTTP entry to `elpher-type-map'
(add-to-list 'elpher-type-map '(http elpher-get-http-page elpher-render-gemini "http" elpher-http))

;; (assq 'http elpher-type-map)

;; HTTP/HTTPS

(defun elpher-get-http-page (renderer)
  "Opens a HTTP/HTTPS connection to the current page address."
  (let* ((address (elpher-page-address elpher-current-page))
         (content (elpher-get-cached-content address)))
    (if (and content (funcall renderer nil))
        (elpher-with-clean-buffer
         (insert content)
         (elpher-restore-pos))
      (elpher-with-clean-buffer
       (insert "LOADING... (use 'u' to cancel)\n"))
      (condition-case the-error
	  (let ((url (elpher-address-to-url address)))
	    (url-retrieve url #'elpher-url-retrieve-callback (list renderer (current-buffer))))
        (error
         (elpher-network-error address the-error))))))

(defun elpher-url-retrieve-callback (status renderer buf)
  "Callback for `url-retrieve'.
In a buffer containing the HTTP headers and the HTTP body,
ignore STATUS and call RENDERER with HTTP body and MIME type.
Do not use `eww-render'."
  (let* ((headers (elpher-parse-headers))
	 (content-type
          (if (zerop (length (cdr (assoc "content-type" headers))))
	      "text/plain"
            (cdr (assoc "content-type" headers))))
	 (body (buffer-substring (point) (point-max))))
    (kill-buffer)			; delete the data buffer
    (with-current-buffer buf
      (save-excursion
	(funcall renderer body content-type))
      (unless (and (consp browse-url-browser-function)
		   (assoc "^https?:" browse-url-browser-function))
	(make-variable-buffer-local 'browse-url-browser-function)
	(if (consp browse-url-browser-function)
	    (add-to-list 'browse-url-browser-function
			 '("^https?:" . elpher-browse-url-elpher))
	  (setq browse-url-browser-function
		(list '("^https?:" . elpher-browse-url-elpher)
		      (cons "" browse-url-browser-function)))))
      (elpher-cache-content
       (elpher-page-address elpher-current-page)
       (buffer-string)))))

(defun elpher-parse-headers ()
  "Parse the headers in this buffer.
These are regular headers as are often used in Internet protocols
such as mail headers or HTTP headers. Return an alist where the
cars of each item are downcased, i.e. \"Host\" turns into \"host\".
This implementation does parse continuation lines."
  ;; a copy of eww-parse-headers
  (let ((headers nil))
    (goto-char (point-min))
    (while (and (not (eobp))
		(not (eolp)))
      (when (looking-at "\\([^:]+\\): *\\(.*\\)")
	(push (cons (downcase (match-string 1))
		    (match-string 2))
	      headers))
      (forward-line 1))
    (unless (eobp)
      (forward-line 1))
    headers))

(provide 'elpher-http)

;; elpher-http.el ends here
