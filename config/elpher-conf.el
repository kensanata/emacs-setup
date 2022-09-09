;;; -*- lexical-binding:t -*-

;; prefer my own copies
;; (add-to-list 'load-path "/home/alex/src/elpher")
;; (add-to-list 'load-path "/home/alex/src/gemini-write")
(add-to-list 'load-path "/home/alex/src/elpher-http")

(use-package elpher :defer t
  ;; add Elpher as a web client via ../lib/elpher-http.el
  :config (require 'elpher-http)
  :commands elpher-browse-url-elpher
  ;; also creates an autoload for elpher
  :bind ("C-c e" . elpher))

(global-set-key (kbd "C-c u") #'browse-url)
(setq browse-url-browser-function 'elpher-browse-url-elpher)

(use-package gemini-write
  :after elpher
  :commands gemini-write-mode
  :config (gemini-write-mode 1))

(use-package gemini-mode
  :defer t
  :commands gemini-mode
  :mode "\\.gmi\\'"
  :config
  ;; I want a mandatory space for headings
  (setq gemini-highlights
	(let* ((gemini-preformatted-regexp "^```[^`]+```$")
               (gemini-heading-rest-regexp "^####+[[:blank:]]+.*$")
               (gemini-heading-3-regexp "^###[[:blank:]]+.*$")
               (gemini-heading-2-regexp "^##[[:blank:]]+.*$")
               (gemini-heading-1-regexp "^#[[:blank:]]+.*$")
               (gemini-quote-regexp "^>[[:blank:]]+.*$"))
	  ;; preformatted must be declared first has it must absolutely be set
	  ;; before any other face (for exemple to avoid a title inside a
	  ;; preformatted block to hijack it).
	  `((,gemini-preformatted-regexp . 'font-lock-builtin-face)
	    (,gemini-heading-rest-regexp . 'gemini-heading-face-rest)
	    (,gemini-heading-3-regexp . 'gemini-heading-face-3)
	    (,gemini-heading-2-regexp . 'gemini-heading-face-2)
	    (,gemini-heading-1-regexp . 'gemini-heading-face-1)
	    (,gemini-regex-link-line 1 'link)
	    (,gemini-quote-regexp . 'gemini-quote-face)))))

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
(add-hook 'gemini-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") 'gemini-write)
	    (local-set-key (kbd "C-c i") 'gemini-insert-link-to-oddmuse-wiki)
	    (local-set-key (kbd "C-c l") 'gemini-insert-local-link)))

(defun elpher-new-page-on-alexschroeder (page)
  "Create a new PAGE."
  (interactive "sPage: ")
  (elpher-go (concat "gemini://alexschroeder.ch/raw/"
		     (url-encode-url page))))

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
    (let ((chosen-certificate (elpher-choose-client-certificate)))
      (when chosen-certificate
	(setq elpher-client-certificate chosen-certificate)))))

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

(setq
 gemini-write-use-auth-source nil
 gemini-write-tokens
 '(("alexschroeder.ch" . "diary")
   ("communitywiki.org" . "fSu1Fbsa")
   ("transjovian.org" . "hello")
   ("toki.transjovian.org" . "hello")
   ("xn--vxagggm5c.transjovian.org" . "hello")
   ("next.oddmuse.org" . "hello")
   ("emacswiki.org" . "emacs")
   ("127.0.0.1" . "hello")
   ("localhost" . "hello")
   ("campaignwiki.org" . "hello")))

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

(defun gemini-insert-link-to-wikipedia (language pagename)
  "Insert a link to PAGENAME on Wikipedia."
  (interactive "sLanguage: \nsPagename: ")
  (insert "=> //vault.transjovian.org/text/" language "/" pagename " " pagename))

;; Utilities

(defun gemini-url-percent-encode-title (start end)
  "Percent-encode the region and insert it at START."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (insert (url-encode-url (buffer-substring-no-properties start end)) " ")))

;; Do not cache Ijirait!

(defvar asc:elpher-no-cache-prefixes '("gemini://campaignwiki.org/play")
  "Prefixes of URLs not to cache.")

(advice-add 'elpher-cache-content :around #'asc:elpher-cache-content)

(defun asc:elpher-cache-content (oldfun address content)
  "Do not cache Ijirait URLs."
  (let ((url (elpher-address-to-url address)))
    (unless (catch 'found
	      (dolist (prefix asc:elpher-no-cache-prefixes)
		(when (string-prefix-p prefix url)
		  (throw 'found t))))
      (funcall oldfun address content))))

;; Stream Ijirait

;; (autoload 'elpher-address-from-gemini-url "elpher")

;; (defun ijirait-stream ()
;;   "Open a stream to Ijirait."
;;   (interactive) 
;;   (let* ((buf (switch-to-buffer-other-window "*Ijirait Stream*"))
;; 	 (url "gemini://campaignwiki.org:1965/play/ijirait")
;; 	 (address (elpher-address-from-gemini-url url))
;; 	 (host (elpher-address-host address))
;; 	 (port (elpher-address-port address))
;; 	 (process (get-buffer-process buf)))
;;     (unless (process-live-p process)
;;       (erase-buffer)
;;       (gemini-mode)
;;       (insert "# Ijirait\n")
;;       (insert "A client certificate is required.\n")
;;       (setq-local elpher-current-page (elpher-make-page "Ijirait Stream" address)
;; 		  network-security-level 'low
;; 		  gnutls-verify-error nil)
;;       ;; we always ask for a client certificate and therefore always
;;       ;; set elpher-client-certificate
;;       (elpher-choose-client-certificate)
;;       (let ((params (cons 'gnutls-x509pki
;; 			  (gnutls-boot-parameters
;; 			   :type 'gnutls-x509pki
;;                            :hostname host
;;                            :keylist (elpher-get-current-keylist address)))))
;; 	(make-network-process :name "ijirait-stream"
;; 			      :host host
;; 			      :service port
;; 			      :buffer buf
;; 			      :nowait t
;; 			      :coding 'utf-8
;; 			      :tls-parameters params
;; 			      :sentinel 'ijirait-sentinel)))))

;; (defun ijirait-sentinel (proc event)
;;   (message "%S" event)
;;   (when (string-prefix-p "open" event)
;;     (let ((inhibit-eol-conversion t))
;;       (process-send-string proc "gemini://campaignwiki.org:1965/play/ijirait\r\n"))))

;; (get-buffer-process "*Ijirait Stream*")
;; (list-processes)
