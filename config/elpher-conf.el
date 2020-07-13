;;; -*- lexical-binding:t -*-

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
(autoload 'gemini-write-init "gemini-write" "Initialize Gemini Write Mode" t)

(add-to-list 'load-path "/home/alex/src/gemini.el")
(autoload 'gemini-mode "gemini-mode" "Gemini Mode" t)

(add-hook 'elpher-mode-hook
	  (lambda ()
	    (local-set-key (kbd "u") 'elpher-up)
	    (local-set-key (kbd "b") 'elpher-back)
	    (local-set-key (kbd "l") 'elpher-back)
	    (gemini-write-init)))

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

;; add support for client certificates

(defcustom elpher-client-certificates nil
  "The client certificates to use.
Like, if you're playing astrobotany...

To generate, run the following:
openssl req -new -x509 -nodes -out cert.pem -keyout key.pem

Answer the questions (or not). When it asks you for the Common
Name, provide some sort of name. That's the only important part."
  :group 'elpher
  :type '(choice (const :tag "None" nil)
		 (list :tag "Client certificate"
		       ;; same order as used for open-network-stream
		       (file :must-match t :tag "Key file ")
		       (file :must-match t :tag "Cert file"))))

;; (setq elpher-client-certificates
;;       (list (expand-file-name "~/key.pem")
;; 	    (expand-file-name "~/cert.pem")))

(defun elpher-get-gemini-response (address renderer &optional force-ipv4)
  "Retrieve gemini ADDRESS, then render using RENDERER.
If FORCE-IPV4 is non-nil, explicitly look up and use IPv4 address corresponding
to ADDRESS."
  (unless elpher-gemini-TLS-cert-checks
    (setq-local network-security-level 'low))
  (if (not (gnutls-available-p))
      (error "Cannot establish gemini connection: GnuTLS not available")
    (unless (< (elpher-address-port address) 65536)
      (error "Cannot establish gemini connection: port number > 65536"))
    (defvar gnutls-verify-error)
    (condition-case nil
        (let* ((kill-buffer-query-functions nil)
               (gnutls-verify-error nil) ; We use the NSM for verification
               (port (elpher-address-port address))
               (host (elpher-address-host address))
               (response-string-parts nil)
               (bytes-received 0)
               (hkbytes-received 0)
               (proc (open-network-stream "elpher-process"
                                          nil
                                          (if (or elpher-ipv4-always force-ipv4)
                                              (dns-query host)
                                            host)
                                          (if (> port 0) port 1965)
                                          :type 'tls
                                          :nowait t
					  :client-certificate elpher-client-certificates))
               (timer (run-at-time elpher-connection-timeout nil
                                   (lambda ()
                                     (elpher-process-cleanup)
                                     (unless (or elpher-ipv4-always force-ipv4)
                                        ; Try again with IPv4
                                       (message "Connection timed out.  Retrying with IPv4.")
                                       (elpher-get-gemini-response address renderer t))))))
          (setq elpher-network-timer timer)
          (set-process-coding-system proc 'binary)
          (set-process-filter proc
                              (lambda (_proc string)
                                (when timer
                                  (cancel-timer timer)
                                  (setq timer nil))
                                (setq bytes-received (+ bytes-received (length string)))
                                (let ((new-hkbytes-received (/ bytes-received 102400)))
                                  (when (> new-hkbytes-received hkbytes-received)
                                    (setq hkbytes-received new-hkbytes-received)
                                    (with-current-buffer "*elpher*"
                                      (let ((inhibit-read-only t))
                                        (goto-char (point-min))
                                        (beginning-of-line 2)
                                        (delete-region (point) (point-max))
                                        (insert "("
                                                (number-to-string (/ hkbytes-received 10.0))
                                                " MB read)")))))
                                (setq response-string-parts
                                      (cons string response-string-parts))))
          (set-process-sentinel proc
                                (lambda (proc event)
                                  (condition-case the-error
                                      (cond
                                       ((string-prefix-p "open" event)    ; request URL
                                        (let ((inhibit-eol-conversion t))
                                          (process-send-string
                                           proc
                                           (concat (elpher-address-to-url address)
                                                   "\r\n"))))
                                       ((string-prefix-p "deleted" event)) ; do nothing
                                       ((and (not response-string-parts)
                                             (not (or elpher-ipv4-always force-ipv4)))
                                        ; Try again with IPv4
                                        (message "Connection failed. Retrying with IPv4.")
                                        (cancel-timer timer)
                                        (elpher-get-gemini-response address renderer t))
                                       (t
                                        (funcall #'elpher-process-gemini-response
                                                 (apply #'concat (reverse response-string-parts))
                                                 renderer)
                                        (elpher-restore-pos)))
                                    (error
                                     (elpher-network-error address the-error))))))
      (error
       (error "Error initiating connection to server")))))
