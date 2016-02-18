(require 'cl-macs)
(require 'widget)

(defconst fünf-winde-regexp "^|\\[\\[\\(.*?\\)\\]\\][ \t]*|[ \t]*\\(0\\|1\\|1/2\\|½\\|1/3\\|⅓\\)[ \t]*|\\([ \t]*[0-9]+[ \t]*\\)|\\([ \t]*[0-9]+[ \t]*\\)"
  "Regular expression to parse the Status page.
\(let ((name (match-string 1))
      (share (match-string 2))
      (xp (match-string 3))
      (gold (match-string 4)))
      ...\)")

(defvar fünf-winde-buf nil
  "Source buffer.")

(defvar fünf-winde-xp nil
  "XP share.")

(defvar fünf-winde-gold nil
  "Gold share.")

(defvar fünf-winde-gold-zu-xp nil
  "Gold to XP share")

(defvar fünf-winde-party nil
  "Charakters in the party.")

(defun fünf-winde-xp-and-gold ()
  "Hand out Gold and XP."
  (interactive)
  (let ((buf (current-buffer))
	names shares)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward fünf-winde-regexp nil t)
	(let ((name (match-string 1))
	      (share (match-string 2)))
          (push name names)
          (push share shares))))
    (switch-to-buffer "*Fünf Winde*")
    (kill-all-local-variables)
    (set (make-local-variable 'fünf-winde-buf) buf)
    (make-local-variable 'fünf-winde-xp)
    (make-local-variable 'fünf-winde-gold)
    (make-local-variable 'fünf-winde-gold-zu-xp)
    (make-local-variable 'fünf-winde-party)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (setq fünf-winde-xp
	  (widget-create 'integer
			 :size 13
			 :format "XP total:   %v\n"
			 0))
    (setq fünf-winde-gold-zu-xp
	  (widget-create 'integer
			 :size 13
			 :format "Gold zu XP: %v\n"
			 0))
    (setq fünf-winde-gold
	  (widget-create 'integer
			 :size 13
			 :format "Gold total: %v\n"
			 0))
    (setq fünf-winde-party 
          (mapcar*
           (lambda (name share)
             (let ((character (list name)))
               (push (widget-create 'checkbox nil) character)
               (widget-insert " " name (make-string (max 0 (- 20 (length name))) ? ))
               (widget-insert "Share: ")
               (push (widget-create 'checkbox (string= share "1")) character)
               (widget-insert " 1 ")
               (push (widget-create 'checkbox
                                    (or (string= share "1/2")
                                        (string= share "½"))) character)
               (widget-insert " ½ ")
               (push (widget-create 'checkbox
                                    (or (string= share "1/3")
                                        (string= share "⅓"))) character)
              (widget-insert " ⅓   ")
              (push (widget-create 'integer :size 7 :format "Gold spent: %v" 0) character)
	      (widget-insert " ")
              (push (widget-create 'integer :size 7 :format "Transfer: %v\n" 0) character)
              (nreverse character)))
           (nreverse names)
           (nreverse shares)))
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (fünf-winde-process
			      fünf-winde-buf
			      (widget-value fünf-winde-xp)
			      (widget-value fünf-winde-gold-zu-xp)
			      (widget-value fünf-winde-gold)
			      fünf-winde-party))
		   "Go!")
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (local-set-key (kbd "q") 'bury-buffer)
    (local-set-key (kbd "SPC") 'widget-button-press)
    (local-set-key (kbd "<left>") 'widget-backward)
    (local-set-key (kbd "<up>") 'widget-backward)
    (local-set-key (kbd "<right>") 'widget-forward)
    (local-set-key (kbd "<down>") 'widget-forward)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 1)))

(defun fünf-winde-xp-shares (party)
  (let ((shares 0))
    (dolist (character party)
      (when (widget-value (nth 1 character))
        (setq shares (1+ shares))))
    shares))

(defun fünf-winde-gold-shares (party)
  (let ((shares 0))
    (dolist (character party)
      (when (widget-value (nth 1 character))
        (setq shares (+ shares
                        (cond ((widget-value (nth 2 character)) 1)
                              ((widget-value (nth 3 character)) 0.5)
                              ((widget-value (nth 4 character)) (/ 1.0 3))
			      (t 0))))))
    shares))

(defun fünf-winde-process (buf total-xp gold-zu-xp total-gold party)
  (switch-to-buffer buf)
  (save-excursion
    (let ((xp-shares (fünf-winde-xp-shares party))
	  (xp-per-person nil)
	  (gold-shares (fünf-winde-gold-shares party))
	  (gold-per-person nil)
	  (gold-zu-xp-per-person nil))
      (when (zerop xp-shares)
        (error "No party members selected"))
      (when (not (zerop (apply '+ (mapcar
				   (lambda (ch) (widget-value (nth 6 ch)))
				   party))))
	(error "Total transfer is not zero sum"))
      (setq gold-per-person (/ total-gold gold-shares)
	    xp-per-person (/ total-xp xp-shares)
	    gold-zu-xp-per-person (/ gold-zu-xp gold-shares))
      (goto-char (point-min))
      (while (re-search-forward fünf-winde-regexp nil t)
	(let* ((name (match-string 1))
               ;; ignore share
	       (xp (match-string 3))
	       (gold (match-string 4))
               (character (assoc name party))
               (share (cond ((widget-value (nth 2 character)) 1)
			    ((widget-value (nth 3 character)) 0.5)
			    ((widget-value (nth 4 character)) (/ 1.0 3))
			    (t 0))))
	  (when (and (widget-value (nth 1 character)))
	    (setq gold (format (concat "%9d")
			       (if (and (zerop share)
					(< xp-per-person 100))
				   0
				 (+  (string-to-number gold)
				     (* gold-per-person share)
				     (- (widget-value (nth 5 character)))
				     (widget-value (nth 6 character)))))
		  xp (format (concat "%9d")
			     (if (and (zerop share)
				      (< xp-per-person 100))
				 0
			       (+  (string-to-number xp)
				   (* gold-zu-xp-per-person share)
				   xp-per-person
				   (widget-value (nth 5 character))))))
	    (replace-match (concat "|[[" name "]]"
                                   (make-string (max 0 (- 20 (length name))) ? )
                                   "| " (cond ((widget-value (nth 2 character)) "1")
                                              ((widget-value (nth 3 character)) "½")
                                              ((widget-value (nth 4 character)) "⅓")
					      (t "0")) " "
				   "|" xp
				   "|" gold))))))))

(defun wilderlande-xp (xp)
  (interactive "nXP pro Person: ")
  (save-excursion
    (while (re-search-forward "^|\\(.*?\\)[ \t]*|[ \t]*\\([0-9]+\\)[ \t]*|[ \t]*\\([0-9]+\\)[ \t]*|" nil t)
      (let ((name (match-string 1))
	    (level (string-to-number (match-string 2)))
	    (num (string-to-number (match-string 3)))
	    (start (match-beginning 0))
	    (end (match-end 0)))
	(when (y-or-n-p (format "War %s dabei?" name))
	  (delete-region start end)
	  (insert (format "|%s | %d| %d|" name level
			  (+  num xp))))))))
