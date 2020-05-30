(require 'thingatpt)

;; remember C-+ runs the command increment-number-at-point

(defun transfer-number-here (amount)
  "Increase this number by a certain amount and decrease the next
number by the same amount."
  (interactive "nTransfer how much: ")
  (let ((num (number-at-point)))
    (unless num
      (error "Point must be on a number"))
    (delete-region (match-beginning 0) (match-end 0))
    (insert (number-to-string (+ num amount)))
    (unless (re-search-forward "-?[0-9]+\\.?[0-9]*" (+ (point) 500))
      (error "No following number found"))
    (setq num (string-to-number
	       (buffer-substring (match-beginning 0) (match-end 0))))
    (delete-region (match-beginning 0) (match-end 0))
    (insert (number-to-string (- num amount)))))

(defvar number-digits "0123456789")

(defun number-unmark-all ()
  "Remove all marks."
  (interactive)
  (remove-overlays (point-min) (point-max) 'number t))

(defun number-unmark ()
  "Remove the current mark."
  (interactive)
  (dolist (o (overlays-at (point)))
    (when (overlay-get o 'number)
      (delete-overlay o))))

(defun number-mark ()
  "Mark number at point for distribution."
  (interactive)
  (skip-chars-backward number-digits)
  (let ((start (point)))
    (skip-chars-forward number-digits)
    (when (= start (point))
      (error "No number at point"))
    (let ((o (make-overlay start (point) nil nil t)))
      (overlay-put o 'number t)
      (overlay-put o 'face 'query-replace))))

(defun number-mark-add (amount)
  "Add a number to all marked numbers."
  (interactive "nAdd how much? ")
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'number)
      (let ((num (string-to-number (buffer-substring (overlay-start o) (overlay-end o)))))
	(goto-char (overlay-start o))
	(delete-region (overlay-start o) (overlay-end o))
	(insert (number-to-string (+ num amount)))))))

(defun number-mark-distribute (amount)
  "Distribute a number to all marked numbers."
  (interactive "nDistribute how much? ")
  (let* ((os (overlays-in (point-min) (point-max)))
	 (n (/ amount (length os))))
    (dolist (o os)
      (when (overlay-get o 'number)
	(let ((num (string-to-number (buffer-substring (overlay-start o) (overlay-end o)))))
	  (goto-char (overlay-start o))
	  (delete-region (overlay-start o) (overlay-end o))
	  (insert (number-to-string (+ num n))))))))

(defun number-mark-column (regexp amount)
  "FIXME"
  (interactive "sRegexp: \nnAmount: ")
  )

(define-minor-mode number-mode
  "A mode to work with numbers in a text buffer"
  nil
  "N"
  (list (cons (kbd "C-=") 'number-mark)
	(cons (kbd "M-C-+") 'number-mark-add)
	(cons (kbd "M-C-=") 'number-mark-distribute)
	(cons (kbd "C-c C-=") 'number-mark-column)))

;; (define-key number-mode-map (kbd "C-=") 'number-mark)
;; (define-key number-mode-map (kbd "M-C-=") 'number-mark-add)

