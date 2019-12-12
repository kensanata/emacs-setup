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

