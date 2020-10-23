;; Helper functions for editing SVG files

(defun as:rotate-coordinates (start end)
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "\\([0-9]+\\),\\([0-9]+\\)" end t)
    (let* ((x (string-to-number (match-string 1)))
	   (y (string-to-number (match-string 2)))
	   (x2 y)
	   (y2 (- 10 x)))
      (replace-match (format "%d,%d" x2 y2)))))

(defun as:increase-step-numbers (start end)
  (interactive "r")
  (goto-char start)
  (save-excursion
    (while (re-search-forward "step\\([0-9]+\\)" end t)
      (let ((n (string-to-number (match-string 1))))
	(replace-match (format "step%d" (1+ n)))))))

(defun as:round-numbers (start end)
  (interactive "r")
  (goto-char start)
  (save-excursion
    (while (re-search-forward "\\([0-9]+\\.[0-9][0-9]+\\)" end t)
      (let ((n (string-to-number (match-string 1))))
	(replace-match (format "%.1f" n))))))
    
