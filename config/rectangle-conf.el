(global-set-key (kbd "C-x r >") 'rectangle-text-flush-right)
(global-set-key (kbd "C-x r <") 'rectangle-text-flush-left)

(defun rectangle-text-flush-right (start end)
  "Justify right the text in the rectangle."
  (interactive "r")
  (apply-on-rectangle
   (lambda (col1 col2)
     (let ((from (+ (point) col1))
	   (to (+ (point) col2)))
       (goto-char to)
       (let ((n (skip-syntax-backward " " from)))
	 (delete-region (point) to)
	 (goto-char from)
	 (insert-char ?  (- n)))))
   start
   end))

(defun rectangle-text-flush-left (start end)
  "Justify left the text in the rectangle."
  (interactive "r")
  (apply-on-rectangle
   (lambda (col1 col2)
     (let ((from (+ (point) col1))
	   (to (+ (point) col2)))
       (goto-char from)
       (let ((n (skip-syntax-forward " " to)))
	 (goto-char to)
	 (insert-char ?  n)
	 (delete-region from (+ from n)))))
   start
   end))

;; aaaaaa
;; abb  a
;; abbb a
;; aaaaaa
