(define-generic-mode text-mapper-mode
  '("#")
  nil
  '("include"
    "^[0-9][0-9][0-9][0-9]")
  '("text-mapper-.*\\.txt$")
  nil
  "A major mode to edit Text Mapper data files.")

(defun text-mapper-new-map (width height)
  "Create a new map of given WIDTH and HEIGHT."
  (interactive "nWidth: \nnHeight: ")
  (dotimes (x width)
    (dotimes (y height)
       (insert (format "%02d%02d empty\n" (1+ x) (1+ y))))))

(defun text-mapper-change (from to expression)
  "Replace FROM with TO where EXPRESSION is true.
Example: from \"empty\" to \"mountains\" for \"(< (* x 2) y)\"."
  (interactive "sFrom: \nsTo: \nsExpression: ")
  (setq expression (read expression))
  (let ((n 0))
    (while (re-search-forward "^\\([0-9][0-9]\\)\\([0-9][0-9]\\) " nil t)
      (let ((x (string-to-number (match-string 1)))
            (y (string-to-number (match-string 2))))
        (when (and (eval expression)
                   (re-search-forward from (line-end-position) t))
          (replace-match to)
          (incf n))))
    (message "%d changes made" n)))
