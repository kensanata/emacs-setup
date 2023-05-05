
(setq mhtml-tag-relative-indent 'ignore)

(add-hook 'html-mode-hook 'asc:init-html-mode)

(defun asc:init-html-mode ()
  (idle-highlight-mode 1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (setq c-basic-offset 2
	tab-width 2))

(defun asc:html-tag ()
  "Insert a tag appropriate after a keyword."
  (let ((end (point))
        tag)
    (backward-word-strictly)
    (setq tag (buffer-substring (point) end))
    (insert "<")
    (forward-word-strictly)
    (insert ">")
    (save-excursion
      (insert "</" tag ">"))))

(defun asc:html-escape (start end)
  "HTML escape the region.
Turns < & > → &lt; &amp; &gt;."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((m (make-marker)))
      (set-marker m end)
      (while (< (point) m)
        (case (char-after)
          (?< (delete-char 1) (insert "&lt;"))
          (?& (delete-char 1) (insert "&amp;"))
          (?> (delete-char 1) (insert "&gt;"))
          (t (forward-char))))
      (set-marker m nil))))
