;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

;; move line with M-up and M-down
(use-package move-text
  :config (move-text-default-bindings))

(add-hook
 'text-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil '(("\\<\\(will\\|ll\\)\\>" . 'font-lock-warning-face)))))

(add-hook 'text-mode-hook 'abbrev-mode)

(global-set-key (kbd "C-c i") 'insert-date)

;; insert date
(defun insert-date (&optional arg)
  "Insert the current date.
With optional ARG, insert the time as well.
This uses ISO date and a 24h clock."
  (interactive "P")
  (insert (format-time-string
	   (if arg "%F %R" "%F"))))

(defun remove-display-text-property (start end)
  "Remote all text properties from START to END.
This is useful when copying stuff with a display property set
from elsewhere."
  (interactive "r")
  (set-text-properties start end nil))

(defun sort-words (start end)
  "Sort words in the region between START and END.
For the purposes of this function, a word is anything that's not
whitespace. This replaces all whitespace with a literal space."
  (interactive "r")
  (save-excursion
    (let ((words))
      (goto-char start)
      (while (re-search-forward "\\S-+" end t)
	(push (match-string 0) words))
      (delete-region start end)
      (dolist (word (sort words 'string<))
	(insert word " ")))
    (delete-char -1)))

(defun goto-random-line ()
  "Go to a random line in the buffer."
  (interactive)
  (goto-line (1+ (random (count-lines (point-min) (point-max))))))
