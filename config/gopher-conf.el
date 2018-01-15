(autoload 'gopher "gopher" nil t)

(add-hook 'gopher-edit-mode-hook
	  'asc:gopher-edit-setup)

(defun asc:gopher-edit-setup ()
  (insert "```\n"
	  "username: Alex\n"
	  "summary: linebreaks\n"
	  "answer: hello\n"
	  "minor: 1\n"
	  "```\n"
	  "\n"
	  "Tags: [[tag:todo]]"))

(defun asc:sdf-phlogs ()
  "Visit http://phlogosphere.org/ using gopher."
  (interactive)
  (gopher-goto-url "phlogosphere.org"))
