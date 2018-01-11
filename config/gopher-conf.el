(autoload 'gopher "gopher")

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
