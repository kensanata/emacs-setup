;; bitblee uses K&R with tabs and 120 columns
(add-hook 'c-mode-hook 'asc:init-c-mode)

(defun asc:init-c-mode ()
  (idle-highlight-mode 1)
  (local-set-key (kbd "C-<tab>") 'company-complete)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (define-abbrev c-mode-abbrev-table "fu" "" 'asc:c-comment-function)
  (define-abbrev c-mode-abbrev-table "co" "" 'asc:c-comment)
  (setq c-basic-offset 4
	tab-width 4
	fill-column 120))

(eval-after-load "cc-vars"
  (lambda ()
    (add-to-list 'c-default-style '(c-mode . "k&r"))))

(setq tags-revert-without-query t)

(define-skeleton asc:c-comment-function
  "Insert a multi-line comment.

/**
 * Like this.
 */"
  nil
  "/**\n"
  " * " _ "\n"
  " */")

(define-skeleton asc:c-comment
  "Insert a single-line comment. /* like this */"
  nil "/* " _ " */")

(defvar mastodon-documentation
  '(("https://developer.gnome.org/glib/stable/glib-String-Utility-Functions.html#top"
     . "*String Utility Functions*")
    ("https://github.com/tootsuite/documentation/blob/master/Using-the-API/API.md#start-of-content"
     . "*Mastodon API*")
    ("https://developer.gnome.org/glib/stable/glib-Strings.html#top" . "*Strings*")
    ("https://developer.gnome.org/glib/stable/glib-Unicode-Manipulation.html#top"
     . "*Unicode Manipulation*")
    ("https://developer.gnome.org/glib/stable/glib-Singly-Linked-Lists.html#top" . "*Single Linked Lists*")
    ("https://developer.gnome.org/glib/stable/glib-Hash-Tables.html#top" . "*Hash Tables*"))
 "List of cons cells with doc URL and buffer name.")

(defun mastodon-documentation ()
  "Load all the documentation documents."
  (interactive)
  (dolist (cell mastodon-documentation)
    (unless (get-buffer (cdr cell))
      (set-buffer (get-buffer-create (cdr cell)))
      ;; this makes eww reuse the buffer we just created
      (eww-mode)
      (eww (car cell)))))
