(setq nxml-child-indent 2)
(add-to-list 'auto-mode-alist '("\\.opml\\'" . nxml-mode))

(add-hook 'nxml-mode-hook 'asc:init-nxml-mode)

(defun asc:init-nxml-mode ()
  (idle-highlight-mode 1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
  (define-abbrev nxml-mode-abbrev-table "ol"
    "" 'opml-outline)
  (define-abbrev nxml-mode-abbrev-table "bl"
    "" 'blogspot-opml-outline)
  (define-abbrev nxml-mode-abbrev-table "txt"
    "" 'txt-element)
  (define-abbrev nxml-mode-abbrev-table "it"
    "" 'item-element)
  (setq indent-tab-mode nil
	c-basic-offset 2
	truncate-lines t))

(define-skeleton opml-outline
  "Add a new entry to an OPML outline."
  "Feed: "
  "<outline title=\"" _ "\" xmlUrl=\"" str "\"/>")

(define-skeleton blogspot-opml-outline
  "Add a new Blogspot entry to an OPML outline."
  "Feed: "
  "<outline title=\"" _ "\" xmlUrl=\"https://" str ".blogspot.com/feeds/posts/default\"/>")

(define-skeleton txt-element
  "Add a new text element to an SVG file."
  "Text: "
  "<text id=\"" _ "\" x=\"" _ "\" y=\"" _ "\" class=\"" _ "\"><tspan>" str "</tspan></text>")

(define-skeleton category-element
  "Add a new category element to an RSS file."
  "Category: "
  "<category>" _ "</category>" \n)  

(defun item-element ()
  "Add a new item element to an RSS file."
  (interactive)
  (let* ((filename (read-file-name "Page name: " "/ssh:sibirocobombus:alexschroeder.ch/wiki/" nil t))
         (pagename (file-name-nondirectory (file-name-sans-extension filename)))
         (title pagename)
         (tags ())
         point)
    (with-temp-buffer
      (insert-file filename)
      (when (re-search-forward "^# \\(.*\\)" nil t)
        (setq title (match-string 1)))
      (while (re-search-forward "#\\([[:alpha:]_]+\\)" nil t)
        (setq tags (cons (match-string 1) tags))))
    (insert "<item>\n"
            "<title>" title "</title>\n"
            "<link>https://alexschroeder.ch/view/" pagename "</link>\n"
            "<guid>https://alexschroeder.ch/view/" pagename "</guid>\n"
            "<description>")
    (setq point (point))
    (insert "</description>\n"
            "<pubDate>" (format-time-string "%a, %d %b %Y %H:%M:%S %z") "</pubDate>\n")
    (dolist (tag tags)
      (insert "<category>" tag "</category>\n"))
    (insert "</item>")
    (goto-char point)))
