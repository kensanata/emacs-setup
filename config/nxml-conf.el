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
