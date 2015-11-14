;;; Oddmuse

(require 'oddmuse-curl)

(setq oddmuse-username "AlexSchroeder")
(add-to-list 'auto-mode-alist '("/Users/alex/.emacs.d/oddmuse" . oddmuse-mode))
(add-to-list 'vc-handled-backends 'oddmuse)
(setq oddmuse-directory "~/.emacs.d/oddmuse")

(defun vc-oddmuse-registered (file)
  "Handle files in `oddmuse-directory'."
  (string-match (concat "^" (expand-file-name oddmuse-directory))
		(file-name-directory file)))

(autoload 'oddmuse-edit "oddmuse-curl"
  "Edit a page on an Oddmuse wiki." t)
(autoload 'oddmuse-post "oddmuse-curl"
  "Post the current buffer to an Oddmuse wiki." t)
(autoload 'oddmuse-new "oddmuse-curl"
  "Create a day page on an Oddmuse wiki." t)
(autoload 'oddmuse-rc "oddmuse-curl"
  "Recent Changes on an Oddmuse wiki." t)
(autoload 'oddmuse-mode "oddmuse-curl"
  "Yadda yadda." t)

(setq oddmuse-wikis
      '(("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/emacs"
	 utf-8 "uihnscuskc" nil)
	("Campaign Wiki" "https://campaignwiki.org/wiki"
	 utf-8 "frodo" "Alex")
	("DarkDungeonsSRD" "https://campaignwiki.org/wiki/DarkDungeonsSRD"
	 utf-8 "frodo" "Alex")
	("DungeonMaps" "https://campaignwiki.org/wiki/DungeonMaps"
	 utf-8 "frodo" "Alex")
	("Gridmapper" "https://campaignwiki.org/wiki/Gridmapper"
	 utf-8 "frodo" "Alex")
	("Greyheim" "https://campaignwiki.org/wiki/Greyheim"
	 utf-8 "frodo" "Alex")
	("Montag in Zürich" "https://campaignwiki.org/wiki/MontagInZ%C3%BCrich"
	 utf-8 "frodo" "Alex")
	("Fünf Winde" "https://campaignwiki.org/wiki/F%C3%BCnfWinde"
	 utf-8 "frodo" "Alex")
	("Monsters" "https://campaignwiki.org/wiki/Monsters"
	 utf-8 "frodo" "Alex")
	("BeremAndBeyond" "https://campaignwiki.org/wiki/BeremAndBeyond"
	 utf-8 "frodo" "Alex")
	("Links to Wisdom" "https://campaignwiki.org/wiki/LinksToWisdom"
	 utf-8 "frodo" "Alex")
	("Karameikos" "https://campaignwiki.org/wiki/Karameikos"
	 utf-8 "frodo" "Alex")
	("Wilderlande" "https://campaignwiki.org/wiki/Wilderlande"
	 utf-8 "frodo" "Alex")
	("Rollenspiele" "https://campaignwiki.org/wiki/Rollenspiele"
	 utf-8 "frodo" "Alex")
	("Community Wiki" "http://www.communitywiki.org/cw"
	 utf-8 "question" nil)
	("Oddmuse Wiki" "https://www.oddmuse.org/wiki"
	 utf-8 "ham" nil)
	("Alex" "https://alexschroeder.ch/wiki"
	 utf-8 "question" "Alex Schroeder"))
      oddmuse-rc-command "curl --silent %w\"?action=rc;days=7;rollback=1;showedit=1;raw=1\"")

(add-hook 'oddmuse-mode-hook 'flyspell-mode)
(add-hook 'oddmuse-mode-hook 'oddmuse-my-init)

(defun oddmuse-my-init ()
  (cond ((string= oddmuse-wiki "Alex")
	 (local-set-key (kbd "C-c C-t") 'oddmuse-tag)
	 (font-lock-add-keywords
	  nil
	  '(("!!\\S-.*?\\S-?!!" . 'bold)
	    ("!\\S-.*?\\S-?!" 0 '(face bold nobreak t)))
	  'append))
	((member oddmuse-wiki '("Fünf Winde" "BeremAndBeyond" "Wilderlande"))
	 (turn-on-auto-fill)
	 (ispell-change-dictionary "swiss"))))

(defun oddmuse-toc () (interactive) (occur "^=.*"))

(defvar oddmuse-tags nil
  "Favorite tags to use for completion.
This is used for `completing-read'. It's usually a list of
strings.")

(defun oddmuse-tag (&rest tags)
  (interactive
   (let ((tags nil))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "\\[\\[tag:\\(.*?\\)\\]\\]" nil t)
	 (setq tags (cons (match-string 1) tags))))
     (let ((tag (completing-read "Tag: " oddmuse-tags
				 (lambda (s)
				   (not (member s tags))))))
       (while (not (string= tag ""))
	 (setq tags (cons tag tags))
	 (setq tag (completing-read "Tag: " oddmuse-tags
				    (lambda (s)
				      (not (member s tags)))))))
     tags))
  (save-excursion
    (when tags
      (goto-char (point-min))
      (when (re-search-forward "Tags:\\( \\[\\[tag:.*?\\]\\]\\)+\n" nil t)
	(replace-match ""))
      (goto-char (point-max))
      ;; make sure a new paragraph starts
      (unless (and (> (point) 1)
		   (string= "\n\n" (buffer-substring (- (point) 2) (point))))
	(newline (if (eq (char-before) ?\n) 1 2)))
      (insert "Tags:")
      (mapcar (lambda (s)
		(insert " [[tag:" s "]]"))
	      tags))))

(setq oddmuse-tags '("RPG" "Old School" "RSP" "Map"
		     "Software" "Copyright"
		     "Books" "Movies" "Music" "Life" "Podcast"
		     "Web" "Oddmuse" "Wikis" "Blogs"
		     "Switzerland" "Israel" "Palestine" "USA" "Germany"))

(defun oddmuse-creole-table (start end rows)
  "Transpose cells into a table.
The region must contain one cell content per line, cells by rows
first. Use `wiki-transpose-table-cells' if your data is columns
first."
  (interactive "r\nnHow many rows? ")
  (wiki-transpose-table start end rows "|=" "|"))

(defun wiki-transpose-table-cells (start end rows)
  "Transpose cells.
The region must contain one cell content per line, cells by
column first. Transposition reorders them to be row first. Use
`oddmuse-creole-table' to get them into a table."
  (interactive "r\nnHow many rows? ")
  (let ((lines (split-string (buffer-substring start end) "\n")))
    (delete-region start end)
    (dotimes (row rows)
      (dotimes (col (/ (length lines) rows))
	(insert (nth (+ row (* rows col)) lines))
	(newline)))))

(defun wiki-transpose-table (start end rows title-separator cell-separator)
  "Transpose cells into a table.
The region must contain one cell content per line,
cells by rows first."
  (interactive "r\nnHow many rows? ")
  (let* ((cells (split-string (buffer-substring start end) " *\n *" t))
	 (m (length cells)))
    (unless (= 0 (mod m rows))
      (error "Cannot fit %d cells into %d rows"
	     m rows))
    (unless cells
      (error "No cells in the region"))
    (let ((columns (/ m rows))
	  (row 0)
	  result)
      (while cells
	(setq result cells
	      cells (nthcdr columns cells))
	(setcdr (nthcdr (1- columns) result) nil)
      	(let ((sep (if (= row 0) title-separator cell-separator)))
	  (insert sep (mapconcat (lambda (s)
				   (if (and (> row 0)
					    (string-match "[a-z]" s))
				       s; headers are always centered
				     (concat " " s)))
				 result
				 (concat " " sep))
		  " " cell-separator "\n"))
	(setq row (1+ row)))))
  (delete-region start end))