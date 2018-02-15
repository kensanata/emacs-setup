;;; Oddmuse

(when (require 'shr nil t)
  (require 'oddmuse-curl nil t))

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
      '(("Emacs Wiki" "https://www.emacswiki.org/emacs"
	 utf-8 "uihnscuskc" nil)
	("Campaign Wiki" "https://campaignwiki.org/wiki"
	 utf-8 "frodo" "Alex")
	("Gridmapper" "https://campaignwiki.org/wiki/Gridmapper"
	 utf-8 "frodo" "Alex")
	("Greyheim" "https://campaignwiki.org/wiki/Greyheim"
	 utf-8 "frodo" "Alex")
	("Rasiermesserküste" "https://campaignwiki.org/wiki/Rasiermesserk%c3%bcste"
	 utf-8 "frodo" "Alex")
	("Halberds and Helmets" "https://campaignwiki.org/wiki/Halberds_and_Helmets"
	 utf-8 "frodo" "Alex")
	("Fünf Winde" "https://campaignwiki.org/wiki/F%C3%BCnfWinde"
	 utf-8 "frodo" "Alex")
	("Monsters" "https://campaignwiki.org/wiki/Monsters"
	 utf-8 "frodo" "Alex")
	("Links to Wisdom" "https://campaignwiki.org/wiki/LinksToWisdom"
	 utf-8 "frodo" "Alex")
	("Wilderlande" "https://campaignwiki.org/wiki/Wilderlande"
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
	 ;; see spell-conf.el
	 (german))))

(defun oddmuse-comment ()
  "Switch between article and talk pages."
  (interactive)
  (if (string-match "^Comments_on_\\(.*\\)" oddmuse-page-name)
      (oddmuse-follow oddmuse-wiki (match-string 1 oddmuse-page-name))
    (oddmuse-follow oddmuse-wiki (concat "Comments_on_" oddmuse-page-name))))

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

(defun oddmuse-sign ()
  "Add signature."
  (interactive)
  (insert "-- Alex Schroeder "
	  (format-time-string "%Y-%m-%d %H:%M" (current-time))
	  "\n"))

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

(defun oddmuse-to-latex (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]+\\) ?\\(ft\\.?\\|gold\\|[sgp]p\\)\\b" nil t)
	(let ((num (match-string 1))
	      (unit (match-string 2)))
	  (when (string= unit "gold")
	    (setq unit "gp"))
	  (replace-match (concat "\\\\SI{" num "}{" unit "}"))))
      (goto-char (point-min))
      (while (re-search-forward "\n\n\\(HD.*\\)" nil t)
	(let ((str (concat "\n\n\\begin{quote}\n"
			   (match-string 1)
			   "\n\\end{quote}")))
	  (replace-match "")
	  (insert str)))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\(.+\\)\\*\\*" nil t)
	(replace-match (concat "\\\\textbf{" (match-string 1) "}")))
      (goto-char (point-min))
      (while (re-search-forward "\\*\\(.+?\\)\\*" nil t)
	(replace-match (concat "\\\\textbf{" (match-string 1) "}")))
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]\\)\\(st\\|nd\\|rd\\|th\\)" nil t)
	(replace-match (concat (match-string 1) "\\\\" (match-string 2))))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\(https?://\\S-+\\) \\(.+?\\)\\]" nil t)
	(replace-match (concat "\\\\href{" (match-string 1)
			       "}{" (match-string 2) "}")))
      ;; italic must come after URLs
      (goto-char (point-min))
      (while (re-search-forward "\\/\\(.+?\\)\\/" nil t)
	;; avoid URLs like protocol://foo
	(unless (eq (char-before (match-beginning 0)) ?:)
	  (replace-match (concat "\\\\emph{" (match-string 1) "}"))))
      (goto-char (point-min))
      (while (re-search-forward "//\\(.+?\\)//" nil t)
	;; avoid URLs like protocol://foo
	(unless (eq (char-before (match-beginning 0)) ?:)
	  (replace-match (concat "\\\\emph{" (match-string 1) "}"))))
      (goto-char (point-min))
      (while (re-search-forward "\\(\\s-\\)/\\(.+?\\)/" nil t)
	(replace-match (concat (match-string 1) "\\\\emph{" (match-string 2) "}")))
      (goto-char (point-min))
      ;; hyperref must also come after emph
      (while (re-search-forward "\\[\\[\\(.+?\\)|\\(.+?\\)\\]\\]" nil t)
	(replace-match (format "\\\\hyperref[sec:%s]{%s}"
			       (downcase (match-string 1))
			       (match-string 2))))
      (goto-char (point-min))
      (while (re-search-forward "%" nil t)
	(replace-match "\\\\%"))
      (goto-char (point-min))
      (while (re-search-forward "^\\# " nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0)
			    (progn
			      (re-search-forward "\n\\s-*\n[^#]\\|\\'")
			      (match-beginning 0)))
	  (goto-char (point-max))
	  (insert "\n\\end{enumerate}")
	  (goto-char (point-min))
	  (insert "\\begin{enumerate}\n")
	  (while (re-search-forward "^\\# " nil t)
	    (replace-match (concat "  \\\\item ")))))
      ;; tables
      (goto-char (point-min))
      (while (re-search-forward "^|= \\(.*\\) |" nil t)
	(let ((str (match-string 1)))
	  (replace-match "")
	  (save-excursion
	    (save-match-data
	      (let ((list (split-string str " |= " t)))
		(insert "\\begin{table}\n"
			"\\begin{tabular}{c"
			(make-string (1- (length list)) ?l) "}\n"
			(mapconcat 'identity list " & ") " \\\\")))))
	(while (re-search-forward "^|\\(.*\\)" nil t)
	  (let ((str (match-string 1)))
	    (replace-match "")
	    (save-excursion
	      (save-match-data
		(let ((list (split-string str " *| *" t)))
		  (insert (mapconcat 'identity list " & ") " \\\\"))))))
	(end-of-line)
	(newline)
	(insert "\\end{tabular}\n"
		"\\end{table}\n")))))

(defun latex-to-oddmuse (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "^\\\\[a-z]+{" nil t)
	(forward-char -1)
	(latex-forward-sexp 1)
	(when (= (char-after) ?\n)
	  (delete-region (match-beginning 0) (1+ (point)))))
      (goto-char (point-min))
      (while (re-search-forward "\\([^\\\\]\\)%.*\n" nil t)
	(replace-match (match-string 1)))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\([a-z]+\\){\\([^[}]+\\)}{\\([^[}]+\\)}" nil t)
	(cond ((string= (match-string 1) "SI")
	       (replace-match (concat (match-string 2) (match-string 3))))
	      ((or (string= (match-string 1) "href")
		   (string= (match-string 1) "hiref"))
	       (replace-match (concat "[" (match-string 2) " " (match-string 3) "]") t t))
	      (t (goto-char (match-end 1)))))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\([a-z]+\\)\\[\\([^[}]+\\)\\]{\\([^[}]+\\)}" nil t)
	(cond ((string= (match-string 1) "hyperref")
	       (let ((orig (match-string 3))
		     (s (capitalize (match-string 3))))
		 (save-match-data
		   (cond ((string-match "^giant \\(.*?\\)s?$" s)
			  (setq s (format "%s, Giant" (match-string 1 s))))
			 ((string-match "\\(.*?\\)s?$" s)
			  (setq s (format "%s" (match-string 1 s))))))
		 (if (string= s orig)
		     (replace-match (format "[[%s]]" s))
		   (replace-match (format "[[%s|%s]]" s orig)))))))
      (goto-char (point-min))
      (while (re-search-forward "\\\\marginnote" nil t)
	(replace-match " ")
	(save-excursion
	  (latex-forward-sexp 1)
	  (delete-char -1))
	(delete-char 1))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\([a-z]+\\){\\([^[}]+\\)}" nil t)
	(cond ((string= (match-string 1) "section")
	       (replace-match (concat "== " (match-string 2))))
	      ((string= (match-string 1) "key")
	       (replace-match (concat "*" (match-string 2) "*")))
	      ((string= (match-string 1) "textbf")
	       (replace-match (concat "*" (match-string 2) "*")))
	      ((string= (match-string 1) "emph")
	       (replace-match (concat "/" (match-string 2) "/")))
	      ((member (match-string 1) '("label" "index" "forests" "begin" "end"
					  "includegraphics" "averagevalue"))
	       (replace-match ""))))
      (goto-char (point-min))
      (while (search-forward "\\%" nil t)
	(replace-match "%"))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\(st\\|nd\\|rd\\|th\\)\\b" nil t)
	(replace-match (match-string 1)))
      (goto-char (point-min))
      (while (search-forward "~" nil t)
	(replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "``\\([^']*\\)''" nil t)
	(replace-match (format "\"%s\"" (match-string 1)))))))
  
