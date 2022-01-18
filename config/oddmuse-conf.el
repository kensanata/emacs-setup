;;; Oddmuse

(add-to-list 'load-path "~/src/oddmuse-curl")

(when (require 'shr nil t)
  (require 'oddmuse-curl nil t))

(eval-after-load 'goto-address-url-regexp
  '(setq goto-address-url-regexp
	 (replace-regexp-in-string "gopher" "gopher\\\\|gemini" goto-address-url-regexp)))

(setq oddmuse-basic-markup
      '(("\\[\\[.*?\\]\\]"
	 0 '(face link
		  help-echo "Basic free link"))
	("\\[\\<\\(gophers?\\|https?\\)[-a-zA-Z0-9/@=+$_~*.,;:?!'\"()&#%]+ [^]\n]*\\]"
	 0 '(face link
		  help-echo "Basic external free link with text"))
	("\\<\\(gophers?\\|https?\\)[-a-zA-Z0-9/@=+$_~*.,;:?!'\"()&#%]+[-a-zA-Z0-9/@=+$_~*]"
	 0 '(face link
		  help-echo "Basic external free link"))
	("\\[[[:upper:]]\\S-*:\\S-+ [^]\n]*\\]"
	 0 '(face link
		  help-echo "Basic external interlink with text"))
	("[[:upper:]]\\S-*:\\S-+"
	 0 '(face link
		  help-echo "Basic external interlink"))
	("^\\([*] \\)"
	 0 '(face font-lock-constant-face
		  help-echo "Basic bullet list"))))

(setq oddmuse-username "AlexSchroeder"
      oddmuse-markup '(oddmuse-basic-markup oddmuse-markdown-markup))
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
	("Drei Wälder" "https://campaignwiki.org/wiki/Drei_W%c3%a4lder"
	 utf-8 "frodo" "Alex")
	("Rasiermesserküste" "https://campaignwiki.org/wiki/Rasiermesserk%c3%bcste"
	 utf-8 "frodo" "Alex")
	("Halberds and Helmets" "https://campaignwiki.org/wiki/Halberds_and_Helmets"
	 utf-8 "frodo" "Alex")
	("Links to Wisdom" "https://campaignwiki.org/wiki/LinksToWisdom"
	 utf-8 "frodo" "Alex")
	("Community Wiki" "https://communitywiki.org/wiki"
	 utf-8 "IS9oBk9t" nil)
	("Oddmuse Wiki" "https://oddmuse.org/wiki"
	 utf-8 "ham" nil)
	("Alex" "https://alexschroeder.ch/wiki"
	 utf-8 "question" "Alex Schroeder")
	("Software" "https://alexschroeder.ch/software"
	 utf-8 "question" "Alex Schroeder")
	("Food" "https://communitywiki.org/food"
	 utf-8 "question" "Alex Schroeder")
	("Kaylash" "https://campaignwiki.org/wiki/Kaylash"
	 utf-8 "frodo" "Alex")
	("Tau Subsector" "https://campaignwiki.org/wiki/Tau_Subsector"
	 utf-8 "frodo" "Alex")
	("Octagon" "https://campaignwiki.org/wiki/Octagon"
	 utf-8 "frodo" "Alex")
	("Dschungel von Chult" "https://campaignwiki.org/wiki/DschungelvonChult"
	 utf-8 "frodo" "Alex")
	("Die Zeit der Waldbrände" "https://campaignwiki.org/wiki/Die_Zeit_der_Waldbr%c3%a4nde"
	 utf-8 "frodo" "Alex"))
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
	((member oddmuse-wiki '("Rasiermesserküste" "Wilderlande" "MondscheinSaga"))
	 (turn-on-auto-fill)
	 ;; see spell-conf.el
	 (german))))

(defun oddmuse-comment ()
  "Switch between article and talk pages."
  (interactive)
  (if (string-match "^Comments_on_\\(.*\\)" oddmuse-page-name)
      (oddmuse-follow oddmuse-wiki (match-string 1 oddmuse-page-name))
    (oddmuse-follow oddmuse-wiki (concat "Comments_on_" oddmuse-page-name))))

(add-hook 'oddmuse-mode-hook
	  (lambda () (local-set-key (kbd "C-c c") 'oddmuse-comment)))

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
      (when (re-search-forward "Tags:\\( \\[\\[tag:.*?\\]\\]\\)+\n?" nil t)
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

(setq oddmuse-tags
      '("RPG" "Old School" "RSP" "Maps" "Pictures" "Philosophy"
	"Software" "Copyright" "Social Media" "Gadgets" "Administration"
	"PureOS" "Debian" "Monsters"
	"Programming" "Mastodon" "Trunk" "Halberts and Helmets"
	"Books" "Movies" "Music" "Life" "Podcast" "Gridmapper"
	"Web" "Oddmuse" "Wikis" "Blogs" "Text Mapper" "Hex Describe"
	"Switzerland" "USA" "Germany" "Google Plus" "Diaspora"
	"Keep It Short" "Hellebarden und Helme" "Pendragon"
	"Halberds and Helmets Podcast" "Indie" "Perl"))

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

(defun asc:markdown-mode-basics ()
  "Stealing all the setup from `markdown-mode'."
  (require 'markdown-mode)
  ;; Natural Markdown tab width
  (setq tab-width 4)
  ;; Comments
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  (setq-local comment-start-skip "<!--[ \t]*")
  (setq-local comment-column 0)
  (setq-local comment-auto-fill-only-comments nil)
  (setq-local comment-use-syntax t)
  ;; Syntax
  (add-hook 'syntax-propertize-extend-region-functions
            #'markdown-syntax-propertize-extend-region)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'markdown-font-lock-extend-region-function t t)
  (setq-local syntax-propertize-function #'markdown-syntax-propertize)
  (syntax-propertize (point-max)) ;; Propertize before hooks run, etc.
  ;; Font lock.
  (setq font-lock-defaults
        '(markdown-mode-font-lock-keywords
          nil nil nil nil
          (font-lock-multiline . t)
          (font-lock-syntactic-face-function . markdown-syntactic-face)
          (font-lock-extra-managed-props
           . (composition display invisible rear-nonsticky
                          keymap help-echo mouse-face))))
  (if markdown-hide-markup
      (add-to-invisibility-spec 'markdown-markup)
    (remove-from-invisibility-spec 'markdown-markup))
  ;; Wiki links
  (markdown-setup-wiki-link-hooks)
  ;; Math mode
  (when markdown-enable-math (markdown-toggle-math t))
  ;; Add a buffer-local hook to reload after file-local variables are read
  (add-hook 'hack-local-variables-hook #'markdown-handle-local-variables nil t)
  ;; For imenu support
  (setq imenu-create-index-function
        (if markdown-nested-imenu-heading-index
            #'markdown-imenu-create-nested-index
          #'markdown-imenu-create-flat-index))
  ;; For menu support in XEmacs
  (easy-menu-add markdown-mode-menu markdown-mode-map)
  ;; Defun movement
  (setq-local beginning-of-defun-function #'markdown-beginning-of-defun)
  (setq-local end-of-defun-function #'markdown-end-of-defun)
  ;; Paragraph filling
  (setq-local fill-paragraph-function #'markdown-fill-paragraph)
  (setq-local paragraph-start
              ;; Should match start of lines that start or separate paragraphs
              (mapconcat #'identity
                         '(
                           "\f" ; starts with a literal line-feed
                           "[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           "[ \t]*[*+-][ \t]+" ; unordered list item
                           "[ \t]*\\(?:[0-9]+\\|#\\)\\.[ \t]+" ; ordered list item
                           "[ \t]*\\[\\S-*\\]:[ \t]+" ; link ref def
                           "[ \t]*:[ \t]+" ; definition
                           "^|" ; table or Pandoc line block
                           )
                         "\\|"))
  (setq-local paragraph-separate
              ;; Should match lines that separate paragraphs without being
              ;; part of any paragraph:
              (mapconcat #'identity
                         '("[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           ;; The following is not ideal, but the Fill customization
                           ;; options really only handle paragraph-starting prefixes,
                           ;; not paragraph-ending suffixes:
                           ".*  $" ; line ending in two spaces
                           "^#+"
                           "[ \t]*\\[\\^\\S-*\\]:[ \t]*$") ; just the start of a footnote def
                         "\\|"))
  (setq-local adaptive-fill-first-line-regexp "\\`[ \t]*[A-Z]?>[ \t]*?\\'")
  (setq-local adaptive-fill-regexp "\\s-*")
  (setq-local adaptive-fill-function #'markdown-adaptive-fill-function)
  (setq-local fill-forward-paragraph-function #'markdown-fill-forward-paragraph)
  ;; Outline mode
  (setq-local outline-regexp markdown-regex-header)
  (setq-local outline-level #'markdown-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; ElDoc support
  (if (eval-when-compile (fboundp 'add-function))
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'markdown-eldoc-function)
    (setq-local eldoc-documentation-function #'markdown-eldoc-function))
  ;; Inhibiting line-breaking:
  ;; Separating out each condition into a separate function so that users can
  ;; override if desired (with remove-hook)
  (add-hook 'fill-nobreak-predicate
            #'markdown-line-is-reference-definition-p nil t)
  (add-hook 'fill-nobreak-predicate
            #'markdown-pipe-at-bol-p nil t)

  ;; Indentation
  (setq-local indent-line-function markdown-indent-function)

  ;; Flyspell
  (setq-local flyspell-generic-check-word-predicate
              #'markdown-flyspell-check-word-p)

  ;; Electric quoting
  (add-hook 'electric-quote-inhibit-functions
            #'markdown--inhibit-electric-quote nil :local)

  ;; Backwards compatibility with markdown-css-path
  (when (boundp 'markdown-css-path)
    (warn "markdown-css-path is deprecated, see markdown-css-paths.")
    (add-to-list 'markdown-css-paths markdown-css-path))

  ;; Prepare hooks for XEmacs compatibility
  (when (featurep 'xemacs)
    (make-local-hook 'after-change-functions)
    (make-local-hook 'font-lock-extend-region-functions)
    (make-local-hook 'window-configuration-change-hook))

  ;; Make checkboxes buttons
  (when markdown-make-gfm-checkboxes-buttons
    (markdown-make-gfm-checkboxes-buttons (point-min) (point-max))
    (add-hook 'after-change-functions #'markdown-gfm-checkbox-after-change-function t t)
    (add-hook 'change-major-mode-hook #'markdown-remove-gfm-checkbox-overlays t t))

  ;; edit-indirect
  (add-hook 'edit-indirect-after-commit-functions
            #'markdown--edit-indirect-after-commit-function
            nil 'local)

  ;; Marginalized headings
  (when markdown-marginalize-headers
    (add-hook 'window-configuration-change-hook
              #'markdown-marginalize-update-current nil t)))

(defvar software-projects nil
  "The known software projects.")

(defun software-get-projects (&optional no-cache)
  "Get the list projects from the main page of the Software wiki
or from the cache. Use a prefix argument to reset the cache."
  (interactive "P")
  (or software-projects
      (setq software-projects
	    (let* ((url "https://alexschroeder.ch/software/raw/Software")
		   (buf (url-retrieve-synchronously url))
		   (regexp "\\[\\[\\(.*?\\)\\]\\]")
		   projects)
	      (with-current-buffer buf
		(while (re-search-forward regexp nil t)
		  (setq projects (cons (match-string 1) projects))))
	      projects))))

(defun software-issue (project pagename)
  "Create a new issue on the software issue."
  (interactive (list (completing-read "Project: " (software-get-projects))
		     (read-string "Title: ")))
  ;; a lot of code from `oddmuse-edit'
  (let ((wiki "Software"))
    (make-directory (concat oddmuse-directory "/" wiki) t)
    (let ((name (concat wiki ":" pagename)))
      (if (and (get-buffer name)
               (not current-prefix-arg))
          (pop-to-buffer (get-buffer name))
	(set-buffer (get-buffer-create name))
	;; do not insert page content from the wiki
	(save-excursion
	 (insert "Describe issue...\n\n[[tag:Issue]] [[tag:" project "]] [[tag:Open]]\n"))
	(setq buffer-file-name (concat oddmuse-directory "/" wiki "/" pagename))
	(vc-mode-line buffer-file-name 'oddmuse)
	(pop-to-buffer (current-buffer))
	(when (file-exists-p buffer-file-name)
	  (warn "Page loaded from the wiki but a local file also exists"))
	;; produces a message about saving the buffer
	(basic-save-buffer)
	;; produces a message about the wiki revision
	(oddmuse-revision-put wiki pagename (oddmuse-get-latest-revision wiki pagename))
	(oddmuse-mode)))))
