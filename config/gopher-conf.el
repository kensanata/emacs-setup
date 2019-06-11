(autoload 'gopher "gopher" nil t)
(autoload 'gopher-goto-url "gopher" nil t)

(add-hook 'gopher-edit-mode-hook
	  'asc:gopher-edit-setup)

(defun asc:gopher-edit-setup ()
  (insert "```\n"
	  "username: Alex\n"
	  "summary: \n"
	  "answer: hello\n"
	  "minor: 1\n"
	  "```\n"
	  "\n"
	  "Tags: [[tag:todo]]"))

(defun asc:sdf-phlogs ()
  "Visit http://phlogosphere.org/ using gopher."
  (interactive)
  (gopher-goto-url "phlogosphere.org"))

(add-hook 'gopher-mode-hook
	  'asc:gopher-setup)

(defun asc:gopher-setup ()
  (visual-line-mode 1))

;; (defun asc:gopher-setup ()
;;   (setq bookmark-make-record-function 'gopher-make-bookmark-record))

;; (defun gopher-make-bookmark-record ()
;;   "Add the current location as a bookmark.
;; This really only works for directories."
;;   (let* ((bookmark-name (gopher-format-address gopher-current-address)))
;;     `(,bookmark-name
;;       ,@(bookmark-make-record-default 'no-file)
;;       (gopher-address . ,gopher-current-address)
;;       (gopher-content-type
;;        . ,(if gopher-current-data 'directory-listing 'plain-text))
;;       (handler . gopher-bookmark-jump))))

;; (defvar gopher-bookmark nil
;;   "Bookmark to jump to when finishing the buffer")

;; (defadvice gopher-finish-buffer (after gopher-goto-bookmark activate)
;;   "When finishing the buffer, jump to `gopher-bookmark'."
;;   (when gopher-bookmark
;;     (bookmark-default-handler
;;      ;; based on Info-bookmark-jump
;;      `("" (buffer . ,(current-buffer)) . ,gopher-bookmark))))

;; (defun gopher-bookmark-jump (bmk)
;;   "Jump to the bookmark."
;;   (let ((address (bookmark-prop-get bmk 'gopher-address))
;; 	(content-type (bookmark-prop-get bmk 'gopher-content-type)))
;;     (gopher-goto-url (nth 0 address)
;; 		     (nth 1 address)
;; 		     (nth 2 address)
;;                      content-type)
;;     (pop-to-buffer gopher-buffer-name)
;;     (setq gopher-bookmark bmk)))
