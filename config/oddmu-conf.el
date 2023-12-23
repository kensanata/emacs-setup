(defvar oddmu-pages (make-hash-table :test 'equal)
  "A hashmap of names and titles.")

(defun oddmu-load-pages ()
  "Add the output of 'oddmu list' to `oddmu-pages'."
  (interactive)
  (with-temp-buffer
    (message "Indexing...")
    (call-process "~/bin/oddmu" nil (current-buffer) nil "list")
    (goto-char (point-min))
    (message "Parsing...")
    (let ((done nil)
          (n 0))
      (while (not done)
        (let ((items (split-string
                      (buffer-substring
                       (line-beginning-position)
                       (line-end-position))
                      "\t")))
          (if (= 1 (forward-line))
              (setq done t)
            (setq n (1+ n))
            (when (= 0 (mod n 100))
              (message "Parsing...%d" n))
            (puthash (nth 1 items) (nth 0 items) oddmu-pages)))))
    (message "Parsing...done")))

(defun oddmu-insert-link ()
  "Insert a link to an Oddmu page."
  (interactive)
  (when (= (hash-table-count oddmu-pages) 0)
    (error "Run oddmu-load-pages in the data directory"))
  (let ((name (completing-read "Link: " oddmu-pages)))
    (insert (format "[%s](%s)" name (gethash name oddmu-pages)))))

