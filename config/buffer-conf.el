;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Possible elements: mark modified read-only locked name size mode
;; process filename
(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 40 40 :left :elide)
              " " filename-and-process)
        (mark " " (name 16 -1)
              " " filename)))

;; some buffers should be full-size
(setq special-display-buffer-names
      '("*compilation*" "*Help*" "*grep*" "*occur*" "*info")
      special-display-function 'display-buffer-same-window)

(defvar asc:file-names-seen nil
  "List of file names seen.
A good candidate to add to `desktop-globals-to-save'.")

(defun asc:file-names-seen-update ()
  "Add the current file name to `asc:file-names-seen'."
  (let ((name (or buffer-file-name dired-directory)))
    (when name
      (setq asc:file-names-seen
            (cons name (delete name asc:file-names-seen))))))

(add-hook 'find-file-hook 'asc:file-names-seen-update)
(add-hook 'dired-after-readin-hook 'asc:file-names-seen-update)

(defvar switch-buffer-or-find-file-history nil
  "History of file names or buffers picked.")

;; default history-length is 100 which is not enough
(put 'switch-buffer-or-find-file-history 'history-length 1000)

(defun switch-buffer-or-find-file ()
  "Switch buffer or find file."
  (interactive)
  (let ((buffer-names (mapcar 'buffer-name (buffer-list)))
        (file-names nil)
        (file-names-alist nil)
        (default nil)n
        (this-buffer-name (buffer-name (current-buffer))))
    ;; build filename list (reversed!)
    (dolist (full-name asc:file-names-seen)
      (let* ((name (if (file-directory-p full-name)
                       ;; parent directory with a slash…
                       (file-name-as-directory
                        (file-name-nondirectory
                         (directory-file-name full-name)))
                     ;; filename
                     (file-name-nondirectory full-name)))
             (full-names (assoc name file-names-alist)))
        (setq file-names (cons name file-names))
        (if full-names
            (setcdr full-names (cons full-name (cdr full-names)))
          (setq file-names-alist (cons (cons name (list full-name))
                                       file-names-alist)))))
    ;; filter beginning of the buffer name list and figure out the default
    (while (and (not default) buffer-names)
      (let ((name (car buffer-names)))
        (if (or (string= (substring name 0 1) " ")
                (eq this-buffer-name name))
            (setq buffer-names (cdr buffer-names))
          (setq default name))))
    (setq candidates (completion-table-merge buffer-names file-names))
    (let* ((name (completing-read
                  (format "Switch to (%s): " default)
                  candidates
                  (lambda (s)
                    (not (or (string= s "")
                             (string= (substring s 0 1) " ")
                             (eq this-buffer-name s))))
                  nil nil
                  'switch-buffer-or-find-file-history
                  default))
           (buf (get-buffer name))
           (files (cdr (assoc name file-names-alist))))
      (cond (buf (switch-to-buffer buf))
            ((= 1 (length files))
             (find-file (car files)))
            (t (find-file
                (completing-read "Which one of these files: " files)))))))

(global-set-key (kbd "C-x b") 'switch-buffer-or-find-file)
