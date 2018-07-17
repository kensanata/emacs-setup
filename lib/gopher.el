;;; gopher.el --- easily access and navigate Gopher servers

;; Copyright (C) 2011 Matthew Snyder

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Matthew Snyder <matthew.c.snyder@gmail.com>
;;         and the gopher.el authors (see AUTHORS.org)
;; URL: http://github.com/ardekantur/gopher.el
;; Version: 0.0.2

;; This file is not part of GNU Emacs.

;;; Commentary:

;; gopher.el allows you to navigate Gopher servers.

;; "M-x gopher" prompts you for an address. <TAB> and <M-TAB> navigate
;; between links on a directory listing, while <[> and <]> navigate
;; between text documents. <RET> opens the link at the cursor's
;; position. You can navigate up through the directory tree with <u>.
;;
;; There is primitive history support. <B> navigates backwards
;; and <F> forwards through the history.

(require 'cl)
(require 'shr)

(defconst gopher-available-content-types
  '(("0" . plain-text)
    ("1" . directory-listing)
    ("i" . informational-message)
    ("g" . gif)
    ("h" . html)
    ("I" . generic-image)
    ("7" . search-query)
    ("w" . write)))

(defconst gopher-extra-network-arguments
  '((gif . (:coding binary))
    (generic-image . (:coding binary))))

(defconst gopher-faces
  '((directory-listing . font-lock-builtin-face)
    (informational-message . font-lock-comment-face)
    (gif . font-lock-variable-name-face)
    (generic-image . font-lock-string-face)
    (html . font-lock-type-name-face)
    (write . font-lock-warning-face)))

(defvar gopher-buffer-name "*gopher*")

(defgroup gopher nil
  "Gopher server navigation"
  :group 'hypermedia)

(defvar gopher-history-ring nil
  "List of URLs visited in gopher.")

(defvar gopher-history-ring-pointer nil
  "The tail of the gopher history ring, whose car is the last page visited.")

(defcustom gopher-history-ring-max 60
  "Maximum length of gopher history ring before oldest elements are thrown away."
  :type 'integer
  :group 'gopher)

(defun gopher-get-matching (function content-type)
  (let ((name (intern (concat "gopher-" function "-" (symbol-name content-type)))))
    (if (fboundp name)
        name
      (intern (concat "gopher-" function)))))

(defun gopher-refresh-current-address ()
  (interactive)
  (gopher-goto-url (nth 0 gopher-current-address)
                   (nth 1 gopher-current-address)
                   (nth 2 gopher-current-address)
                   nil nil t))

(defun gopher-get-content-type (line-data)
  (let ((content-type (assoc (getf line-data :item-type) gopher-available-content-types)))
    (if content-type
        (cdr content-type)
      nil)))

(defun gopher-get-face (content-type)
  (let ((face (assoc content-type gopher-faces)))
    (if face
        (cdr face)
      nil)))

(defun gopher-get-extra-network-args (content-type)
  (let ((args (assoc content-type gopher-extra-network-arguments)))
    (if args
        (cdr args)
      nil)))

(defun gopher (address)
  (interactive "MGopher URL: ")
  (let* ((split-address (split-string (replace-regexp-in-string
				       "^gopher:\/\/" "" address) "/"))
         (split-url (split-string (car split-address) ":"))
	 (hostname (car split-url))
	 (port (nth 1 split-url))
	 (content-type (cdr (assoc (nth 1 split-address)
				   gopher-available-content-types)))
         (selector (when (> (length split-address) 1)
		     (mapconcat 'identity
				(cons "" (nthcdr 2 split-address)) "/"))))
    (gopher-goto-url hostname port selector content-type)))

(defun gopher-goto-url (&optional hostname port selector content-type
                                  search-argument no-history)
  (interactive)
  (with-current-buffer (get-buffer-create gopher-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer))
  (if (not content-type)
      (setq content-type 'directory-listing))
  (if (not port)
      (setq port "70"))
  (unless no-history (gopher-history-new hostname port selector content-type))
  (gopher-prepare-buffer hostname port selector)
  (setq gopher-network-args (append (list
                                     :name "gopher"
                                     :buffer gopher-buffer-name
                                     :host hostname
                                     :service (string-to-number port)
                                     :filter (gopher-get-matching "filter" content-type)
                                     :sentinel (gopher-get-matching "sentinel" content-type))
                                    (gopher-get-extra-network-args content-type)))
  (setq gopher-current-network-process (apply 'make-network-process gopher-network-args)
        gopher-line-fragment nil)
  (process-send-string gopher-current-network-process (gopher-prepare-request selector search-argument)))

(defun gopher-prepare-request (selector search-argument)
  (cond
   ((and selector search-argument) (format "%s\t%s\r\n" selector search-argument))
   (selector (format "%s\r\n" selector))
   (t "\r\n")))

(defun gopher-prepare-buffer (hostname port selector)
  (set-window-buffer (selected-window) gopher-buffer-name)
  (with-current-buffer gopher-buffer-name
    (gopher-mode)
    (setq gopher-current-address (list hostname port selector)
          gopher-current-data nil
          line-spacing 3)
    (insert "\n\n")))

(defun gopher-format-address (address)
  (let ((hostname (nth 0 address))
        (port (nth 1 address))
        (selector (nth 2 address)))
    (cond
     ((and selector
           (not (zerop (length selector)))
           (string= "/" (substring selector 0 1)))
      (format "%s:%s%s" hostname port selector))
     (selector
      (format "%s:%s/%s" hostname port selector))
     (t
      (format "%s:%s" hostname port)))))

(defun gopher-process-line (line)
  (let* ((lineparts (split-string line "\t"))
         (item-type (substring (nth 0 lineparts) 0 1))
         (display-string (substring (nth 0 lineparts) 1))
         (selector (nth 1 lineparts))
         (hostname (nth 2 lineparts))
         (port (nth 3 lineparts)))
    (list :item-type item-type
          :display-string display-string
          :selector selector
          :hostname hostname
          :port port)))

(defun gopher-filter (proc string)
  (with-current-buffer gopher-buffer-name
    (insert string)))

(defun gopher-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun gopher-filter-gif (proc string)
  (with-current-buffer gopher-buffer-name
    (setq gopher-current-data (concat gopher-current-data string))))

(defun gopher-filter-generic-image (proc string)
  (with-current-buffer gopher-buffer-name
    (setq gopher-current-data (concat gopher-current-data string))))

(defun gopher-filter-directory-listing (proc string)
  (with-current-buffer gopher-buffer-name
    (setq gopher-current-data (concat gopher-current-data string))))

(defun gopher-display-line (line)
  (if (or
       (zerop (length line))
       (string-match "^\.$" line))
      ""
    (let* ((line-data (gopher-process-line line))
           (indent (apply 'propertize "     " line-data)))
      (concat indent (gopher-format-line line-data) "\n"))))

(defun gopher-format-line (line-data)
  (let ((content-type (gopher-get-content-type line-data)))
    (if (and content-type (gopher-get-face content-type))
        (propertize (getf line-data :display-string)
                    'face (gopher-get-face content-type))
      (getf line-data :display-string))))

(defun gopher-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (gopher-finish-buffer))))

(defun gopher-sentinel-directory-listing (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (let* ((lines (split-string gopher-current-data "\r?\n")))
        (mapc (lambda (line) (insert (gopher-display-line line))) lines))
      (gopher-finish-buffer))))

(defalias 'gopher-sentinel-search-query 'gopher-sentinel-directory-listing)
(defalias 'gopher-filter-search-query 'gopher-filter-directory-listing)

(defun gopher-sentinel-plain-text (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (gopher-finish-buffer))))

(defun gopher-sentinel-html (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (shr-render-region (point-min) (point-max) (current-buffer))
      (gopher-finish-buffer))))

(defun gopher-sentinel-gif (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (insert "     ")
      (insert-image (create-image gopher-current-data 'gif 'data))
      (gopher-finish-buffer))))

(defun gopher-sentinel-generic-image (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (let ((image-type (image-type-from-data gopher-current-data)))
        (if image-type
            (progn
              (insert "     ")
              (insert-image (create-image
                             gopher-current-data image-type 'data))
              (gopher-finish-buffer))
          (error "Could not determine image type for %s"
		 (gopher-format-address gopher-current-address)))))))

(defun gopher-finish-buffer ()
  (setq buffer-read-only t)
  (goto-char (point-min))
  (gopher-remove-dos-eol)
  (message "Loaded %s."
	   (gopher-format-address gopher-current-address)))

(defun gopher-goto-url-at-point (&optional arg)
  (interactive)
  (let ((url (url-get-url-at-point)))
    (if url
	(gopher url)
      (move-beginning-of-line nil)
      (let* ((properties (text-properties-at (point)))
	     (content-type (gopher-get-content-type properties)))
	(cond ((eq content-type 'search-query)
	       (call-interactively 'gopher-goto-search))
	      ((eq content-type 'write)
	       (gopher-goto-write (getf properties :hostname)
				  (getf properties :port)
				  (getf properties :selector)))
	      (content-type
	       (gopher-goto-url (getf properties :hostname)
				(getf properties :port)
				(getf properties :selector)
				content-type))
	      (t
	       (error "Nothing to follow, here.")))))))

(defun gopher-goto-parent (&optional arg)
  (interactive)
  (let* ((address gopher-current-address)
         (hostname (nth 0 address))
         (port (nth 1 address))
         (selector (nth 2 address)))
  (gopher-goto-url hostname port (gopher-selector-parent selector))))

(defun gopher-goto-search (search-argument)
  (interactive "MSearch argument: ")
  (let* ((properties (text-properties-at (point)))
         (content-type (gopher-get-content-type properties)))
    (gopher-goto-url (getf properties :hostname)
		     (getf properties :port)
                     (getf properties :selector)
                     content-type search-argument)))

(defun gopher-goto-search (search-argument)
  (interactive "MSearch argument: ")
  (let* ((properties (text-properties-at (point)))
         (content-type (gopher-get-content-type properties)))
    (gopher-goto-url (getf properties :hostname)
		     (getf properties :port)
                     (getf properties :selector)
                     content-type search-argument)))

(defun gopher-goto-write (hostname port selector)
  (set-window-buffer (selected-window) (get-buffer-create gopher-buffer-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  (with-current-buffer gopher-buffer-name
    (gopher-edit-mode)
    (setq gopher-current-address (list hostname port selector))))

(defun gopher-write ()
  (interactive)
  (let* ((address gopher-current-address)
         (hostname (nth 0 address))
         (port (nth 1 address))
         (selector (concat (nth 2 address) "\r\n"
			   (buffer-string))))
    (gopher-goto-url hostname port selector nil nil t)))

(define-derived-mode gopher-edit-mode text-mode "Gopher Edit"
  (set (make-local-variable 'gopher-current-address) nil)
  (local-set-key (kbd "C-c C-c") 'gopher-write))

(define-derived-mode gopher-mode fundamental-mode "Gopher"
  (set (make-local-variable 'gopher-current-data) nil)
  (set (make-local-variable 'gopher-current-address) nil))

(defvar gopher-current-data nil)
(defvar gopher-current-address nil)

(defalias 'gopher-next-line 'next-line)
(defalias 'gopher-previous-line 'previous-line)

(defun gopher-pop-last (list)
  (remove-if (lambda (x) t) list :count 1 :from-end t))

(defun gopher-selector-parent (selector)
  (mapconcat 'identity (gopher-pop-last (split-string selector "/")) "/"))

(defun w3m-open-this-url-in-gopher ()
  "Open this URL in Gopher."
  (interactive)
  (gopher (w3m-anchor)))

(defmacro gopher-navigate (direction content-type)
  `(defun ,(intern (concat "gopher-" (symbol-name direction) "-" (symbol-name content-type))) ()
     (interactive)
     (,direction)
     (move-beginning-of-line nil)
     (while (not (eq ',content-type (gopher-get-content-type (text-properties-at (point)))))
       (,direction))))

(defun gopher-history-current-item (n &optional do-not-move)
  "Rotate the gopher history by N places, and then return that item.
If N is zero, does nothing.

If optional argument DO-NOT-MOVE is non-nil, don't actually
move the remembered point in history, just navigate to that
location."
  (or gopher-history-ring (error "History list is empy."))
  (let ((Nth-history-element
         (nthcdr (mod (- n (length gopher-history-ring-pointer))
                      (length gopher-history-ring))
                 gopher-history-ring)))
    (unless do-not-move
      (setq gopher-history-ring-pointer Nth-history-element))
    (car Nth-history-element)))

(defun gopher-history-new (hostname port selector content-type &optional replace)
  "Make (cons HOSTNAME PORT SELECTOR) the latest item in gopher's history.
Set `gopher-history-ring-pointer' to point to it. Optional third
argument REPLACE non-nil means that this item will replace the
front of the history ring, rather than being added to the list."
  (let ((address (car gopher-history-ring)))
    (when (and (equal hostname (nth 0 address))
	       (equal port (nth 1 address))
	       (equal selector (nth 2 address))
	       (equal content-type (nth 3 content-type)))
      (setq replace t)))
  (let ((entry (list hostname port selector content-type)))
    (if (and replace gopher-history-ring)
        (setcar gopher-history-ring entry)
      (push entry gopher-history-ring)
      (if (> (length gopher-history-ring)
             gopher-history-ring-max)
          (setcdr (nthcdr (1- gopher-history-ring-max)
                          gopher-history-ring) nil)))
    (setq gopher-history-ring-pointer gopher-history-ring)))

(defun gopher-history (&optional step)
  "Walk back through gopher's history.

With optional argument STEP, an integer, go that many steps.
If STEP is negative, move forward through the history."
  (interactive "p")
  (unless step (setq step 1))
  (let ((address (gopher-history-current-item step)))
    (gopher-goto-url (nth 0 address)
		     (nth 1 address)
		     (nth 2 address)
		     (nth 3 address)
                     nil t)))

(defalias 'gopher-history-backwards 'gopher-history)

(defun gopher-history-forward (&optional step)
  "Walk forward through gopher's history.

With optional argument STEP, an integer, go that many steps.
If STEP is negative, move backward through the history"
  (interactive "p")
  (if step (setq step (* -1 step))
    (setq step -1))
  (gopher-history step))

(defun gopher-define-keymaps ()
  (setq gopher-mode-map (make-sparse-keymap))
  (define-key gopher-mode-map "\r" 'gopher-goto-url-at-point)
  (define-key gopher-mode-map "n" 'gopher-next-line)
  (define-key gopher-mode-map "p" 'gopher-previous-line)
  (define-key gopher-mode-map "g" 'gopher)
  (define-key gopher-mode-map "\t" (gopher-navigate next-line directory-listing))
  (define-key gopher-mode-map "\M-\t" (gopher-navigate previous-line directory-listing))
  (define-key gopher-mode-map "]" (gopher-navigate next-line plain-text))
  (define-key gopher-mode-map "[" (gopher-navigate previous-line plain-text))
  (define-key gopher-mode-map "u" 'gopher-goto-parent)
  (define-key gopher-mode-map "r" 'gopher-refresh-current-address)
  (define-key gopher-mode-map "B" 'gopher-history-backwards)
  (define-key gopher-mode-map "F" 'gopher-history-forward)
  (define-key gopher-mode-map "q" 'quit-window))

(gopher-define-keymaps)

(defun gopher-kill-address-at-point ()
  (interactive)
  (move-beginning-of-line nil)
  (let* ((properties (text-properties-at (point)))
         (string (mapconcat 'identity (list
                                       (getf properties :hostname)
				       (getf properties :port)
                                       (getf properties :selector)) "/")))
    (kill-new string)
    (message string)))

(provide 'gopher)

;;; gopher.el ends here
