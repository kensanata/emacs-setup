(defun swiss ()
  "Switch ispell dictionary to 'swiss'."
  (interactive)
  (ispell-change-dictionary "swiss8")
  (asc:flyspell)
  (typo-mode 1)
  (setq typo-language "German")
  (set-input-method 'german-prefix))

(defalias 'german 'swiss)

(defun deutsch ()
  "Switch ispell dictionary to 'swiss'."
  (interactive)
  (ispell-change-dictionary "german-new8")
  (asc:flyspell)
  (typo-mode 1)
  (setq typo-language "German")
  (set-input-method 'german-prefix))

(defun english ()
  "Switch ispell dictionary to 'british'."
  (interactive)
  (ispell-change-dictionary "british")
  (asc:flyspell)
  (typo-mode 1)
  (setq typo-language "English")
  (set-input-method nil))

(defun asc:flyspell ()
  "Ensure flyspell is running."
  (interactive)
  (unless flyspell-mode
    (flyspell-mode t))
  (flyspell-buffer))

(eval-after-load "ispell"
  '(defun ispell-get-decoded-string (n)
     "Get the decoded string in slot N of the descriptor of the current dict."
     (let* ((slot (or
		   (assoc ispell-current-dictionary ispell-local-dictionary-alist)
		   (assoc ispell-current-dictionary ispell-dictionary-alist)
		   (error "No data for dictionary \"%s\" in `ispell-local-dictionary-alist' or `ispell-dictionary-alist'"
			  ispell-current-dictionary)))
            (str (nth n slot)))
       (when (stringp str)
	 ;; no decoding!
         str))))

;; Fix encoding? Check with words such as "Tätigkeitsliste"
;; ispell-dictionary-alist has latin-1 encoded umlauts!
;; (assoc "swiss8" ispell-dictionary-alist)

;; Also, the British dictionary doesn't recognise type mode stuff such
;; as "doesn’t".
;; (assoc "british" ispell-dictionary-alist)

(setq ispell-local-dictionary-alist
      '(("swiss8" "[A-Za-zÄÜÖäüöß]" "[^A-Za-zÄÜÖäüöß]"
	 "[']" t ("-C" "-d" "de_CH") nil utf-8)
	("british" "[[:alpha:]]" "[^[:alpha:]]"
	 "['’]" t ("-d" "en_GB") nil utf-8)))
