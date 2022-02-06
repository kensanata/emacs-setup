(setq calendar-week-start-day 1
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
			       "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
				 "Juni" "Juli" "August" "September"
				 "Oktober" "November" "Dezember"])

(setq display-time-world-list
      '(("America/New_York" "New York")
	("Europe/Lisbon" "Lissabon")
	("Europe/Zurich" "Zürich")))

(defun itime-correct ()
  "When point is on or after a local time, translate it into Internet Time."
  (interactive)
  (skip-chars-backward "0-9: \t")
  (skip-chars-forward " \t")
  (when (looking-at "\\([0-9]+\\):\\([0-9]+\\):?\\([0-9]+\\)?")
    (let ((hours (match-string 1))
	  (minutes (match-string 2))
	  (seconds (match-string 3)))
      (when (not seconds)
	(setq seconds "0"))
      (replace-match (itime-string hours minutes seconds)))))

(defun itime-string (hour minute second &optional ignore)
  "Return internet time as string.
    HOUR MINUTE and SECOND are strings as provided within
    `display-time-string-forms' and are local time."
  (let* ((seconds (+ (* 3600 (string-to-number hour))
		     3600
		     (- (car (current-time-zone)))
		     (* 60 (string-to-number minute))
		     (string-to-number second)))
	 (beats (mod (floor seconds 86.4) 1000)))
    (format "@%03d" beats)))

(setq display-time-string-forms
     '(" " 24-hours ":" minutes " "
       (itime-string 24-hours minutes seconds t)
       (if mail
	   " Mail"
	 ""))
     display-time-interval 5)

(display-time-mode 1)

(defun asc:display-time-world (string)
  "Show world time for STRING.
STRING is a time that can be parsed by `parse-time-string'.
The output depends on `display-time-world-list'."
  (interactive "sTime: ")
  (when (string-match-p "^[0-9]+:[0-9][0-9]$" string)
    (setq string (concat (format-time-string "%F " (current-time)) string)))
  (let* ((now (parse-iso8601-time-string string))
	 (max-width 0)
	 result fmt)
    (dolist (zone display-time-world-list)
      (let* ((label (cadr zone))
	     (width (string-width label)))
	(push (cons label
		    (format-time-string display-time-world-time-format
					now (car zone)))
	      result)
	(when (> width max-width)
	  (setq max-width width))))
    (setq fmt (concat "%-" (int-to-string max-width) "s %s"))
    (message (mapconcat
	      (lambda (timedata)
		(format fmt (car timedata) (cdr timedata)))
	      (nreverse result)
	      "\n"))))

