;;; rpg-shell.el --- an interactive environment for RPGs

;; Copyright (C) 2020  Alex Schroeder <alex@gnu.org>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

(require 'comint)

;;; User variables

(defgroup rpg-shell nil
  "Interaction mode for RPG expressions."
  :group 'games)

(defcustom rpg-prompt "RPG> "
  "Prompt used in the RPG Shell.
Setting this variable does not affect existing RPG runs.

Interrupting the RPG process with \\<rpg-map>\\[comint-interrupt-subjob],
and then restarting it using \\[rpg], makes the then current
default value affect _new_ prompts.  Unless the new prompt
differs only in text properties from the old one, RPG will no
longer recognize the old prompts.  However, executing \\[rpg]
does not update the prompt of an *rpg* buffer with a running process.
For RPG buffers that are not called `*rpg*', you can execute
\\[rpg-shell-mode] in that RPG buffer to update the value,
for new prompts.  This works even if the buffer has a running process."
  :type 'string
  :group 'rpg-shell)

(defvar rpg-prompt-internal "ELISP> "
  "Stored value of `rpg-prompt' in the current RPG buffer.
This is an internal variable used by RPG.  Its purpose is to
prevent a running RPG process from being messed up when the user
customizes `rpg-prompt'.")

(defvar rpg-font-lock-keywords
  '(("\\([0-9]+\\)\\(d\\)\\([0-9]+\\)\\(?:\\(+\\)\\([0-9]+\\)\\)?"
     (1 font-lock-variable-face)
     (2 font-lock-constant-face)
     (3 font-lock-variable-face)
     (4 font-lock-constant-face)
     (5 font-lock-variable-face)))
  "Additional expressions to highlight in RPG buffers.")

(defvar rpg-header
  "*** Welcome to the RPG shell ***\n"
  "Message to display when RPG shell is started.")

(defvar rpg-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'rpg-return)
    (define-key map "\C-j" 'rpg-send-input)
    map)
  "Keymap for RPG mode.")

(defvar rpg-data nil
  "Last result.")

;;; Other bindings

(defvar rpg-input)

(defun rpg-input-sender (_proc input)
  ;; Just sets the variable rpg-input, which is in the scope of
  ;; `rpg-send-input's call.
  (setq rpg-input input))

(defun rpg-send-input ()
  "Evaluate the expression after the prompt."
  (interactive)
  (let (rpg-input)			; set by rpg-input-sender
    (comint-send-input)                 ; update history, markers etc.
    (rpg-eval-input rpg-input)))

;;; Utility functions

(defun rpg-is-whitespace (string)
  "Return non-nil if STRING is all whitespace."
  (or (string= string "")
      (string-match-p "\\`[ \t\n]*\\'" string)))

;;; RPG stuff

(defun rpg-roll (number-of-dice size-of-die number-to-add)
  (loop for d from 1 to number-of-dice
	sum (1+ (random size-of-die))
	into result
	return (+ result number-to-add)))

(defun rpg-parse-table (url separator)
  "Download URL and try to find a table using SEPARATOR.
Returns the table as a list of lists and sets `rpg-data'."
  (shell-command (concat "curl " url))
  (let ((document (with-current-buffer "*Shell Command Output*" (buffer-string)))
	result)
    (dolist (line (split-string document "\n"))
      (let ((tokens (split-string line separator t)))
	(when (> (length tokens) 1)
	  (setq result (cons tokens result)))))
    (setq rpg-result result)))

;; (rpg-parse-table "https://campaignwiki.org/wiki/Rasiermesserk%c3%bcste/raw/Status" "|")

(defun rpg-filter-table (&rest patterns)
  "Filter `rpg-result' using a list of regular expressions PATTERNS.
Returns the table as a list of lists and sets `rpg-data'."
  (let (result)
    (dolist (tokens rpg-result)
      (catch 'next
	(dolist (token tokens)
	  (dolist (pattern patterns)
	    (when (string-match pattern token)
	      (setq result (cons tokens result))
	      (throw 'next t))))))
    (setq rpg-result result)))

;; (let ((rpg-result '(("Jask (Claudia)" "1000") ("Fila (Claudia)" "1200") ("Ronon (Lilly)" "1400")))) (rpg-filter-table "claudia"))

(defun rpg-distribute (column amount)
  "Distribute AMOUNT equally amongst the rows of `rpg-result' and add it
to COLUMN (0-indexed)."
  (let* ((n (length rpg-result))
	 (share (/ amount n)))
    (dolist (tokens rpg-result)
      (setcar (nthcdr column tokens)
	      (number-to-string (+ (string-to-number (nth column tokens))
				   share))))
    rpg-result))

;; (let ((rpg-result '(("Jask (Claudia)" "  1000") ("Fila (Claudia)" "  1200")))) (rpg-distribute 1 300))


;;; Command processing
   
(defun rpg-eval-input (input-string)
  "Evaluate the expression INPUT-STRING."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.
  (let ((string input-string)        ; input expression, as a string
        (output "")                  ; result to display
        (pmark (rpg-pm)))
    (unless (rpg-is-whitespace string)
      (cond ((string-match "\\([0-9]+\\)d\\([0-9]+\\)\\(?:+\\([0-9]+\\)\\)?" string)
	     (let ((n (string-to-number (match-string 1 string)))
		   (d (string-to-number (match-string 2 string)))
		   (p (if (match-string 3 string)
			  (string-to-number (match-string 3 string))
			0)))
	       (setq output (number-to-string (rpg-roll n d p))))))
      (goto-char pmark)
      (when (not (equal output ""))
        (setq output (concat output "\n"))))
    (setq output (concat output rpg-prompt-internal))
    (comint-output-filter (rpg-process) output)))

;;; Process and marker utilities

(defun rpg-process nil
  ;; Return the current buffer's process.
  (get-buffer-process (current-buffer)))

(defun rpg-pm nil
  ;; Return the process mark of the current buffer.
  (process-mark (get-buffer-process (current-buffer))))

(defun rpg-set-pm (pos)
  ;; Set the process mark in the current buffer to POS.
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

;;; Major mode

(define-derived-mode rpg-shell-mode comint-mode "RPG"
  "Major mode for interactively evaluating RPG expressions.
Uses the interface provided by `comint-mode' (which see).

* \\<rpg-map>\\[rpg-send-input] evaluates the expression following the prompt.

* \\[rpg-return] inserts a newline and indents, or evaluates a
  complete expression (but see variable `rpg-dynamic-return').
  Inputs longer than one line are moved to the line following the
  prompt (but see variable `rpg-dynamic-multiline-inputs').

* Entry to this mode runs `comint-mode-hook' and `rpg-mode-hook'
 (in that order).

Customized bindings may be defined in `rpg-map', which currently contains:
\\{rpg-map}"
  (setq comint-prompt-regexp (concat "^" (regexp-quote rpg-prompt)))
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (setq comint-input-sender 'rpg-input-sender)
  (setq comint-process-echoes nil)
  (set (make-local-variable 'rpg-prompt-internal) rpg-prompt)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (setq comint-get-old-input 'rpg-get-old-input)
  (setq mode-line-process '(":%s on " (:eval (buffer-name rpg-working-buffer))))

  ;; font-lock support
  (set (make-local-variable 'font-lock-defaults)
       '(rpg-font-lock-keywords nil nil ((?: . "w") (?- . "w") (?* . "w"))))

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "rpg" (current-buffer) "hexl")
      (file-error (start-process "rpg" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (rpg-process) nil)
    (goto-char (point-max))

    ;; Add a silly header
    (insert rpg-header)
    (rpg-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (rpg-process) rpg-prompt-internal)
    (set-marker comint-last-input-start (rpg-pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)))

(defun rpg-get-old-input nil
  ;; Return the previous input surrounding point
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

;;;###autoload
(defun rpg-shell nil
  "Interactively evaluate RPG expressions.
Switches to the buffer `*rpg*', or creates it if it does not exist.
See `rpg-shell-mode' for details."
  (interactive)
  (let (old-point)
    (unless (comint-check-proc "*rpg*")
      (with-current-buffer (get-buffer-create "*rpg*")
        (unless (zerop (buffer-size)) (setq old-point (point)))
        (rpg-shell-mode)))
    (pop-to-buffer-same-window "*rpg*")
    (when old-point (push-mark old-point))))

(provide 'rpg)
