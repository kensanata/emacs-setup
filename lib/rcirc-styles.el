;;; rcirc-styles.el --- support mIRC-style color and attribute codes

;; Package-Version: 20150720.001
;; Copyright 2015 Aaron Miller <me@aaron-miller.me>
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; The de facto standard for IRC color codes, as originally
;; implemented in mIRC, can be found at
;; http://en.wikichip.org/wiki/irc/colors .

;; As may be expected from the context, it's a bit ad hoc and not the
;; easiest thing in the world to parse correctly, which may explain why a
;; prior attempt to satisfy this use case, under the name of
;; "rcirc-controls.el", attempted to do so with a recondite yet woefully
;; inadequate regexp.

;; Rather than attempt to fix the regexp, and even if successful make
;; it even more incomprehensible than it started out being, I decided
;; it'd be easier and more maintainable to write a string-walking
;; parser.

;; So I did that.  In addition to those cases supported in the previous
;; library, this code correctly handles:
;; * Background colors, including implicit backgrounds when a new code
;;   provides only a foreground color.
;; * Colors at codes between 8 and 15 (and correct colors for codes < 8).
;; * Color specifications implicitly terminated by EOL.
;; * Color specifications implicitly terminated by new color
;;   specifications.
;; * The ^O character terminating color specifications.

;; While I was at it, I noticed some areas in which the both the stock
;; attribute markup function in rcirc, and the one provided in
;; the previous library, could use improving.

;; So I did that too, and the following cases are now correctly handled:
;; * Implicit termination of attribute markup by EOL.
;; * ^V as the specifier for reverse video, rather than italics.
;; * ^] as the specifier for italics.

;; As far as I'm aware, this code implements correct and complete
;; support for mIRC colors and attributes.  If I've missed something,
;; let me know! The canonical version of this file lives in the repo
;; at https://github.com/aaron-em/rcirc-styles.el, and that's the
;; place to open issues -- or, even better, pull requests.

;; Finally, a note: Since this package entirely obsoletes
;; rcirc-controls, it will attempt rather vigorously to disable its
;; predecessor, by removing rcirc-controls' hooks from
;; `rcirc-markup-text-functions' if they are installed.  Not to do so,
;; when both packages are loaded, would result in severely broken
;; style markup behavior.

;;; Code:

(require 'cl-lib)
(require 'rcirc)

(defvar rcirc-styles-attribute-alist
  '(("\C-b" . bold)
    ("\C-]" . italic)
    ("\C-_" . underline)
    ("\C-v" . inverse))
  "mIRC text attribute specification characters.")

(defvar rcirc-styles-color-vector ["white"
                            "black"
                            "blue"
                            "green"
                            "red"
                            "brown"
                            "purple"
                            "orange"
                            "yellow"
                            "light green"
                            "cyan"
                            "light cyan"
                            "light blue"
                            "pink"
                            "gray"
                            "light gray"]
  "Vector of color names for the numbers 0-15.")

;; Colors and attributes are actually orthogonal, with the exception
;; of the ^O sequence that turns off both. So we need to handle them
;; accordingly, with one function to mark up attributes, another to
;; mark up colors, and a third which executes after both and cleans up
;; the leftover ^Os.

;; Since these functions are order-dependent (in that things go
;; horribly wrong if the ^Os are removed before both of the markup
;; functions have run), we wrap them all in a single defun, in a safe
;; execution order, and hang that off the
;; `rcirc-markup-text-functions' hook.

(if (not (facep 'inverse))
    (progn
      (make-face 'inverse)
      (set-face-inverse-video-p 'inverse t)))

(defun rcirc-styles-markup-colors (&rest ignore)
  "Mark up received messages with foreground and background colors,
according to the de facto IRC standard at
http://en.wikichip.org/wiki/irc/colors.

This function is intended to be hung off `rcirc-styles-markup-styles',
which is rather magical. It probably will not do what you have in
mind when invoked outside that context."
  (let* (;; a list of char ranges we'll want to delete, as (offset . length)
         deletes
         ;; a list of ranges we'll want to facify, as plists (see below)
         ranges
         ;; the current foreground and background colors, if any
         fg bg)

    ;; begin from the start of the new message
    (goto-char (point-min))

    ;; walk along the message
    (while (not (cl-equalp (point) (point-max)))
      
      ;; ^O means "turn off all formatting"
      (if (looking-at "\C-o")
          (progn
            (setq fg nil)
            (setq bg nil)))

      ;; ^C introduces a color code
      (if (not (looking-at "\C-c"))
          ;; if we're not looking at a color code, just move ahead to the next char
          (forward-char 1)
        ;; otherwise, parse the color code and capture the facification info
        (let ((delete-from (point))
              (delete-length 1)
              range)

          ;; we have a foreground color spec
          (if (looking-at "\C-c\\([0-9]\\{1,2\\}\\)")
              (progn
                ;; capture the specified color
                (setq fg (string-to-number (match-string 1)))
                ;; extend the delete length to include it
                (setq delete-length (+ delete-length
                                       (length (match-string 1))))
                ;; move point past it
                (forward-char (1+ (length (match-string 1))))))

          ;; we also have a background color spec
          (if (looking-at ",\\([0-9]\\{1,2\\}\\)")
              (progn
                ;; capture it
                (setq bg (string-to-number (match-string 1)))
                ;; extend delete length over it
                (setq delete-length (+ delete-length 1
                                       (length (match-string 1))))
                ;; move point past it
                (forward-char (1+ (length (match-string 1))))))

          ;; push a facification spec plist into `range'
          (setq range `(:from ,(point)
                        ;; range ends at:
                        :to ,(save-excursion
                               ;; either the next relevant control char
                               (or (re-search-forward "\C-o\\|\C-c" nil t)
                                   ;; or the last char before whitespace to eol
                                   (re-search-forward "[:space:]*$" nil t)
                                   ;; or, failing all else, point-max
                                   (point-max)))
                        :fg ,fg
                        :bg ,bg))
          (setq ranges (push range ranges))

          ;; push the delete specification into `deletes'
          (setq deletes (push
                         `(,delete-from . ,delete-length)
                         deletes))

          ;; finally, move point to the next unexamined char
          (forward-char 1))))

    ;; facify all the known ranges, ignoring unspecified or
    ;; out-of-range color values
    (dolist (range ranges)
      (let ((fg (plist-get range :fg))
            (bg (plist-get range :bg))
            face)
        (when (and fg (< fg 16))
          (setq face (push (cons 'foreground-color
                                 (aref rcirc-styles-color-vector fg))
                           face)))
        (when (and bg (< bg 16))
          (setq face (push (cons 'background-color
                                 (aref rcirc-styles-color-vector bg))
                           face)))
        (rcirc-add-face (plist-get range :from) (plist-get range :to)
                        face)))

    ;; delete all the deletion ranges, getting rid of literal control codes
    ;; NB `deletes' is in last-to-first order, so we can just iterate
    ;; NB it like this without needing to adjust as we go
    (dolist (pair deletes)
      (goto-char (car pair))
      (delete-char (cdr pair)))))

(defun rcirc-styles-markup-attributes (&rest ignore)
  "Mark up received messages with text attributes.

This function marks up newly received IRC messages with text
attributes (bold, italic, underline, and reverse video) according to
the de facto IRC standard at http://en.wikichip.org/wiki/irc/colors.

rcirc as shipped in Emacs 24 actually includes a function by this
name, but it does not support reverse video, and it uses the
wrong control character for italics; it also fails to recognize
implicit termination of attributes by EOL, and fails to mark up
such cases.

This function is intended to be hung off `rcirc-styles-markup-styles',
which is rather magical.  It probably will not do what you have in
mind when invoked outside that context."
  (let* (deletes
         ranges
         attrs)
    (goto-char (point-min))
    (while (not (cl-equalp (point) (point-max)))
      
      ;; ^O means "turn off all formatting"
      (if (looking-at "\C-o")
          (progn
            (setq attrs nil)))

      ;; Check each attribute character in turn, to see if we're
      ;; looking at it.
      (dolist (pair rcirc-styles-attribute-alist)
        (let ((char (car pair))
              (face (cdr pair)))
          ;; If so, toggle that attribute in `attrs'...
          (when (looking-at char)
            (setq deletes (push `(,(point) . 1) deletes))
            (forward-char 1)
            (setq attrs
                  (if (member face attrs)
                      (cl-remove-if #'(lambda (e) (eq face e)) attrs)
                    (push face attrs)))
            ;; ...and, when there are attributes to apply, push a
            ;; range that does so.
            (when attrs
              (setq ranges (push
                            `(:from ,(point)
                              :to ,(save-excursion
                                     ;; either the next relevant control char
                                     (or (re-search-forward
                                          (concat "\C-o\\|" char) nil t)
                                         ;; or the last char before whitespace to eol
                                         (re-search-forward "[:space:]*$" nil t)
                                         ;; or, failing all else, point-max
                                         (point-max)))
                              :attrs ,attrs)
                            ranges))))))
      (forward-char 1))

    ;; As in `rcirc-styles-markup-colors', q.v.
    (dolist (range ranges)
      (let (face)
        (dolist (attr (plist-get range :attrs))
          (setq face (push attr face)))
        (rcirc-add-face (plist-get range :from) (plist-get range :to) face)))
    
    ;; As in `rcirc-styles-markup-colors', q.v.
    (dolist (pair deletes)
      (goto-char (car pair))
      (delete-char (cdr pair)))))

(defun rcirc-styles-markup-remove-control-o (&rest ignore)
  "Remove all the ^O characters from a string.

This function is intended to be hung off `rcirc-styles-markup-styles',
which is rather magical.  It probably will not do what you have in
mind when invoked outside that context."
  (while (re-search-forward "\C-o" nil t)
    (backward-delete-char 1)))

(defun rcirc-styles-markup-styles (&rest ignore)
  "Apply rcirc-styles color/attribute markup.

This function is intended to be hung off
`rcirc-markup-text-functions', which invokes some magic to
constrain point within the bounds of the newly received
message.  It probably will not do what you have in mind when
invoked outside that context."
  (save-excursion
    (rcirc-styles-markup-colors))
  (save-excursion
    (rcirc-styles-markup-attributes))
  (save-excursion
    (rcirc-styles-markup-remove-control-o)))

(defun rcirc-styles-disable-rcirc-controls nil
  "Disable rcirc-controls.el, if it is installed."
  (remove-hook 'rcirc-markup-text-functions #'rcirc-markup-controls)
  (remove-hook 'rcirc-markup-text-functions #'rcirc-markup-colors))

(defun rcirc-styles-activate nil
  "Activate rcirc-styles.el; if necessary, disable rcirc-controls."
  ;; forcibly supersede broken rcirc-controls.el to avoid broken behavior
  (when (featurep 'rcirc-controls)
    (message "rcirc-styles obsoletes rcirc-controls; disabling rcirc-controls.")
    (rcirc-styles-disable-rcirc-controls))
  
  ;; rcirc.el already added this hook (and defined it, badly) - we need
  ;; to get rid of it...
  (remove-hook 'rcirc-markup-text-functions 'rcirc-markup-attributes)
  ;; ...then add our own hook that does all our markup.
  (add-hook 'rcirc-markup-text-functions 'rcirc-styles-markup-styles))

;; activate package on load
(rcirc-styles-activate)
;; activate package (again, idempotently) once init is complete, to
;; ensure we catch and supersede rcirc-controls if installed
(add-hook 'after-init-hook #'rcirc-styles-activate)
(provide 'rcirc-styles)

;;; rcirc-styles.el ends here
