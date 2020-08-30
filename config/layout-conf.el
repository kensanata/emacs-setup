;; If I've set up my windows just right using a ton of C-x 2 and C-x 3
;; and C-x + then I don't want buffers popping up to disrupe this.

;; Sandra of https://idiomdrottning.org/ suggested using display-buffer-alist
(dolist (name '("\\*shell\\*" "\\*scheme\\*" "\\*Help\\*" "\\*Buffer List\\*"
		"\\*Man " "^magit:"))
  (push (cons name display-buffer--same-window-action) display-buffer-alist))
