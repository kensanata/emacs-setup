;; Bookmark list with Elpher UI

(defun elpher-bookmark-bmenu-list ()
  "Display a list of existing bookmarks.
The list is displayed in a buffer named ‘*Bookmark List*’.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying.
This command differs from \\[bookmark-bmenu-list] in that
the keymap is more Elpher-like."
  (interactive)
  (call-interactively 'bookmark-bmenu-list)
  (use-local-map elpher-bookmark-integration-map))

(define-key elpher-mode-map "B" #'elpher-bookmark-bmenu-list)

(defvar elpher-bookmark-integration-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map bookmark-bmenu-mode-map)
    (define-key map "m" #'bookmark-bmenu-search)
    map)
  "Additional keybindings for \\[elpher-bookmark-bmenu-list].")

(provide 'elpher-bookmark-integration)
