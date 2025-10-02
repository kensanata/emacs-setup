;;; Code:

;; I no longer know how custom themes work. It seems I cannot extend
;; themes in the ways I want them to. So now I'm falling back to using
;; defface all over the place.
(face-spec-set 'default '((t (:family "Iosevka" :height 120 :background "#ffe" :foreground "#111"))))
(face-spec-set 'fixed-pitch '((t (:family "Iosevka Term"))))
(face-spec-set 'fixed-pitch-serif '((t (:family "Iosevka Term"))))
(face-spec-set 'variable-pitch '((t (:family "Iosevka Etoile"))))

(face-spec-set 'error '((t (:foreground "tomato"))))
(face-spec-set 'region '((t (:background "#eeb"))))
(face-spec-set 'secondary-selection '((t (:background "#ffa"))))
(face-spec-set 'mode-line-inactive '((t (:foreground "grey30"))))
(face-spec-set 'mode-line-highlight '((t (:foreground "dim gray"))))
(face-spec-set 'query-replace '((t (:background "dark gray" :foreground "#ffe"))))
(face-spec-set 'git-gutter:added '((t (:foreground "#111"))))
(face-spec-set 'git-gutter:deleted '((t (:foreground "#111"))))
(face-spec-set 'git-gutter:modified '((t (:foreground "#111"))))
(face-spec-set 'git-gutter:separator '((t (:foreground "#111"))))

(face-spec-set 'highlight '((t (:foreground "grey30"))))
(face-spec-set 'idle-highlight '((t (:background "light gray"))))

(face-spec-set 'completions-highlight '((t (:background "light gray"))))

(face-spec-set 'dictionary-reference-face '((t (:inherit default))))

(face-spec-set 'eglot-highlight-symbol-face '((t (:background "light gray"))))

(face-spec-set 'eshell-prompt '((t (:foreground "#111"))))

(face-spec-set 'ediff-odd-diff-A '((t (:background "#eee"))))
(face-spec-set 'ediff-even-diff-A '((t (:background "#eee"))))
(face-spec-set 'ediff-odd-diff-B '((t (:background "#eee"))))
(face-spec-set 'ediff-even-diff-B '((t (:background "#eee"))))

(face-spec-set 'font-lock-builtin-face '((t (:foreground "#111"))))
(face-spec-set 'font-lock-comment-face '((t (:foreground "dim gray"))))
(face-spec-set 'font-lock-comment-delimiter-face '((t (:inherit font-lock-comment-face))))
(face-spec-set 'font-lock-constant-face '((t (:inherit bold :foreground "#111"))))
(face-spec-set 'font-lock-doc-face '((t (:inherit font-lock-comment-face :slant italic))))
(face-spec-set 'font-lock-function-name-face '((t (:foreground "#111"))))
(face-spec-set 'font-lock-keyword-face '((t (:inherit font-lock-constant-face :foreground "#111"))))
(face-spec-set 'font-lock-preprocessor-face '((t (:inherit italic))))
(face-spec-set 'font-lock-reference-face '((t (:inherit default))))
(face-spec-set 'font-lock-string-face '((t (:foreground "steel blue"))))
(face-spec-set 'font-lock-type-face '((t (:inherit underline :foreground "#111"))))
(face-spec-set 'font-lock-variable-name-face '((t (:foreground "#111"))))
(face-spec-set 'font-lock-variable-use-face '((t (:foreground "#111"))))
(face-spec-set 'font-lock-warning-face '((t (:foreground "tomato"))))

(face-spec-set 'dired-directory '((t (:inherit font-lock-string-face))))

(face-spec-set 'hbut-face '((t (:foreground "steel blue" :underline t))))
(face-spec-set 'hbut-flash '((t (:foreground "#111" :background "#ffe" :inverse-video t))))
(face-spec-set 'hbut-item-face '((t (:background "grey30"))))

(face-spec-set 'isearch '((t (:foreground "#111" :background "#cfc"))))
(face-spec-set 'isearch-fail '((t (:foreground "#111" :background "#fdd"))))
(face-spec-set 'isearch-group-1 '((t (:foreground "#111" :background "#bfb"))))
(face-spec-set 'isearch-group-2 '((t (:foreground "#111" :background "#9d9"))))
(face-spec-set 'lazy-highlight '((t (:foreground "#111" :background "#dfd"))))

(face-spec-set 'magit-bisect-bad '((t (:foreground "#111" :bold t))))
(face-spec-set 'magit-bisect-good '((t (:foreground "#111" :bold t))))
(face-spec-set 'magit-bisect-skip '((t (:foreground "#111" :bold t))))
(face-spec-set 'magit-branch-remote '((t (:foreground "#111" :bold t))))
(face-spec-set 'magit-branch-warning '((t (:foreground "tomato"))))
(face-spec-set 'magit-header-line '((t (:foreground "#111"))))
(face-spec-set 'magit-section-heading '((t (:foreground "#111" :bold t))))
(face-spec-set 'magit-section-highlight '((t (:foreground "#111"))))
(face-spec-set 'magit-sequence-head '((t (:foreground "#111"))))

(face-spec-set 'markdown-blockquote-face '((t (:foreground "dim gray"))))

(face-spec-set 'rcirc-my-nick '((t (:foreground "steel blue"))))
(face-spec-set 'rcirc-dim-nick '((t (:foreground "dim gray"))))
(face-spec-set 'rcirc-prompt '((t (:inherit bold))))

(face-spec-set 'sh-quoted-exec '((t (:foreground "tomato"))))

(face-spec-set 'wgrep-delete-face '((t (:inherit font-lock-comment-face :strike-through t))))
(face-spec-set 'wgrep-done-face '((t (:inherit default))))
(face-spec-set 'wgrep-face '((t (:inherit default))))
(face-spec-set 'wgrep-file-face '((t (:inherit default))))

(face-spec-set 'tex-verbatim '((t (:foreground "dim gray"))))

(setq ansi-color-names-vector
      ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"])
