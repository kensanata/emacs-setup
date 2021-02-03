(use-package typo
  :hook text-mode-hook
  :config
  (progn
    (define-typo-cycle typo-cycle-left-single-quotation-mark
      "Cycle through the left single quotation mark and the backtick."
      ("‘" "`" "```"))
    (define-typo-cycle typo-cycle-dashes
      "Cycle through various dashes."
      ("-" ; HYPHEN-MINUS
       "–" ; EN DASH
       "—" ; EM DASH
       "----"
       "−" ; MINUS SIGN
       "‐" ; HYPHEN
       "‑" ; NON-BREAKING HYPHEN
       ))))
