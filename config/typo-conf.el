(use-package typo
  :hook text-mode-hook
  :config (define-typo-cycle typo-cycle-left-single-quotation-mark
	    "Cycle through the left single quotation mark and the backtick.

If used with a numeric prefix argument N, N backticks will be inserted."
  ("‘" "`" "```")))
