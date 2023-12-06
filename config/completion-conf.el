;; Override completion-styles for buffer name completion. Use the same
;; kind of completion for all categories (buffer, unicode-name,
;; project-file, xref-location, info-menu, symbol-help, etc.).
(setq completion-styles '(basic partial-completion substring flex)
      completion-category-overrides nil
      completion-category-defaults nil)
