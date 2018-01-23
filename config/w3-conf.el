(setq font-window-system-mappings
  '((x         . (x-font-create-name x-font-create-object))
    (gtk       . (x-font-create-name x-font-create-object))
    (mswindows . (mswindows-font-create-name mswindows-font-create-object))
    (win32    . (x-font-create-name x-font-create-object))
    (mac      . (x-font-create-name x-font-create-object))
    (w32      . (x-font-create-name x-font-create-object))
    (pm       . (x-font-create-name x-font-create-object)) ; Change? FIXME
    (tty      . (tty-font-create-plist tty-font-create-object)))
  w3--args nil)

