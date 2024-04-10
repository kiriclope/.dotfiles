(require 'map) ;; Needed for map-merge

(setq lc/system-settings
      (append
       ;; Put all system-specific settings at the front so that their values are
       ;; found first

       (when (equal system-name "minibaps")
         '((desktop/dpi . 220)
           (emacs/default-face-size . 180)
           (emacs/variable-face-size . 180)
           (emacs/fixed-face-size . 180)))

       (when (equal system-name "persephone")
         '((desktop/dpi . 220)
           (emacs/default-face-size . 180)
           (emacs/variable-face-size . 180)
           (emacs/fixed-face-size . 180)))

       '((desktop/dpi . 220)
         (emacs/default-face-size . 180)
         (emacs/variable-face-size . 180)
         (emacs/fixed-face-size . 180))))
