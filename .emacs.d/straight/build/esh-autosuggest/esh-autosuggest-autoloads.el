;;; esh-autosuggest-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "esh-autosuggest" "esh-autosuggest.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from esh-autosuggest.el

(autoload 'esh-autosuggest "esh-autosuggest" "\
`company-mode' backend to provide eshell history suggestion.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'esh-autosuggest-mode "esh-autosuggest" "\
Enable fish-like autosuggestions in eshell.

This is a minor mode.  If called interactively, toggle the
`Esh-Autosuggest mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `esh-autosuggest-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

You can use <right> to select the suggestion. This is
customizable through `esh-autosuggest-active-map'. If
you prefer to use the default value of `company-active-map', you
may set the variable
`esh-autosuggest-use-company-map', though this isn't
recommended as RET and TAB may not work as expected (send input,
trigger completions, respectively) when there is an active
suggestion.

The delay defaults to 0 seconds to emulate fish shell's
instantaneous suggestions, but is customizable with
`esh-autosuggest-delay'.

Note: This assumes you want to use something other than company
for shell completion, e.g. `eshell-pcomplete',
`completion-at-point', or helm-esh-pcomplete, since
`company-active-map', `company-backends', and `company-frontends'
will be locally overriden and company will be used solely for
history autosuggestions.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "esh-autosuggest" '("esh-autosuggest-"))

;;;***

(provide 'esh-autosuggest-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; esh-autosuggest-autoloads.el ends here
