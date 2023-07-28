;;; pyenv-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "pyenv" "pyenv.el" (0 0 0 0))
;;; Generated autoloads from pyenv.el

(autoload 'pyenv-use-global "pyenv" "\
activate pyenv global python" t nil)

(autoload 'pyenv-use-corresponding "pyenv" "\
search for .python-version and activate the corresponding python" t nil)

(autoload 'pyenv-use "pyenv" "\
choose what python you want to activate

\(fn PYTHON-VERSION)" t nil)

(defvar global-pyenv-mode nil "\
Non-nil if Global Pyenv mode is enabled.
See the `global-pyenv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pyenv-mode'.")

(custom-autoload 'global-pyenv-mode "pyenv" nil)

(autoload 'global-pyenv-mode "pyenv" "\
use pyenv to configure the python version used by your Emacs.

This is a minor mode.  If called interactively, toggle the
`Global Pyenv mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='global-pyenv-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pyenv" '("pyenv"))

;;;***

;;;### (autoloads nil nil ("pyenv-pkg.el") (0 0 0 0))

;;;***

(provide 'pyenv-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyenv-autoloads.el ends here
