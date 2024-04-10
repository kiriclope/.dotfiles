;;  -*-lexical-binding: t-*-
(setq debug-on-error 1)

;; Add configuration modules to load path
(add-to-list 'load-path '"~/.dotfiles/.config/emacs/modules")

;;; Improve Startup Performance

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(setq byte-compile-warnings '(not obsolete))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'lc-package)
(require 'lc-settings)
(require 'lc-core)
(require 'lc-interface)
(require 'lc-keys)
(require 'lc-evil)
(require 'lc-org)
(require 'lc-dev)
(require 'lc-ai)

(setq gc-cons-threshold (* 2 1000 1000))
