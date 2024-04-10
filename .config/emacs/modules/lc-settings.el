;; -*- lexical-binding: t; -*-

(defun lc/load-system-settings ()
  (interactive)
  (load-file "~/.dotfiles/.config/emacs/per-system-settings.el"))

(defun lc/system-settings-get (setting)
  (alist-get setting lc/system-settings))

;; Load settings for the first time
(lc/load-system-settings)

(provide 'lc-settings)
