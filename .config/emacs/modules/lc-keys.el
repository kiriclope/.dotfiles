;;  -*-lexical-binding: t-*-

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-M-u") 'universal-argument)

(define-key minibuffer-local-map (kbd "C-;") 'backward-delete-char)

(provide 'lc-keys)
