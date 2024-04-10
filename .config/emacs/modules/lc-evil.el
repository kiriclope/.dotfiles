;;  -*-lexical-binding: t-*-
;;; --- Evil ---

(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-d-scroll nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (require 'subr-x)
  ;; Set Emacs state modes
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Basic movement
  (define-key evil-normal-state-map (kbd "C-f") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd "C-b") 'evil-backward-char)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)

  ;; Word movement
  (define-key evil-normal-state-map (kbd "M-f") 'evil-forward-word-end)
  (define-key evil-normal-state-map (kbd "M-b") 'evil-backward-word-begin)
  (define-key evil-insert-state-map (kbd "M-f") 'evil-forward-word-end)
  (define-key evil-insert-state-map (kbd "M-b") 'evil-backward-word-begin)

  (define-key evil-insert-state-map (kbd "C-;") 'backward-delete-char)
  (define-key evil-emacs-state-map (kbd "C-;") 'backward-delete-char)
  (define-key evil-normal-state-map (kbd "C-;") 'backward-delete-char)

  ;; python jump to prev/next def
  (defun my-python-nav-forward-defun ()
    (interactive)
    (let ((current-pos (point)))
      (python-nav-forward-defun)
      (when (equal (point) current-pos)
        (message "Reached the last def in the file"))))

  (defun my-python-nav-backward-defun ()
    (interactive)
    (let ((current-pos (point)))
      (python-nav-backward-defun)
      (when (equal (point) current-pos)
        (message "Reached the first def in the file"))))

  ;; Line movement
  (define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)

  (define-key evil-normal-state-map (kbd "M-$") 'evil-scroll-line-down)
  (define-key evil-normal-state-map (kbd "C-$") 'evil-scroll-line-up)

  (define-key evil-normal-state-map (kbd "M-n") 'next-buffer)
  (define-key evil-normal-state-map (kbd "M-p") 'previous-buffer)

  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-define-key 'normal org-mode-map (kbd "M-n") 'org-babel-next-src-block)

  (evil-define-key 'normal org-mode-map (kbd "M-p") 'org-babel-previous-src-block)
  (evil-define-key 'emacs org-mode-map (kbd "M-n") 'org-babel-next-src-block)
  (evil-define-key 'emacs org-mode-map (kbd "M-p") 'org-babel-previous-src-block)

  ;; (evil-global-set-key 'normal (kbd "M-n") 'next-buffer)
  ;; (evil-global-set-key 'normal (kbd "M-p") 'previous-buffer)

  (defun dw/dont-arrow-me-bro ()
    (interactive)
    (message "Arrow keys are bad, you know?"))

  ;; Disable arrow keys in normal and vsual modes
  ;; (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
  ;; (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
  ;; (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
  ;; (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
  ;; (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
  ;; (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
  ;; (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
  ;; (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(provide 'lc-evil)
