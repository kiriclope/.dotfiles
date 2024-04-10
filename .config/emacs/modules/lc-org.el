;; -*- lexical-binding: t; -*-

;;; -- General Org Mode Config -----

;;;; requirements for scimax

(use-package jupyter)

;; this is my compiled version of zmq
;; if configure: error: cannot find required auxiliary files: config.guess config.sub ar-lib compile missing install-sh then to src and run autoreconf -ivf
;; go back to zmq and run make configure and then make
;;(add-to-list 'load-path "/home/leon/.emacs.d/lisp/zmq")
(use-package zmq
  :load-path "/home/leon/.emacs.d/lisp/zmq")

(use-package ox-ipynb
  :straight (ox-ipynb
             :type git
             :host github
             :repo "jkitchin/ox-ipynb"))

;; getting notebook like experience with scimax
(add-to-list 'load-path "/home/leon/.dotfiles/.emacs.d/lisp/scimax")
(require 'lc-scimax)
;; (use-package scimax
;;   :load-path "/home/leon/.dotfiles/.emacs.d/lisp/scimax")

;;;; custom setup
(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq org-support-shift-select t)
  (diminish org-indent-mode))

(defun my/org-fonts ()

  (set-face-attribute 'org-document-title nil :font "Cantarell" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'medium :height (cdr face))))

;;;; org configuration
(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :bind (("C-c o n" . org-toggle-narrow-to-subtree)
         ("C-c o a" . org-agenda)
         ("C-c o t" . (lambda ()
                        (interactive)
                        ;; Display tasks after selecting tags to filter by
                        (org-tags-view t)))
         ("C-c o c" . 'org-capture)
         ("C-c o x" . 'org-export-dispatch)
         ;; ("C-c o D" . 'dw/org-move-done-tasks-to-bottom)
         :map org-mode-map
         ("M-n" . org-move-subtree-down)
         ("M-p" . org-move-subtree-up))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-capture-bookmark nil)

  ;; (setq org-modules
  ;;       '(org-crypt
  ;;         org-habit
  ;;         org-bookmark
  ;;         org-eshell
  ;;         org-irc))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1))
        ;; Refile items to the top of parent heading
        org-reverse-note-order t)

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  ;; Don't prompt for confirmation when evaluating code block
  (setq org-confirm-babel-evaluate nil)

  ;; Images
  ;; default width images
  (setq org-image-actual-width nil)
  ;; Display inline images on startup
  (setq org-startup-with-inline-images t)

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;;;; images hook
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;;;; subtree C-c C-c
(defun org-babel-execute-subtree ()
  "Execute all source blocks in the current subtree."
  (interactive)
  (org-babel-map-src-blocks nil
    (org-babel-execute-src-block)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-c")
              (lambda (arg)
                (interactive "P")
                (if (and (org-in-src-block-p) (not arg))
                    (org-babel-execute-src-block)
                  (if (org-at-heading-p)
                      (save-restriction
                        (org-narrow-to-subtree)
                        (org-babel-execute-subtree))
                    (call-interactively 'org-ctrl-c-ctrl-c))))))

;;;; org-faces
(use-package org-faces
  :ensure nil
  :straight (:type built-in) ; ensure straight.el knows this is a built-in package
  :after org
  :config
  (my/org-fonts))

;;;; org-tempo
;; This is needed as of Org 9.2
(use-package org-tempo
  :ensure nil
  :straight (:type built-in) ; ensure straight.el knows this is a built-in package
  :after org
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("ip" . "src ipython")
                  ("go" . "src go")
                  ("yaml" . "src yaml")
                  ("json" . "src json")))
    (add-to-list 'org-structure-template-alist item)))

;;;; ansi colored errors
(require 'ansi-color)

(defun org-babel-ansi-colorize-results ()
  "Colorize ansi codes in babel results."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward org-babel-results-keyword nil t)
      (let ((next-head (save-excursion (outline-next-heading))))
        (ansi-color-apply-on-region (point) (or next-head (point-max)))))))

(add-hook 'org-babel-after-execute-hook 'org-babel-ansi-colorize-results)

;;;; org-present
(use-package org-present
  :bind (:map org-present-mode-keymap
              ("C-c C-j" . dw/org-present-next)
              ("C-c C-k" . dw/org-present-prev))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

;;;; org-appear
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

;;;; org-cv
(use-package ox-awesomecv
  :straight '(org-cv :host gitlab :repo "Titan-C/org-cv"))

;;;; org-modern
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config (global-org-modern-mode))

(provide 'lc-org)
