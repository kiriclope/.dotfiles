;; -*- lexical-binding: t; -*-

(use-package hydra)

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit-input)
              :map minibuffer-local-map
              ("M-h" . vertico-directory-up))
  :custom
  (vertico-cycle t)

  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))

  :config
  (require 'vertico-directory)
  (vertico-mode))

(use-package corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)

  :config
  (global-corfu-mode 1)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;;; company
(use-package company
  :bind (("C-<tab>" . company-complete)
         :map company-active-map
         ("TAB" . company-indent-or-complete-common)
         ([tab] . company-indent-or-complete-common)
         ("C-c d" . company-show-doc-buffer))


  :custom
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-limit 5)

  :init
  (global-company-mode 1)

  ;; :config
  ;;(add-hook 'ein:notebook-multilang-mode-hook 'company-mode)  ;; enable company-mode only in ein

  :hook (python-mode . company-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))

;; (use-package company-box
;;     :after company
;;     :config
;;     (setq company-box-show-scrollbar nil)
;;     (setq company-box-max-candidates 5)
;;     :hook (company-mode . company-box-mode))

(use-package company-jedi
  :after company
  :init
  (add-to-list 'company-backends 'company-jedi)
  :hook (add-hook 'python-mode-hook 'company-jedi-setup)
  :config
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t))

(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :demand t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        orderless-matching-styles '(orderless-literal orderless-regexp)
        completion-category-overrides
        '((file (styles partial-completion)))))

(use-package wgrep
  :after consult
  :hook (grep-mode . wgrep-setup))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-x C-b" . consult-buffer)
         :map minibuffer-local-map
         ("C-r" . consult-history))

  :custom
  (consult-project-root-function #'lc/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)

  :config
  (defun lc/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root))))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))

  :custom
  (consult-dir-project-list-function nil))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :config
  (marginalia-mode))

(use-package embark
  :after vertico
  :bind (("C-." . embark-act)
         ("M-." . embark-lcim)
         :map minibuffer-local-map
         ("C-d" . embark-act)
         :map embark-region-map
         ("D" . denote-region))

  :config
  ;; Remove the mixed indicator to prevent the popup from being displayed
  ;; automatically
  (delete #'embark-mixed-indicator embark-indicators)
  (add-to-list 'embark-indicators 'embark-minimal-indicator)

  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after embark)

(provide 'lc-interface)

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

;;  -*-lexical-binding: t-*-
;;; Development

;;;; git
(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :defer t)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t))

;;;; Projectile
(defun dw/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  ;; TODO: Switch to EXWM workspace 1?
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package projectile
  :commands projectile-mode
  :diminish projectile-mode
  :bind ("C-M-p" . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (add-hook 'prog-mode-hook 'eglot-ensure)
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files
          (append '("compile_commands.json") projectile-project-root-files)))
  (add-to-list 'eglot-server-programs '(c++-mode . ((concat eglot-executable "-clangd") "--header-insertion=never")))
  ;; :init
  ;; (when (file-directory-p "~/Projects/Code")
  ;;   (setq projectile-project-search-path '("~/Projects/Code")))
  ;; (setq projectile-switch-project-action #'dw/switch-project-action))
  )

(use-package counsel-projectile
  :disabled
  :after projectile
  :config
  (counsel-projectile-mode))

;;;; Eglot
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; need elpy for doc and imports sorting
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . eglot-format-buffer))
  :hook ((python-mode . eglot-ensure)
         (python-mode . flyspell-prog-mode)
         (python-mode . superword-mode)
         (python-mode . hs-minor-mode)
         (python-mode . (lambda () (set-fill-column 88))))
  :config
  (add-to-list 'eglot-server-programs '(c++-mode . ("ccls" "--init={\"clang\":{\"includePath\":[\"/usr/include/c++/11\"]}}")))

  ;; Use corfu for completions using Eglot
  ;;(add-hook 'eglot-completion-at-point-functions #'corfu-eglot-complete nil t)
  )

;;;; dap-mode
(use-package dap-mode
  :commands dap-debug
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

;;;; cmake
(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'")

;;;; python
(use-package python-mode
  :mode "\\.py\\'"
  :init
  (setq python-shell-interpreter "/home/leon/mambaforge/bin/python3")

  :custom
  (dap-python-executable "/home/leon/mambaforge/bin/python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

;;;; conda
 (use-package
   conda
   :config
   (conda-env-autoactivate-mode t)
   ;; (setq conda-env-home-directory
  ;;       (expand-file-name "~/mambaforge"))
  ;; TODO: we need to activate the envs for python files but not for, e.g., jupyter repl buffer
  :hook (python-mode . (lambda () (conda-env-activate-for-buffer))))

(use-package pyvenv
  :ensure t
  :after conda
  :hook (python-mode . pyvenv-mode)
  :init
  (setenv "WORKON_HOME" "/home/leon/mambaforge/envs/")
  :config
  (setq pyvenv-workon "/home/leon/mambaforge/envs/"))

;;;; ein
(use-package ein
  :config
  (setq ein:output-area-inlined-images t)
  (setq ein:slice-image t)
  (setq ein:completion-backend 'ein:use-company-backend)
  ;; (evil-define-key 'normal ein:notebook-multilang-mode-map
  ;;   (kbd "RET") 'ein:worksheet-execute-cell-and-goto-next)
  ;; (evil-define-key 'normal ein:notebook-mode-map
  ;;   (kbd "RET") 'ein:worksheet-execute-cell-and-goto-next)
  (add-hook 'ein:notebook-mode-hook #'evil-normal-state)

  (defun save-and-keep-state (&rest args)
    (let ((current-state evil-state)) ; capture current state
      (apply args)                    ; apply original function
      (evil-change-state current-state))) ; return to captured state

  (advice-add 'ein:notebook-save-notebook-command :around #'save-and-keep-state)

  (add-hook 'ein:notebook-mode-hook 'undo-tree-mode)

  (defun custom:notebook-mode-hook ()
    (evil-set-initial-state 'ein:notebook-multilang-mode 'insert))
  (add-hook 'ein:notebook-mode-hook 'custom:notebook-mode-hook))

;;;; yaml
(use-package
  yaml-mode
  :mode "\\.yml\\'"
  ;; :hook (yaml-mode . highlight-indent-guides-mode)
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;;; html
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :after web-mode)

(use-package skewer-mode
  :after web-mode)

;;; Productivity
;;;; flycheck
(use-package flycheck
  :config
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
    To override the path to the ruff executable, set
    `flycheck-python-ruff-executable'.
    See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
              "--format=text"
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-ruff)
  :init (global-flycheck-mode))

(use-package flycheck-pycheckers
  :ensure t
  :after flycheck
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

;;;; tree-sitter
(use-package tree-sitter
  :hook (python-mode . tree-sitter-mode)
  :hook (python-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;;;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum-face
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;;;; yasnippet
(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(use-package yasnippet
  :defer t
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs `(,(concat (expand-file-name user-emacs-directory) "snippets")
                           yasnippet-snippets-dir))
  (setq yas-triggers-in-field t)
  (yas-reload-all))

;;;; smartparens
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;;;; rainbow
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         python-mode))

(provide 'lc-dev)
;;; lc-dev.el ends here
