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
  (evil-define-key 'normal ein:notebook-multilang-mode-map
    (kbd "RET") 'ein:worksheet-execute-cell-and-goto-next)
  (evil-define-key 'normal ein:notebook-mode-map
    (kbd "RET") 'ein:worksheet-execute-cell-and-goto-next)
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
