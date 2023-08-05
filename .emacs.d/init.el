;;  -*-lexical-binding: t-*-
(setq debug-on-error 1)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
;; equivalent of use-package-always-ensure
(setq straight-use-package-by-default t)

(use-package exec-path-from-shell
  :ensure t
  :init
  ;; (setq exec-path-from-shell-arguments nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "CONDA_PATH"))))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(set-default-coding-systems 'utf-8)
;; (setq exec-path (append exec-path '("/usr/include/c++/11")))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-M-u") 'universal-argument)

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

  ;; (evil-define-key 'normal python-mode-map (kbd "C-f n") 'my-python-nav-forward-defun)
  ;; (evil-define-key 'normal python-mode-map (kbd "C-f p") 'my-python-nav-backward-defun)
  ;; (evil-define-key 'emacs python-mode-map (kbd "C-f n") 'my-python-nav-forward-defun)
  ;; (evil-define-key 'emacs python-mode-map (kbd "C-f p") 'my-python-nav-backward-defun)

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

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'frame)
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (dw/leader-key-def
    "t" '(:ignore t :which-key "toggles")))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)      ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(setq visible-bell nil)  ; Disable visible bell

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
;; (setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;;  (use-package spacegray-theme :defer t)
(use-package doom-themes :defer t)
(load-theme 'doom-city-lights t)
(doom-themes-visual-bell-config)

(defvar efs/default-font-size 160)
(defvar efs/default-variable-font-size 160)

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;;Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;;Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

(defun dw/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                     (lambda (i) (string-equal (car i) block-name))
                     unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(use-package unicode-fonts
  :disabled
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
   (lambda (block-name)
     (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
   '("Dingbats"
     "Emoticons"
     "Miscellaneous Symbols and Pictographs"
     "Transport and Map Symbols"))
  (unicode-fonts-setup))

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(use-package diminish)

;; You must run (all-the-icons-install-fonts) one time after
;; (all-the-icons-install-fonts)
;; installing this package!

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; ;; Better Modeline
;; (use-package all-the-icons)
;; (nerd-icons-install-fonts)

(use-package nerd-icons)

(use-package
  doom-modeline
  ;; :if (not (display-graphic-p))
  :init
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-go nil)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-hud t)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-persp-name nil)
  :config
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-icon nil)
  (doom-modeline-mode 1)
  (progn
    (require 'doom-modeline-segments)
    ;; https://martinralbrecht.wordpress.com/2020/08/23/conda-jupyter-and-emacs/
    (doom-modeline-def-segment
      conda-env
      "The current conda environment.  Works with `conda'."
      (when (bound-and-true-p conda-env-current-name)
        (propertize (format " |%s|" conda-env-current-name) 'face (if (doom-modeline--active)
                                                                      'mode-line
                                                                    'mode-line-inactive) 'help-echo
                                                                    (format "Conda environment: %s"
                                                                            conda-env-current-name)))))
  (doom-modeline-def-modeline
    'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position
          word-count parrot selection-info conda-env)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes
                  input-method indent-info buffer-encoding major-mode process vcs checker)))

(use-package perspective
  :demand t
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-initial-frame-name "Main")
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(dw/leader-key-def
  "t"  '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(dw/leader-key-def
  "e"   '(:ignore t :which-key "eval")
  "eb"  '(eval-buffer :which-key "eval buffer"))

(dw/leader-key-def
  :keymaps '(visual)
  "er" '(eval-region :which-key "eval region"))

(use-package aggressive-indent
  :commands (aggressive-indent-mode))

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)

(setq-default python-indent 4)
(setq-default python-indent-offset 4)

(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)
         (python-mode . ws-butler-mode)))

(use-package parinfer
  :disabled
  :hook ((clojure-mode . parinfer-mode)
         (emacs-lisp-mode . parinfer-mode)
         (common-lisp-mode . parinfer-mode)
         (scheme-mode . parinfer-mode)
         (lisp-mode . parinfer-mode))
  :config
  (setq parinfer-extensions
        '(defaults       ; should be included.
           pretty-parens  ; different paren styles for different modes.
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank)))  ; Yank behavior depend on mode.

(dw/leader-key-def
  "tp" 'parinfer-toggle-mode)

(use-package origami
  :hook ((yaml-mode . origami-mode)
         (python-mode . origami-mode)))

(defun dw/org-file-jump-to-heading (org-file heading-title)
  (interactive)
  (find-file (expand-file-name org-file))
  (goto-char (point-min))
  (search-forward (concat "* " heading-title))
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun dw/org-file-show-headings (org-file)
  (interactive)
  (find-file (expand-file-name org-file))
  (counsel-org-goto)
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(use-package hydra
  :defer t)

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
  folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-word (- arg))))

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :config
  ;; Grow and shrink Vertico minibuffer
  (setq vertico-resize t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package corfu
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))

  ;; Optional customizations
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)

  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; :config
  (add-hook 'eglot-completion-at-point-functions #'corfu-eglot-complete nil t)
  :init
  (global-corfu-mode))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Use Dabbrev with Corfu!

(use-package dabbrev

  :after corfu
  ;; Swap M-/ and C-M-/
  :bind (("M-:" . dabbrev-completion)
         ("C-M-:" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-project-list-function nil))

;; Thanks Karthik!
(with-eval-after-load 'eshell-mode
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                            (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                   :narrow ?e
                                                   :category file
                                                   :face consult-file
                                                   :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
          (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs))))))))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :bind (("C-s-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult

  :after embark
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package bufler
  :disabled
  :bind (("C-M-j" . bufler-switch-buffer)
         ("C-M-k" . bufler-workspace-frame-set))
  :config
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET")   'bufler-list-buffer-switch
    (kbd "M-RET") 'bufler-list-buffer-peek
    "D"           'bufler-list-buffer-kill)

  (setf bufler-groups
        (bufler-defgroups
         ;; Subgroup collecting all named workspaces.
         (group (auto-workspace))
         ;; Subgroup collecting buffers in a projectile project.
         (group (auto-projectile))
         ;; Grouping browser windows
         (group
          (group-or "Browsers"
                    (name-match "Vimb" (rx bos "vimb"))
                    (name-match "Qutebrowser" (rx bos "Qutebrowser"))
                    (name-match "Chromium" (rx bos "Chromium"))))
         (group
          (group-or "Chat"
                    (mode-match "Telega" (rx bos "telega-"))))
         (group
          ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
          (group-or "Help/Info"
                    (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                    ;; (mode-match "*Helpful*" (rx bos "helpful-"))
                    (mode-match "*Info*" (rx bos "info-"))))
         (group
          ;; Subgroup collecting all special buffers (i.e. ones that are not
          ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
          ;; through to other groups, so they end up grouped with their project buffers).
          (group-and "*Special*"
                     (name-match "**Special**"
                                 (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                     (lambda (buffer)
                       (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                            buffer)
                                   (funcall (mode-match "Dired" (rx bos "dired"))
                                            buffer)
                                   (funcall (auto-file) buffer))
                         "*Special*"))))
         ;; Group remaining buffers by major mode.
         (auto-mode))))

(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(use-package winner
  :config
  (winner-mode)
  )

(defun dw/center-buffer-with-margins ()
  (let ((margin-size (/ (- (frame-width) 80) 3)))
    (set-window-margins nil margin-size margin-size)))

(defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 130
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . dw/org-mode-visual-fill))

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(defun dw/popper-window-height (window)
  (let (buffer-mode (with-current-buffer (window-buffer window)
                      major-mode))
    (pcase buffer-mode
      ('exwm-mode 40)
      (_ 15))))

(use-package popper
  ;; :straight (popper :host github
  ;;                   :repo "karthink/popper"
  ;;                   :build (:not autoloads))
  :bind (("C-M-'" . popper-toggle-latest)
         ("M-'" . popper-cycle)
         ("C-M-\"" . popper-toggle-type))
  :custom
  (popper-window-height 12)
  ;; (popper-window-height
  ;; (lambda (window)
  ;;   (let ((buffer-mode (with-current-buffer (window-buffer window)
  ;;                        major-mode)))
  ;;     (message "BUFFER MODE: %s" buffer-mode)
  ;;     (pcase buffer-mode
  ;;       ('exwm-mode 40)
  ;;       ('helpful-mode 20)
  ;;       ('eshell-mode (progn (message "eshell!") 10))
  ;;       (_ 15)))))
  (popper-reference-buffers
   '("^\\*eshell\\*"
     "^vterm"
     help-mode
     helpful-mode
     compilation-mode))
  :init
  (require 'popper) ;; Needed because I disabled autoloads
  (popper-mode 1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :straight nil
  ;; :after evil
  :defer 1
  :commands (dired dired-jump)
  :config

  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (s-equals? "/gnu/store/" (expand-file-name default-directory))
              (all-the-icons-dired-mode 1)
              (hl-line-mode 1)))

  (use-package dired-rainbow
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

  ;;  (use-package dired-single
  ;;  :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)
  )

;; Increase the size of various headings
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
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'medium :height (cdr face)))

  ;; Make sure org-indent face is available
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))

(defun my/org-block-templates ()

  ;; Block Templates
  ;; the template.  More documentation can be found at the Org Mode [[https://orgmode.org/manual/Easy-templates.html][Easy Templates]]
  ;; documentation page.

  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("img" . "imagecontainer"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("ipy" . "src ipython :results drawer :async t :session mysession"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

;; Org mode
(setq-default fill-column 80)

;; Turn on indentation and auto-fill mode for Org files
(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq org-support-shift-select t)
  (diminish org-indent-mode))

;; Make sure Straight pulls Org from Guix
(straight-use-package '(org :type built-in))

(use-package org
  ;; :straight (org-plus-contrib
  ;;            :type git
  ;;            :host github
  ;;            :repo "emacs-straight/org-mode"
  ;;            :local-repo "org")
  ;; :straight (org-plus-contrib :repo "https://code.orgmode.org/bzg/org-mode.git" :local-repo "org" :files ("*.el" "lisp/*.el" "contrib/lisp/*.el"))
  :ensure t
  :defer t
  :hook ((org-mode . dw/org-mode-setup)
         (org-mode . my/org-fonts)
         (org-mode . my/org-block-templates))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup t
        org-src-preserve-indentation nil
        org-startup-folded nil
        org-cycle-separator-lines 2
        org-capture-bookmark nil)

  (setq org-modules
        '(org-crypt
          org-habit
          ))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (setq org-startup-with-inline-images t) ;; Display inline images on startup
  (setq org-confirm-babel-evaluate nil) ;; Don't prompt for confirmation when evaluating code blocks
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append) ;; Display inline images

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (ipython . t)
     (C . t)
     ))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  (require 'ox-latex)
  (require 'ox-md))

(use-package  ob-ipython
  :after org
  :config
  ;; set default ipython exex
  (setq org-babel-python-command "/home/leon/mambaforge/bin/python3")

  ;; fix bug with json and obipython
  (advice-add 'ob-ipython--collect-json :before
              (lambda (&rest args)
                (let ((start (point)))
                  (set-mark (point))
                  (while (re-search-forward "{" nil t)
                    (backward-char)
                    (kill-region (region-beginning) (region-end))
                    (re-search-forward "}\n" nil t)
                    (set-mark (point)))
                  (end-of-buffer)
                  (kill-region (region-beginning) (region-end))
                  (goto-char start)))))

(use-package ox-ipynb
  :straight (ox-ipynb
             :type git
             :host github
             :repo "jkitchin/ox-ipynb")
  :after org
  :config
  (require 'ox-ipynb))

;; Use bullet characters instead of asterisks, plus set the header font sizes to something more palatable.  A fair amount of inspiration has been taken from [[https://zzamboni.org/post/beautifying-org-mode-in-emacs/][this blog post]].

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(dw/leader-key-def
  "o"   '(:ignore t :which-key "org mode")

  "oi"  '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "insert link")

  "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

  "os"  '(dw/counsel-rg-org-files :which-key "search notes")

  "oa"  '(org-agenda :which-key "status")
  "ot"  '(org-todo-list :which-key "todos")
  "oc"  '(org-capture t :which-key "capture")
  "ox"  '(org-export-dispatch t :which-key "export"))

(defun dw/search-org-files ()
  (interactive)
  (counsel-rg "" "~/Notes" nil "Search Notes: "))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package org-caldav
  :after org
  :init
  (require 'org-caldav)
  (setq org-caldav-url "https://caldav.fastmail.com/dav/calendars/user/daviwil@fastmail.fm/"
        ;; org-caldav-files '("~/Notes/Calendar/Personal.org" "~/Notes/Calendar/Work.org")
        ;; org-caldav-inbox '("~/Notes/Calendar/Personal.org" "~/Notes/Calendar/Work.org")
        org-caldav-calendar-id "fe098bfb-0726-4e10-bff2-55f8278c8a56"
        org-caldav-files '("~/Notes/Calendar/Personal.org")
        org-caldav-inbox "~/Notes/Calendar/PersonalInbox.org"
        org-caldav-calendars
        '((:calendar-id "fe098bfb-0726-4e10-bff2-55f8278c8a56"
                        :files ("~/Notes/Calendar/Personal.org")
                        :inbox "~/Notes/Calendar/PersonalInbox.org"))
        ;; (:calendar-id "8f150437-cc57-4ba0-9200-d1d98389e2e4"
        ;;  :files ("~/Notes/Calendar/Work.org")
        ;;  :inbox "~/Notes/Calendar/Work.org"))
        org-caldav-delete-org-entries 'always
        org-caldav-delete-calendar-entries 'never))

(use-package org-wild-notifier
  :after org
  :config
  ;; Make sure we receive notifications for non-TODO events
  ;; like those synced from Google Calendar
  (setq org-wild-notifier-keyword-whitelist nil)
  (setq org-wild-notifier-notification-title "Agenda Reminder")
  (setq org-wild-notifier-alert-time 15)
  (org-wild-notifier-mode))

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-display-inline-images)
  (dw/org-present-prepare-slide))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (org-appear-mode 1))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide)
  (when (fboundp 'live-crafter-add-timestamp)
    (live-crafter-add-timestamp (substring-no-properties (org-get-heading t t t t)))))

(use-package org-present
  :bind (:map org-present-mode-keymap
              ("C-c C-j" . dw/org-present-next)
              ("C-c C-k" . dw/org-present-prev))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package ox-awesomecv
  :straight '(org-cv :host gitlab :repo "Titan-C/org-cv"))

(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(dw/leader-key-def
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(use-package magit-todos
  :defer t)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t)
  (dw/leader-key-def
    "gL"  'git-link))

(use-package git-gutter
  ;; :straight git-gutter-fringe
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (require 'git-gutter-fringe)
  (set-face-foreground 'git-gutter-fr:added "LightGreen")
    (fringe-helper-define 'git-gutter-fr:added nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
    (fringe-helper-define 'git-gutter-fr:modified nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

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

(dw/leader-key-def
  "pf"  'projectile-find-file
  "ps"  'projectile-switch-project
  "pF"  'consult-ripgrep
  "pp"  'projectile-find-file
  "pc"  'projectile-compile-project
  "pd"  'projectile-dired)

(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package eglot
  :straight t
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(c++-mode . ("ccls" "--init={\"clang\":{\"includePath\":[\"/usr/include/c++/11\"]}}")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio" "--enable-folding" "--completion" "snippets")))
  ;; Use corfu for completions using Eglot
  (add-hook 'eglot-completion-at-point-functions #'corfu-eglot-complete nil t))

(use-package dap-mode
  :commands dap-debug
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  ;; (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))


(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package prettier-js
  ;; :hook ((js2-mode . prettier-js-mode)
  ;;        (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'")

(use-package python-mode
    :mode "\\.py\\'"
    :init
    (setq python-shell-interpreter "/home/leon/mambaforge/bin/python3")

    :custom
    (dap-python-executable "/home/leon/mambaforge/bin/python3")
    (dap-python-debugger 'debugpy)
    :config
    (require 'dap-python))

;; (use-package py-isort
;;   :hook (python-mode . py-isort-before-save)
;;   :config
;;   (setq py-isort-options '("--lines=88" "-m=3" "-tc" "-fgw=0" "-ca"))) ;

(defun call-isort ()
  "Call the isort command on the current buffer"
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           "isort -"
                           (current-buffer)
                           t))

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'call-isort nil t)))

  ;; (use-package py-autoflake
  ;;       :hook (python-mode . py-autoflake-enable-on-save)
  ;;       :config
  ;;       (setq py-autoflake-options '("--expand-star-imports")))

  ;; (use-package py-docformatter
  ;;       :hook (python-mode . py-docformatter-enable-on-save)
  ;;       :config
  ;;       (setq py-docformatter-options '("--wrap-summaries=88" "--pre-summary-newline")))

  (use-package python-docstring
    :hook (python-mode . python-docstring-mode))

  (use-package blacken
    :hook (python-mode . blacken-mode)
      :config (setq blacken-line-length '88))

  (use-package python-black
    :straight t
    :hook (python-mode . python-black-on-save-mode-enable-dwim))

  (defun leon/eglot-format-buffer ()
    "Format current buffer according to LSP server."
    (interactive)
    (if (and (eq major-mode 'python-mode) (executable-find "black"))
        (python-black-buffer)
      (eglot--format-buffer)))

(use-package
  conda
  :config (conda-env-autoactivate-mode t)
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

;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-idle-delay .2)
;;   (setq company-minimum-prefix-length 2)
;;   (add-hook 'ein:notebook-multilang-mode-hook 'company-mode))  ;; enable company-mode only in ein

;; (use-package company-prescient
;;   :after company
;;   :config
;;   (company-prescient-mode))

;; (use-package company-box
;;   :after company
;;   :hook (company-mode . company-box-mode))

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(dw/leader-key-def
  "e"   '(:ignore t :which-key "eval")
  "eb"  '(eval-buffer :which-key "eval buffer"))

(dw/leader-key-def
  :keymaps '(visual)
  "er" '(eval-region :which-key "eval region"))

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

(use-package compile
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

(use-package aggressive-indent
  :commands (aggressive-indent-mode))

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

;; code parsing
(use-package tree-sitter
  :hook (python-mode . tree-sitter-mode)
  :hook (python-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

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

(use-package
  yaml-mode
  :mode "\\.yml\\'"
  ;; :hook (yaml-mode . highlight-indent-guides-mode)
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

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

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         python-mode
         js2-mode))

(use-package app-launcher
    :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

(use-package mu4e-alert

  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query mu4e-inbox-query)

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  (mu4e-alert-enable-notifications))

(use-package all-the-icons)

(use-package webkit
  :straight (:type git
                   :host github
                   :repo "akirakyle/emacs-webkit"
                   :files (:defaults "*.js" "*.css" "*.so")
                   :pre-build ("make"))
  ;; :bind ("s-b" 'webkit)
  :init
  (setq webkit-own-window nil)

  :config
  ;; If you don't care so much about privacy and want to give your data to google
  (setq webkit-search-prefix "https://google.com/search?q=")

  ;; Specify a different set of characters use in the link hints
  ;; For example the following are more convienent if you use dvorak
  (setq webkit-ace-chars "aoeuidhtns")

  ;; If you want history saved in a different place or
  ;; Set to `nil' to if you don't want history saved to file (will stay in memory)
  (setq webkit-history-file "~/.emacs.d/webkit/history")

  ;; If you want cookies saved in a different place or
  ;; Set to `nil' to if you don't want cookies saved
  (setq webkit-cookie-file "~/.emacs.d/webkit/cookies")

  ;; See the above explination in the Background section
  ;; This must be set before webkit.el is loaded so certain hooks aren't installed
  (setq webkit-own-window nil)

  ;; Set webkit as the default browse-url browser
  (setq browse-url-browser-function 'webkit-browse-url)

  ;; Force webkit to always open a new session instead of reusing a current one
  (setq webkit-browse-url-force-new nil)

  ;; Globally disable javascript
  (add-hook 'webkit-new-hook #'webkit-enable-javascript)

  ;; Override the "loading:" mode line indicator with an icon from `all-the-icons.el'
  ;; You could also use a unicode icon like ↺
  (defun webkit--display-progress (progress)
    (setq webkit--progress-formatted
          (if (equal progress 100.0)
              ""
            (format "%s%.0f%%  " (all-the-icons-faicon "spinner") progress)))
    (force-mode-line-update))

  ;; Set action to be taken on a download request. Predefined actions are
  ;; `webkit-download-default', `webkit-download-save', and `webkit-download-open'
  ;; where the save function saves to the download directory, the open function
  ;; opens in a temp buffer and the default function interactively prompts.
  (setq webkit-download-action-alist '(("\\.pdf\\'" . webkit-download-open)
                                       ("\\.png\\'" . webkit-download-save)
                                       (".*" . webkit-download-default)))

  ;; Globally use a proxy
  ;; (add-hook 'webkit-new-hook (lambda () (webkit-set-proxy "socks://localhost:8000")))

  ;; Globally use the simple dark mode
  (setq webkit-dark-mode t))

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun dw/get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun dw/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun dw/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'dw/map-line-to-status-char status-lines)))))

(defun dw/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun dw/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (dw/get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize " ॐ " 'face `(:foreground "white"))
     (propertize (dw/get-prompt-path) 'face `(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground "white"))
        (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground "white"))
        (propertize package-version 'face `(:foreground "#e8a206"))))
     (propertize " • " 'face `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:foreground "white")))))

(add-hook 'eshell-banner-load-hook
          (lambda ()
            (setq eshell-banner-message
                  (concat "\n" (propertize " " 'display (create-image "~/.dotfiles/.emacs.d/images/flux_banner.png" 'png nil :scale 0.2 :align-to "center")) "\n\n"))))

(defun dw/eshell-configure ()
  ;;    (require 'evil-collection-eshell)
  ;;  (evil-collection-eshell-setup)

  (use-package xterm-color)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  ;; (evil-normalize-keymaps)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      'dw/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil))

(use-package eshell
  :hook (eshell-first-time-mode . dw/eshell-configure)
  :bind ("C-r" . 'consult-history)
  :init
  (setq eshell-directory-name "~/.dotfiles/.emacs.d/eshell/"
        eshell-aliases-file (expand-file-name "~/.dotfiles/.emacs.d/eshell/alias")))

(use-package eshell-z
  :disabled ;; Using consult-dir for this now
  :hook ((eshell-mode . (lambda () (require 'eshell-z)))
         (eshell-z-change-dir .  (lambda () (eshell/pushd (eshell/pwd))))))

;; (use-package exec-path-from-shell
;;   :init
;;   (setq exec-path-from-shell-check-startup-files nil)
;;   :config
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize)))

;; (dw/leader-key-def
;;   "SPC" 'eshell)

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim")))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package fish-completion
  :disabled
  :hook (eshell-mode . fish-completion-mode))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

(use-package eshell-toggle
  :disabled
  :after eshell
  :bind ("C-M-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  )

(use-package eat
  :ensure t
  :config
  (eat-eshell-mode)
  (setq eshell-visual-commands '()))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

(use-package polymode)
(use-package poly-markdown)
(use-package request)

(use-package quarto-mode
  :mode ((".qmd\\.Rmd\\'" . poly-quarto-mode))
  :config
  (require 'polymode)
  (require 'poly-markdown)
  (require 'request))

(use-package leetcode
  :config
  (setq leetcode-prefer-language "python3")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/leetcode"))

(use-package gptel
  :straight '(gptel :host github
                    :repo "karthink/gptel"
                    :branch "master")

  :init
  (setq-default gptel-model "gpt-3.5-turbo"
                gptel-playback t
                gptel-api-key (lambda () (string-trim (shell-command-to-string "pass openai")))
                gptel-default-mode 'org-mode))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :after company
  :config (delq 'company-preview-if-just-one-frontend company-frontends)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  :hook ((prog-mode . (lambda () (unless (equal (buffer-name) "*scratch*") copilot-mode)))))

;; move between buffer with "\C-xp"
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(defun efs/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs_cluster.org"))

    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
