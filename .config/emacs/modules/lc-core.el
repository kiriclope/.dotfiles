;;  -*-lexical-binding: t-*-

;;; -- Basic Configuration Paths -----

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :demand t
  :config
  ;; Set the custom-file to a file that won't be tracked by Git
  (setq custom-file (if (boundp 'server-socket-dir)
                        (expand-file-name "custom.el" server-socket-dir)
                      (no-littering-expand-etc-file-name "custom.el")))
  (when (file-exists-p custom-file)
    (load custom-file t))

  ;; Don't litter project folders with backup files
  (let ((backup-dir (no-littering-expand-var-file-name "backup/")))
    (make-directory backup-dir t)
    (setq backup-directory-alist
          `(("\\`/tmp/" . nil)
            ("\\`/dev/shm/" . nil)
            ("." . ,backup-dir))))

  (setq auto-save-default nil)

  ;; Tidy up auto-save files
  (setq auto-save-default nil)
  (let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
    (make-directory auto-save-dir t)
    (setq auto-save-file-name-transforms
          `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
             ,(concat temporary-file-directory "\\2") t)
            ("\\`\\(/tmp\\|/dev/shm\\)\\([^/]*/\\)*\\(.*\\)\\'" "\\3")
            ("." ,auto-save-dir t)))))

;;; -- Native Compilation -----

;; Silence compiler warnings as they can be pretty disruptive
;; (setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;;; -- Basic Emacs Settings -----

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(setq-default fill-column 220)

(setq visible-bell t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)       ;; scroll window under mouse
(setq scroll-step 1)                    ;; keyboard scroll one line at a time
;; (setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha . (90 90)))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Use UTF-8 by default
(set-default-coding-systems 'utf-8)

(repeat-mode 1)

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)
(setq ad-redefinition-action 'accept)

(defun my/change-org-font-size (size)
  "Change font size for Org mode files and block cells to SIZE."
  (interactive "nFont size: ") ; Allows you to input the font size dynamically
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height size)
  ;;Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height size :weight 'regular)
  (set-face-attribute 'org-block nil :height size) ; Change font size for org blocks
  (my/org-fonts))

(defun my/change-font-size (size)
  "Change font size."
  (interactive "nFont size: ") ; Allows you to input the font size dynamically
  (set-face-attribute 'default nil :height size)) ; Change font size for the buffer

;;; -- Appearance -----

(use-package doom-themes
  :config
  ;; TODO: Move this to a system setting
  (load-theme
   (pcase system-name
     ("persephone" 'doom-city-lights)
     ("minibaps" 'doom-city-lights)
     (_ 'doom-palenight))
   t)

  (doom-themes-visual-bell-config))

;; ;; TODO: Do I use this?  Is it needed?
;; (use-package default-text-scale
;;   :config
;;   (default-text-scale-mode))

;; Set the font face based on platform
(set-face-attribute 'default nil
                    :font "Fira Code Retina"
                    ;; :weight 'normal
                    :height (lc/system-settings-get 'emacs/default-face-size))

;; ;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "Fira Code Retina"
                    ;; :weight 'light
                    :height (lc/system-settings-get 'emacs/fixed-face-size))

;; ;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Cantarell"
                    :weight 'regular
                    :height (lc/system-settings-get 'emacs/variable-face-size))

(setq display-time-format "%l:%M %p %b %d W%U"
      display-time-load-average-threshold 0.0)

;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!

;;; -- Mode Line -----

(use-package diminish)

(use-package minions
  :after doom-modeline
  :hook (doom-modeline-mode . minions-mode))

;; You must run (all-the-icons-install-fonts) one time after
;; (all-the-icons-install-fonts)
;; installing this package!
(use-package all-the-icons)

;; (nerd-icons-install-fonts)
;; (use-package nerd-icons)

(use-package diminish)

(use-package
  doom-modeline
  :init
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-go nil)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-hud t)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-persp-name nil)
  :config
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-time t)
  (setq doom-modeline-env-version t)

  (doom-modeline-mode 1))

(use-package highlight-indent-guides
  :hook (python-mode . highlight-indent-guides-mode)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (setq highlight-indent-guides-method 'character))

(use-package outshine
  :hook (emacs-lisp-mode . outshine-mode))

;;; -- Editing Configuration -----

(setq-default tab-width 4
              indent-tabs-mode nil)

(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode))

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  ;; (add-to-list 'super-save-predicates (lambda ()
  ;;                                       (not (eq major-mode 'mu4e-compose-mode))))
  )

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(use-package paren
  :ensure nil
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t))

(use-package avy
  :bind (("C-'" . avy-goto-char)
         ("C-;" . avy-goto-char-timer))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-single-candidate-jump nil)
  :config
  (defun dw/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'dw/avy-action-embark))

;;; -- Window Management -----

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t))

(use-package winner
  :config
  (winner-mode))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(defun dw/popper-window-height (window)
  (let (buffer-mode (with-current-buffer (window-buffer window)
                      major-mode))
    (pcase buffer-mode
      ('exwm-mode 40)
      (_ 15))))

(use-package popper
  :bind (("C-M-'" . popper-toggle-latest)
         ("M-'" . popper-cycle)
         ("C-M-\"" . popper-toggle-type))
  :custom
  (popper-window-height 12)
  (popper-reference-buffers '(eshell-mode
                              vterm-mode
                              geiser-repl-mode
                              help-mode
                              grep-mode
                              helpful-mode
                              compilation-mode))
  :config
  (require 'popper) ;; Needed because I disabled autoloads
  (popper-mode 1))

;;; -- Dired -----

(use-package all-the-icons-dired)
(use-package dired-ranger)

(defun dw/dired-mode-hook ()
  (interactive)
  ;; (dired-omit-mode 1)
  (dired-hide-details-mode 1)
  (all-the-icons-dired-mode 1)
  (hl-line-mode 1))

(use-package dired
  :ensure nil
  :straight (:type built-in) ; ensure straight.el knows this is a built-in package
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-dwim-target 'dired-dwim-target-next
        dired-hide-details-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-mode-hook #'dw/dired-mode-hook)

  (global-set-key (kbd "s-e") #'dired-jump))

;;; -- Save Minibuffer History -----

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;;; -- Make Help More Helpful -----

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)))

;; Load the info system for info files
(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

(use-package exec-path-from-shell
  :ensure t
  :init
  ;; (setq exec-path-from-shell-arguments nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "CONDA_PATH"))))

;;; -- Foot Support -----

(add-to-list 'term-file-aliases '("foot" . "xterm"))

;;; -- Start the Daemon -----

(server-start)

(provide 'lc-core)
