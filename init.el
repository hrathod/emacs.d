;;; init -- personal config
;;; Commentary:
;;; Personal config

;;; Code:

;;; General
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(define-key prog-mode-map (kbd "C-S-i") 'company-complete)
(define-key prog-mode-map (kbd "C-c g") 'magit-status)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default insert-directory-program "/usr/local/bin/gls")
(global-auto-revert-mode t)

;;; Look & Feel
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-16"))
;;(add-to-list 'default-frame-alist '(alpha . 80))
;; (add-to-list 'default-frame-alist '(foreground-color . "#D9D9D9"))
;; (add-to-list 'default-frame-alist '(background-color . "#353535"))
;; (add-to-list 'default-frame-alist '(scroll-bar-background . "#353535"))
;; (add-to-list 'default-frame-alist '(scroll-bar-foreground . "#555555"))
;; (add-to-list 'default-frame-alist '(border-color . "#353535"))
;; (add-to-list 'default-frame-alist '(line-spacing . 3))
;; (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
;; (add-to-list 'default-frame-alist '(left-fringe . 8))
;; (add-to-list 'default-frame-alist '(right-fringe . 8))
;; (set-face-attribute 'region nil :background "#dffdfd" :foreground "#000000")
;; (set-face-attribute 'fringe nil :background "#353535")
(setq-default left-margin-width 2)
(setq-default right-margin-width 2)
(add-hook 'prog-mode-hook 'linum-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode t)
(column-number-mode t)
(line-number-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode +1)
(setq uniquify-buffer-name-style 'forward)
;; smoother scrolling
(setq scroll-preserve-screen-position t
      scroll-conservatively 100
      maximum-scroll-margin 0.5
      scroll-margin 99999
      scroll-step 1)

;; No bells
(setq visible-bell nil)
(defun my-visible-bell ()
  "Blink mode line instead of weird visual bell behavior."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq ring-bell-function 'my-visible-bell)

;;; Backup & Restore
(setq-default create-lockfiles nil)
(setq auto-save-interval 125)
(setq make-backup-files t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control 1
      vc-make-backup-files t
      vc-follow-symlinks t)
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Code cleanup
(defun code-cleanup ()
  "Clean up current buffer."
  (interactive)
  (whitespace-cleanup)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(define-key prog-mode-map (kbd "C-S-f") 'code-cleanup)

;;; Movement (from Prelude)
(defun my-beginning-of-line (arg)
  "Proper move to beginning of line function.  ARG determines number of lines."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'my-beginning-of-line)

;;; Bootstrap straight.el package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Allow use of the (use-package) macro
(straight-use-package 'use-package)
(setq-default straight-use-package-by-default t)

;;; Common packages
(use-package diminish)
(use-package midnight)
(use-package vterm)
(use-package transpose-frame)
(use-package spacemacs-theme;;solarized-theme;; zenburn-theme
  :defer t ;; <-- this is needed for spacemacs-theme, but not others
  :init
  (progn
    (load-theme 'spacemacs-light t)
    (add-to-list 'default-frame-alist '(font . "Source Code Pro-16"))
    (setq-default left-fringe-width 15)
    (setq-default right-fringe-width 15)
    (setq-default left-margin-width 2)
    (setq-default right-margin-width 2)
    (mapc
     (lambda (face)
       (when (eq (face-attribute face :weight) 'bold)
         (set-face-attribute face nil :weight 'normal)))
     (face-list))
    (set-face-attribute 'fringe nil
                        :foreground (face-foreground 'default)
                        :background (face-background 'default))
    ;;(set-face-attribute 'linum nil
                        ;;:foreground (face-foreground 'default)
                        ;;:background (face-background 'default))
    (set-window-buffer (selected-window) (current-buffer))))
(use-package flycheck
  :init (add-hook 'prog-mode-hook 'flycheck-mode))
(use-package ivy
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)))
(use-package counsel
  :init
  (progn
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)))
(use-package swiper
  :init
  (progn
    (global-set-key (kbd "C-s") 'swiper-isearch)
    (global-set-key (kbd "C-r") 'swiper-isearch-backward)))
(use-package lsp-mode)
(use-package lsp-ui :config (add-hook 'lsp-mode-hook #'lsp-ui-mode))
(use-package lsp-ivy)
(use-package counsel)
(use-package which-key :diminish which-key-mode :config (which-key-mode))
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))
(use-package magit
  :bind (("C-c g" . magit-status)))
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1))
(use-package projectile
  :config
  (progn
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-completion-system 'ivy)
    (setq projectile-enable-caching t)
    (setq projectile-test-suffix-function (lambda (project-type) ".test"))
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "_build")
    (projectile-mode 1)))
(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (progn
    (setq whitespace-line-column 120)
    (setq whitespace-style '(face tabs empty trailing lines-trail))
    (global-whitespace-mode)))
(use-package diff-hl
  :init (setq diff-hl-side 'right)
  (setq diff-hl-draw-borders t)
  :config (global-diff-hl-mode +1))
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
    (setq undo-tree-auto-save-history t)
    (global-undo-tree-mode t)))
(use-package smartparens
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  (progn
    (setq sp-base-key-bindings 'paredit)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil))
  :config
  (progn
    (smartparens-strict-mode +1)
    (smartparens-global-strict-mode +1)
    (show-smartparens-global-mode +1)
    (sp-use-paredit-bindings)
    (turn-on-smartparens-strict-mode)))
(use-package company
  :diminish company-mode
  :init
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-flip-when-above t)
  :config
  (global-company-mode 1))

;;; Organize
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/personal.org"))

;;; Clojure packages
(use-package clojure-mode-extra-font-locking)
(use-package clojure-snippets)
(use-package clojure-mode
  :functions define-clojure-indent
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))
(use-package cider
  :config
  (progn
    (setq cider-shadow-cljs-command "/Users/hvr/.node/current/bin/shadow-cljs")))

;;; Typescript packages
(use-package typescript-mode)
(use-package json-mode)
(use-package lsp-mode
  :commands lsp
  :hook (prog-mode . lsp))
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

;;; PlantUML packages
(use-package plantuml-mode
  :config
  (progn
    (setq plantuml-jar-path (substitute-in-file-name "$HOME/.local/share/plantuml/plantuml.jar"))
    (setq plantuml-default-exec-mode 'jar)
    (org-babel-do-load-languages
     'org-babel-do-load-languages '(plantuml . t))
    (add-hook 'plantuml-mode-hook
              (lambda ()
                (set (make-local-variable 'compile-command)
                     (concat "java -jar "
                             plantuml-jar-path
                             " "
                             (shell-quote-argument buffer-file-name)))))))

;; Mermaid JS
(use-package mermaid-mode)
(use-package ob-mermaid)

;;; Ruby packages
(use-package ruby-mode
  :mode "\\.lit"
  :config
  (progn
    (setq ruby-indent-level 2)))
(use-package rbenv)

;;; Some key mappings
(define-prefix-command 'hr/commands)
(global-set-key (kbd "C-l") 'hr/commands)
(define-key hr/commands (kbd "g") 'magit-status)
(define-key hr/commands (kbd "a") 'counsel-ag)
(define-key hr/commands (kbd "e") 'flycheck-list-errors)
(define-key hr/commands (kbd "f") 'find-file-at-point)
(define-key hr/commands (kbd "p") 'projectile-command-map)
(define-key hr/commands (kbd "c") 'compile)
(define-key hr/commands (kbd "r") 'recompile)

(provide 'init)
;;; init ends here
