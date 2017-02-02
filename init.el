;;; init -- personal config
;;; Commentary:
;;; Personal config

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package midnight
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package which-key
  :ensure t)

(use-package ace-window
  :ensure t
  :init (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  (setq aw-scope 'frame)
  :bind (("C-x o" . ace-window)
         ("C-c w d" . ace-delete-window)))

(use-package anzu
  :ensure t
  :config (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package ediff
  :ensure t
  :init (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl
  :ensure t
  :init (setq diff-hl-side 'right)
  (setq diff-hl-draw-borders t)
  :config (global-diff-hl-mode +1))

(use-package company
  :ensure t
  :diminish company-mode
  :init (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-flip-when-above t)
  :config (global-company-mode 1))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  :config (global-undo-tree-mode t))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init (require 'helm-config)
  :config
  (progn
    (helm-mode 1)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action))
  :bind
  (("C-c h" . helm-command-prefix)
   ("C-c h h" . helm-mini)
   ("C-c h x" . helm-M-x)
   ("C-c h b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-c h w" . helm-google-suggest)
   ("C-c h o" . helm-occur)))

(use-package helm-swoop
  :ensure t
  :bind
  (("C-c h s" . helm-swoop)
   ("C-s" . helm-swoop)
   ("C-r" . helm-swoop)))

(use-package helm-git-grep
  :ensure t
  :bind ("C-c h g" . helm-git-grep))

(use-package helm-descbinds
  :ensure t
  :bind (("C-x w" . helm-descbinds)))

(use-package helm-flycheck
  :ensure t
  :bind (("C-c h e" . helm-flycheck)))

(use-package ag
  :ensure t)

(use-package helm-ag
  :ensure t
  :bind (("C-c h a" . helm-ag)))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :init
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face tabs empty trailing lines-trail))
  :config
  (global-whitespace-mode))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package projectile
  :ensure t
  :init
  (progn
    (setq projectile-completion-system 'helm)
    (setq projectile-enable-caching t)
    (setq projectile-test-suffix-function (lambda (project-type) ".test")))
  :config
  (progn
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (projectile-mode 1)))

;;;(use-package helm-projectile
;;;             :ensure t
;;;             :init (setq projectile-completion-system 'helm)
;;;                   ;;;(setq projectile-enable-caching t)
;;;             :config (helm-projectile-on))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package web-mode
  :ensure t
  :init
  (progn
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-indent-style 4)
    (setq web-mode-enable-auto-pairing nil))
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package flycheck
  :ensure t
  :init (setq flycheck-disabled-checkers '(javascript-jscs javascript-jshint))
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package js2-mode
  :ensure t
  :init
  (progn
    (setq js2-indent-switch-body t)
    (setq js2-bounce-indent-p t)
    (setq js2-global-externs '("module" "require" "sinon" "assert"
                               "setTimeout" "clearTimeout" "setInterval"
                               "clearInterval" "console" "JSON"
                               "describe" "it" "before" "beforeEach"
                               "after" "afterEach" "process" "Buffer")))
  :functions flycheck-select-checker
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook (lambda ()
                               (flycheck-select-checker 'javascript-eslint)))))

(use-package company-tern
  :ensure t
  :diminish tern-mode
  :config
  (progn
    (add-to-list 'company-backends 'company-tern)
    (add-hook 'js2-mode-hook (lambda ()
                               (tern-mode 1)))))

(use-package json-mode
  :ensure t)

(use-package js-doc
  :ensure t
  :init
  (define-key js2-mode-map (kbd "C-c * f") 'js-doc-insert-function-doc)
  (define-key js2-mode-map (kbd "C-c * t") 'js-doc-insert-tag))

(use-package js2-refactor
  :ensure t
  :diminish js2-refactor-mode
  :config
  (progn
    (js2r-add-keybindings-with-prefix "C-c r")
    (add-hook 'js2-mode-hook #'js2-refactor-mode)))

(use-package clojure-mode
  :ensure t
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

(use-package clj-refactor
  :ensure t
  :functions cljr-add-keybindings-with-prefix
  :init
  (progn
    (cljr-add-keybindings-with-prefix "C-c r")
    (add-hook 'clojure-mode-hook (lambda ()
                                   (clj-refactor-mode 1)))))

(use-package cider
  :ensure t)

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package clojure-snippets
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package company-go
  :ensure t
  :init (setenv "GOPATH" (substitute-in-file-name "$HOME/Proejcts/Go"))
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (if (not (string-match "go" compile-command))
                  (set (make-local-variable 'compile-command)
                       "go generate && go build -v && go test -v && go vet"))
              (local-set-key (kbd "M-.") 'godef-jump))))

(use-package smartparens
  :ensure t
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

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

;;; convenience
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode t)
(column-number-mode t)
(line-number-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode +1)

;;; No bells (visible, even!)
(setq visible-bell nil)
(defun my-visible-bell ()
  "Blink mode line instead of weird visual bell behavior."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq ring-bell-function 'my-visible-bell)

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

;;; Code cleanup
(defun code-cleanup ()
  "Clean up current buffer."
  (interactive)
  (whitespace-cleanup)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(define-key prog-mode-map (kbd "C-S-f") 'code-cleanup)

;;; BACKUPS
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

;;; Look & Feel
(setq default-frame-alist
      '((font . "Source Code Pro-15")
        (foreground-color . "#d9d9d9")
        (background-color . "#353535")
        (scroll-bar-background . "#353535")
        (scroll-bar-foreground . "#555555")
        (border-color . "#353535")
        (line-spacing . 3)
        (vertical-scroll-bars . nil)
        (left-fringe . 8)
        (right-fringe . 8)))
(set-face-attribute 'region nil :background "#dffdfd" :foreground "#000000")
(set-face-attribute 'fringe nil :background "#353535")
(setq-default left-margin-width 2)
(setq-default right-margin-width 2)

;;; General
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default insert-directory-program "/usr/local/bin/gls")
(global-auto-revert-mode t)

(provide 'init)
;;; init ends here
