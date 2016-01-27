;;; init -- personal config
;;; Commentary:
;;; Personal config
;;; LOAD PATH
(add-to-list 'load-path "~/.emacs.d/custom/smartparens-js/")
(let ((default-directory (concat user-emacs-directory
                                 (convert-standard-filename "custom/"))))
  (normal-top-level-add-subdirs-to-load-path))

;;; PACKAGES
(require 'package)

;;; CODE:
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(defvar package-list)
(setq-default package-list
              '(flycheck smartparens js2-mode js2-refactor js-doc web-mode
                         expand-region clojure-mode exec-path-from-shell
                         projectile undo-tree company cider
                         clojure-mode-extra-font-locking clj-refactor
                         magit diminish solarized-theme zenburn-theme
                         smooth-scrolling less-css-mode material-theme
                         anzu expand-region clojure-snippets
                         diff-hl helm helm-flycheck helm-projectile
                         helm-descbinds go-mode company-go company-tern))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; convenience
(require 'midnight) ; clean up obsolete buffers
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode t)
(column-number-mode t)
(line-number-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(font . "Source Code Pro-13"))

;;; No bells (visible, even!)
(setq visible-bell nil)
(defun my-visible-bell ()
  "Blink mode line instead of weird visual bell behavior."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq ring-bell-function 'my-visible-bell)

;;; File navigation
(require 'ffap)
(global-set-key (kbd "C-c f g") 'find-file-at-point)

;;; Search & Replace
(require 'anzu)
(global-anzu-mode)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;;; Diffing
(require 'ediff)
(setq ediff-window-setup-function  'ediff-setup-windows-plain)

;;; Git changes
(require 'diff-hl)
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;;; Scrolling
(require 'smooth-scrolling)
(setq smooth-scroll-margin 1)

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


;;; THEME
(load-theme 'material t)

;;; Auto Complete
(require 'company)
(setq company-idle-delay 0)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)
(global-company-mode 1)


;;; UNDO
(require 'undo-tree)
(global-undo-tree-mode t)
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

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


;;; Helm
(require 'helm)
(require 'helm-config)
(require 'helm-flycheck)
(require 'helm-descbinds)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-c h x") 'helm-M-x)
(global-set-key (kbd "C-c h e") 'helm-flycheck)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(helm-mode 1)

;;; Fix PATH
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;; General
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default company-dabbrev-downcase nil)
(setq-default insert-directory-program "/usr/local/bin/gls")
(global-auto-revert-mode t)

;;; Region expansion
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;;; Whitespace
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-line-column 120)
(setq whitespace-style '(spaces face tabs empty trailing lines-tail))
(defun cleanup-ws ()
  "Cleans up any whitespace in the file before save."
  (whitespace-cleanup))
(add-hook 'before-save-hook 'cleanup-ws nil t)

;;; GIT
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)

;;; Projects
(require 'projectile)
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(projectile-global-mode)

;;; Snippets
(yas-global-mode 1)

;;; Web
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq-default web-mode-markup-indent-offset 4)
(setq-default web-mode-css-indent-offset 4)
(setq-default web-mode-code-indent-offset 4)
(setq-default web-mode-indent-style 4)
(setq-default web-mode-enable-auto-pairing nil)

;;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(javascript-jscs javascript-jshint))

;;; JS
(require 'js2-mode)
(require 'js2-refactor)
(require 'js-doc)
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-indent-switch-body t)
(setq-default js2-global-externs '("module" "require" "sinon" "assert"
                                   "setTimeout" "clearTimeout" "setInterval"
                                   "clearInterval" "console" "JSON"
                                   "describe" "it" "before" "beforeEach"
                                   "after" "afterEach"))
(with-eval-after-load 'flycheck
  (setq flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint javascript-jscs))))
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (tern-mode 1)
            ;;;(set (make-local-variable 'company-backends) '((company-tern company-yasnippet company-dabbrev-code)))
            (setq-default js2-bounce-indent t)
            (js2r-add-keybindings-with-prefix "C-c r")
            (define-key js2-mode-map (kbd "C-c * f") 'js-doc-insert-function-doc)
            (define-key js2-mode-map (kbd "C-c * t") 'js-doc-insert-tag)
            (flycheck-select-checker 'javascript-eslint)))

;;; Clojure
(add-hook 'clojure-mode
          (lambda ()
            (clj-refactor-mode 1)
            (yas-minor-mode 1)
            (cljr-add-keybinding-with-prefix "C-c r")
            (define-clojure-indent
              (defroutes 'defun)
              (GET 2)
              (POST 2)
              (PUT 2)
              (DELETE 2)
              (HEAD 2)
              (ANY 2)
              (context 2))))

;;; Go-lang
(require 'company)
(require 'company-go)
(setenv "GOPATH" (substitute-in-file-name "$HOME/Projects/Go"))
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '((company-go company-dabbrev-code company-yasnippet)))
            (add-hook 'before-save-hook 'gofmt-before-save)
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     "go generate && go build -v && go test -v && go vet"))
            (local-set-key (kbd "M-.") 'godef-jump)))

;;; Code cleanup
(defun code-cleanup ()
  "Clean up current buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(define-key prog-mode-map (kbd "C-S-f") 'code-cleanup)

;;; PARENS
(require 'smartparens)
(require 'smartparens-config)
(require 'smartparens-js)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(smartparens-strict-mode +1)
(smartparens-global-strict-mode +1)
(show-smartparens-global-mode +1)
(sp-use-paredit-bindings)
(turn-on-smartparens-strict-mode)

;;; Modeline
(when (require 'diminish)
  (diminish 'undo-tree-mode))

(provide 'init)
;;; init ends here
