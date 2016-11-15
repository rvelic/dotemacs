;;; GENERAL

(require 'package)

;;; Fetch packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Install required packages
(setq package-list '(magit helm helm-projectile helm-ls-git yaml-mode powershell wgrep markdown-preview-mode js2-mode helm-descbinds multiple-cursors cider paredit))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; General configuration
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default auto-save-default nil)
(setq make-backup-files nil)

(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(magit-fetch-arguments (quote ("--prune"))))

(set-frame-font "Inconsolata-18")
(load-theme 'deeper-blue t)

(require 'server)
(server-start)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; DEVELOPMENT

;;; Files
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq c-basic-offset 2)
(electric-pair-mode 1)

(require 'wgrep)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(setq js2-basic-offset 2)
(setq js-indent-level 2)

;;; Adapt sql mode to mssql
(add-hook 'sql-mode-hook
  (lambda () (sql-set-product "ms")))

;;; Magit
(require 'magit)
(global-set-key (kbd "<f12>") 'magit-status)

;;; Completion
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-d") 'helm-browse-project)
(define-key global-map (kbd "C-x C-b") 'helm-buffers-list)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(setq helm-split-window-in-side-p t)

(require 'helm-descbinds)
(helm-descbinds-mode)

;;; Projectile
(require 'helm-projectile)
(projectile-global-mode)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-ls-git-ls)

;;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)

;;; Ignore '/'

;; On unix based systems, / is the root path. So all string starts with / is recognized as a path. This is annoying especially when editing HTML or XML. The following advice ignore / as a wrong result.
(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)
;; (ad-deactivate 'ffap-file-at-point)

;;; YASnippets
;;; (require 'yasnippet)
;;; (yas-global-mode t)
