(require 'package)
(add-to-list 'package-archives'("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome") 

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-fullscreen)

(setq-default auto-save-default nil)
(setq make-backup-files nil) 

(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(magit-fetch-arguments (quote ("--prune"))))
(custom-set-faces)
(set-face-attribute 'default' nil :height 130)

(load-theme 'wombat t)

(require 'server)
(server-start)

(load "engineer.el")
