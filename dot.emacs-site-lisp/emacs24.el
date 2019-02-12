;; Only run this stuff if we are living a emacs v >= 24

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/use-package"))
  (require 'use-package))


(use-package zenburn-theme
  :ensure t
  :config (progn
            (load-theme 'zenburn t)
            (set-cursor-color "coral")))

(setq mouse-autoselect-window nil)

(when window-system
   (load-theme 'zenburn t)
   (set-cursor-color "coral"))


;; Helm for many things
(load-file (expand-file-name ".emacs-site-lisp/helm-setup.el" "~"))

;; ido-mode for find-file
(require 'ido)
(ido-mode t)
(custom-set-variables
 '(ido-enable-flex-matching t)
 '(ido-file-extensions-order (quote (".ts"))) ;; complete to typescript instead of js
 '(ido-mode 'file)
)

(use-package magit-gh-pulls
  :ensure t)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(if (and (string= system-name "bdc34-laptop")
         (window-system))
    (load-file (expand-file-name "~/.emacs-site-lisp/helm-taskswitch.el")))

(require 'spaceline-config)
(spaceline-spacemacs-theme)
