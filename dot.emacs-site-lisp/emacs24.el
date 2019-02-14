;; Only run this stuff if we are living a emacs v >= 24

(require 'package)
(setq package-enable-at-startup nil)
;(add-to-list 'package-archives
                                        ;             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(package-initialize)

(eval-when-compile
  (add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/use-package"))
  (require 'use-package))


;; https://camdez.com/blog/2015/04/03/switching-to-melpa-stable/
(setq my/packages '(ag alert beacon circe company-quickhelp diminish
dimmer docker docker-compose-mode docker-tramp dockerfile-mode
doom-modeline doom-themes all-the-icons ein auto-complete eldoc-eval
elpy company emojify eslint-fix find-file-in-project flx-ido
flx  flycheck-pycheckers flymake gntp helm-ag
helm-projectile helm-swoop helm helm-core highlight-indentation
 imenus ivy jinja2-mode json-mode json-reformat
json-snatcher log4e lua-mode magit-gh-pulls magit git-commit gh logito
magit-popup markdown-toc markdown-mode marshal ht memoize multi-term
oauth2 org-jira pcache php-mode popup pos-tip projectile
py-autopep8 pythonic pyvenv request-deferred request
deferred rjsx-mode shrink-path f skewer-mode js2-mode simple-httpd
smartparens sphinx-doc tablist terraform-mode hcl-mode tide flycheck
seq let-alist pkg-info epl s dash typescript-mode
undo-tree use-package bind-key vlf web-beautify web-mode websocket
with-editor async yaml-mode yasnippet zenburn-theme))
;; removed due to not in stable
;;  ewmctrl flycheck-mypy ido-better-flex pyenv-mode pipenv tt-mode ttl-mode

(defun my/install-packages ()
  "Ensure the packages I use are installed. See `my/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))
(my/install-packages)

(use-package zenburn-theme
  :ensure t
  :config (progn
            (load-theme 'zenburn t)
            (set-cursor-color "coral")))

(setq mouse-autoselect-window nil)


;; Helm for many things
(load-file (expand-file-name ".emacs-site-lisp/helm-setup.el" "~"))

;; but ido-mode for find-file
(require 'ido)
(ido-mode t)
(custom-set-variables
 '(ido-enable-flex-matching t)
 '(ido-file-extensions-order (quote (".ts"))) ;; complete to typescript instead of js
 '(ido-mode 'file)
)

(if (and (string= (system-name) "bdc34-laptop") (window-system))
    (load-file (expand-file-name "~/.emacs-site-lisp/helm-taskswitch.el")))

(use-package magit-gh-pulls
  :ensure t)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)


(when window-system
   (load-theme 'zenburn t)
   (set-cursor-color "coral")

   (use-package doom-modeline
     :ensure t
     :hook (after-init . doom-modeline-mode))
   (use-package doom-themes
     :ensure t
     :after (doom-modeline)
     :config (progn
    ;;           (load-theme 'doom-one t)
               (doom-themes-org-config)))
   )



;(require 'spaceline-config)
;(spaceline-spacemacs-theme)

(use-package vlf :ensure t) ;; View Large Files

(use-package json-mode :ensure t)
(use-package json-reformat :ensure t)

(use-package beacon
  :ensure t
  :config (beacon-mode 1))



