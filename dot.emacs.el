;;; init --- Brian's emacs config
;;
;;; Commentary: My emacs config from various sources.

; Added by Package.el.  This must come before configurations of installed packages.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/local/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/X/"))

;;(require 'bind-key)

(if (>= emacs-major-version 24)
    (load-file( expand-file-name "~/.emacs-site-lisp/emacs24.el" "~")))

(when (display-graphic-p)
  (require 'linkd))

(load-file( expand-file-name ".basic.el" "~"))
(load-file (expand-file-name ".emacs-site-lisp/emacs-common.el" "~"))
(load-file (expand-file-name ".emacs-site-lisp/mode-config.el" "~"))

(require 'cl)

(global-auto-revert-mode)

(set-face-font 'default "-1ASC-Droid Sans Mono Slashed-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" )
;;(add-hook 'helm-mode-hook (lambda () (setq buffer-face-mode-face '(:family "Droid Sans"))))

;; ;; eval a local file for stuff that is specific for a server or
;; ;; should not go to git
 (let
     ( ( local-config-file (expand-file-name "~/.emacs-machine-local.el") ))
   (if (file-exists-p local-config-file)
       (load-file local-config-file )))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(beacon-color 0.6)
 '(browse-url-browser-function (quote browse-url-chrome))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("4fec44166534b09373e946bdf7c8377615c8d7fc1593dd4a3960754db66ea8b2" "0c32e4f0789f567a560be625f239ee9ec651e524e46a4708eb4aba3b9cdc89c5" "c5207e7b8cc960e08818b95c4b9a0c870d91db3eaf5959dd4eba09098b7f232b" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(elpy-project-ignored-directories
   (quote
    (".tox" "build" "dist" ".cask" ".ipynb_checkpoints" ".hypothesis" ".git" ".idea" "__pycache__" ".pytest_cache" ".mypy_cache")))
 '(fci-rule-color "#383838")
 '(font-lock-maximum-decoration (quote ((dired-mode . 1))))
 '(global-highline-mode t)
 '(grep-find-command
   (quote
    ("find . -type f -exec grep --color -nH -e  \\{\\} + | cut -c -300" . 42)))
 '(grep-find-template
   "find <D> <X> -type f <F> -exec grep <C> -nH -e <R> \\{\\} + | cut -c -300")
 '(grep-highlight-matches t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(ido-enable-flex-matching t)
 '(ido-file-extensions-order (quote (".ts")))
 '(ido-mode (quote file) nil (ido))
 '(json-reformat:indent-width 2)
 '(magit-diff-use-overlays nil)
 '(markdown-command "/usr/local/bin/kramdown --input GFM")
 '(org-agenda-files (quote ("~/Dropbox/notes arxiv.org")))
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (ivy-hydra helm-taskswitch beacon beacon-mode vlf php-mode sphinx-doc flycheck-mypy flycheck-pycheckers jinja2-mode company-quickhelp dimmer pipenv org-jira eslint-fix magit yasnippet yaml-mode async dash bind-key diminish typescript-mode s epl pkg-info seq flycheck hcl-mode tablist simple-httpd skewer-mode deferred request-deferred pythonic popup markdown-mode log4e json-snatcher json-reformat highlight-indentation gntp flx auto-complete pyvenv ivy ht helm-core find-file-in-project f pyenv-mode projectile helm ein py-autopep8 docker-compose-mode rjsx-mode web-mode js2-mode terraform-mode elpy docker docker-tramp dockerfile-mode undo-tree company tide zenburn-theme websocket web-beautify use-package ttl-mode tt-mode smartparens request oauth2 multi-term markdown-toc lua-mode json-mode imenus ido-better-flex flymake flx-ido ewmctrl emojify circe alert)))
 '(safe-local-variable-values (quote ((typescript-indent-level . 2))))
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks t)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(which-function-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(smerge-refined-added ((t (:inherit diff-changed :background "#22aa22")))))

