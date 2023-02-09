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

(global-unset-key (kbd "C-x C-c")) ;; don't like accidental quits
(setq create-lockfiles nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/local/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/X/"))

(if (>= emacs-major-version 24)
    (load-file( expand-file-name "~/.emacs-site-lisp/emacs24.el" "~")))

(when (display-graphic-p)
  (require 'linkd))

(load-file( expand-file-name ".basic.el" "~"))
(load-file (expand-file-name ".emacs-site-lisp/emacs-common.el" "~"))
(load-file (expand-file-name ".emacs-site-lisp/mode-config.el" "~"))
(load-file (expand-file-name ".emacs-site-lisp/prog-modes/emacs-rust-config.el" "~"))

;;(require 'cl)

(global-auto-revert-mode)

(set-face-font 'default "-1ASC-Droid Sans Mono Slashed-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" )
;;(add-hook 'helm-mode-hook (lambda () (setq buffer-face-mode-face '(:family "Droid Sans"))))

;; ;; eval a local file for stuff that is specific for a server or
;; ;; should not go to git
(let
     ( ( local-config-file (expand-file-name "~/.emacs-machine-local.el") ))
   (if (file-exists-p local-config-file)
       (load-file local-config-file )))


(use-package ox-moderncv
    :load-path "~/.emacs-site-lisp/org-cv/"
    :init (require 'ox-moderncv))

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
 '(browse-url-browser-function 'browse-url-chrome)
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("6c4c97a17fc7b6c8127df77252b2d694b74e917bab167e7d3b53c769a6abb6d6" "b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" "4fec44166534b09373e946bdf7c8377615c8d7fc1593dd4a3960754db66ea8b2" "0c32e4f0789f567a560be625f239ee9ec651e524e46a4708eb4aba3b9cdc89c5" "c5207e7b8cc960e08818b95c4b9a0c870d91db3eaf5959dd4eba09098b7f232b" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(elpy-project-ignored-directories
   '(".tox" "build" "dist" ".cask" ".ipynb_checkpoints" ".hypothesis" ".git" ".idea" "__pycache__" ".pytest_cache" ".mypy_cache"))
 '(fci-rule-color "#383838")
 '(font-lock-maximum-decoration '((dired-mode . 1)))
 '(global-highline-mode t)
 '(grep-find-command
   '("find . -type f -exec grep --color -nH -e  \\{\\} + | cut -c -300" . 42))
 '(grep-find-template
   "find <D> <X> -type f <F> -exec grep <C> -nH -e <R> \\{\\} + | cut -c -300")
 '(grep-highlight-matches t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(ido-enable-flex-matching t)
 '(ido-file-extensions-order '(".ts"))
 '(ido-mode 'file nil (ido))
 '(json-reformat:indent-width 2)
 '(magit-diff-use-overlays nil)
 '(markdown-command "/usr/local/bin/kramdown --input GFM")
 '(org-agenda-files
   '("~/Dropbox/work/journal/bdc34workjournal.org" "~/Dropbox/work/journal/notes2013.org" "~/Dropbox/work/journal/notes2014.org" "~/Dropbox/work/journal/notes2015.org" "~/Dropbox/work/journal/notes2016.org" "~/Dropbox/work/journal/notes2017.org" "~/Dropbox/work/journal/notes2018.org" "~/Dropbox/personalNotes/2018-vetschooljob.org" "~/Dropbox/personalNotes/BrooklynArtists.org" "~/Dropbox/personalNotes/TomasSchuman.org" "~/Dropbox/personalNotes/UsingAMacNotes.org" "~/Dropbox/personalNotes/UsingLinuxNotes.org" "~/Dropbox/personalNotes/coverLetter2019-07CALSDevOps.org" "~/Dropbox/personalNotes/coverLetter2019-07Roper.org" "~/Dropbox/personalNotes/journal.org" "~/Dropbox/personalNotes/music.org" "~/Dropbox/personalNotes/resume_2018.vetschoo.org" "~/Dropbox/personalNotes/resume_2019-07Roper.org" "~/Dropbox/personalNotes/resume_2019.org" "~/Dropbox/personalNotes/resume_2019_07CALSOps.org" "~/Dropbox/personalNotes/songs.org" "~/Dropbox/personalNotes/wantsAndNeedsFromOs.org" "~/Dropbox/personalNotes/woodworking.org" "~/Dropbox/712/712-cayuga-rental-todo.org" "~/Dropbox/sailing/wayfarerLaunchList.org"))
 '(package-check-signature nil)
 '(package-selected-packages
   '(deadgrep epl let-alist pyenv-mode pythonic s csv-mode numpydoc vscode-dark-plus-theme flymake eglot tree-sitter tree-sitter-indent tree-sitter-ispell tree-sitter-langs helm-chrome-history multiple-cursors rustic lsp-pyright lsp-julia ws-butler selectrum selectrum-prescient org-plus-contrib lsp-mode lsp-ui flycheck-tip flycheck-pyflakes pycoverage json-navigator pyenv-mode-auto pylint vterm importmagic which-key iedit flycheck-yamllint rg helm-rg counsel-tramp flycheck-package package-lint helm-taskswitch beacon-mode php-mode flycheck-mypy dimmer pipenv eslint-fix magit flx ivy find-file-in-project helm web-mode company zenburn-theme ttl-mode tt-mode smartparens oauth2 ido-better-flex ewmctrl alert))
 '(safe-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (>eval auto-fill-mode 1)
     (typescript-indent-level . 2)))
 '(syslog-debug-face
   '((t :background unspecified :foreground "#2aa198" :weight bold)))
 '(syslog-error-face
   '((t :background unspecified :foreground "#dc322f" :weight bold)))
 '(syslog-hour-face '((t :background unspecified :foreground "#859900")))
 '(syslog-info-face
   '((t :background unspecified :foreground "#268bd2" :weight bold)))
 '(syslog-ip-face '((t :background unspecified :foreground "#b58900")))
 '(syslog-su-face '((t :background unspecified :foreground "#d33682")))
 '(syslog-warn-face
   '((t :background unspecified :foreground "#cb4b16" :weight bold)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
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
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks t)
 '(web-mode-comment-formats '(("java" . "/*") ("javascript" . "//") ("php" . "/*")))
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-current-element-highlight t)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(which-function-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(smerge-refined-added ((t (:inherit diff-changed :background "#22aa22")))))
(put 'downcase-region 'disabled nil)
