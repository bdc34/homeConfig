;; this gets run for GNU emacs
;; snagged from various places like www.dotfile.com
;; bdc 01dec2004

(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/local/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/elib-1.0"))

(tool-bar-mode -1);;lose tool bar, bleck
(scroll-bar-mode -1);;lose scrollbars, oph.

(require 'package)
(setq package-archives
             '(("gnu" . "http://elpa.gnu.org/packages/")
               ("marmalade" . "http://marmalade-repo.org/packages/")
               ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(rainbow-mode
    fill-column-indicator
    clojure-mode
    cursor-chg
    highlight-indentation
    highlight-symbol
    markdown-mode
    protobuf-mode
    emacs-eclim
    auto-complete
    zenburn-theme
    solarized-theme
    magit
    rvm)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

(load-file( expand-file-name ".basic.el" "~"))
(load-file (expand-file-name ".emacs-site-lisp/emacs-common.el" "~"))
(load-file (expand-file-name ".emacs-site-lisp/mode-config.el" "~"))

(load-theme 'zenburn t)
(set-cursor-color "coral")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#fdf6e3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(ansi-term-color-vector ["#586e75" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(dabbrev-case-fold-search nil)
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(eclim-eclipse-dirs (quote ("/opt/eclipse")))
 '(eclim-executable "/opt/eclipse4.2/eclim")
 '(fci-rule-color "#073642")
 '(global-highline-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(which-function-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
