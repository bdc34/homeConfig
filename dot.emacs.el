;; this gets run for GNU emacs
;; snagged from various places like www.dotfile.com
;; bdc 01dec2004

(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/local/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/elib-1.0"))

(tool-bar-mode -1);;lose tool bar, bleck
(scroll-bar-mode -1);;lose scrollbars, oph.

;;delete region on del or bksp, set before icicles
(delete-selection-mode 1)

(require 'package)
(setq package-archives
             '(("gnu" . "http://elpa.gnu.org/packages/")
               ("marmalade" . "http://marmalade-repo.org/packages/")
               ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'linkd)
(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(ecb
    cedet
    ruby-mode
    rinari    
    rainbow-mode
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
    rvm
    icicles
    fuzzy-match
    hexrgb
    evil
    window-number
    helm
    ace-jump-mode
    )
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
;; per sys needs to come before mode becuase it sets the location of eclimd
(load-file (expand-file-name ".emacs-site-lisp/per-system-config.el" "~"))
(load-file (expand-file-name ".emacs-site-lisp/mode-config.el" "~"))

(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/gradle.el"))
(require 'gradle)

(load-theme 'wheatgrass t)
(set-cursor-color "coral")

(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(set-face-attribute 'default nil :font 
                    (font-candidate 
                     "Droid Sans Mono Slashed-11"
                     '"Consolas-11:weight=normal" 
                      "DejaVu Sans Mono-11:weight=normal"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#fdf6e3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(ansi-term-color-vector ["#586e75" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"])
 '(custom-safe-themes (quote ("f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(dabbrev-case-fold-search nil)
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
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
