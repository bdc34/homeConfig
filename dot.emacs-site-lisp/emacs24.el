;; Only run this stuff if we are living a emacs 24 


(require 'package)
(setq package-archives
             '(("gnu" . "http://elpa.gnu.org/packages/")
               ("marmalade" . "http://marmalade-repo.org/packages/")
               ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


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

(load-theme 'wheatgrass t)
(set-cursor-color "coral")

