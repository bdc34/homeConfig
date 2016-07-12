;; Only run this stuff if we are living a emacs 24 

(require 'package)
(setq package-archives
             '(("gnu" . "http://elpa.gnu.org/packages/")
               ("marmalade" . "http://marmalade-repo.org/packages/")
               ("melpa-stable" . "https://stable.melpa.org/packages/")
;;               ("melpa" . "http://melpa.org/packages/")
               ))

(package-initialize)

(defvar my-packages-list
  '(async
    dash
    epl
    ewmctrl
    flx
    flx-ido
    flymake
    git-commit
    helm
    helm-core
    helm-projectile
    jabber
    json-mode
    json-reformat
    json-snatcher
    magit
    magit-popup
    pkg-info
    popup
    projectile
    request
    s
    seq
    tt-mode
    ttl-mode
    web-beautify
    with-editor
    zenburn
    zenburn-theme
    )
  "List of packages should be installed")

(load-theme 'zenburn t)
(set-cursor-color "coral")

(load-file (expand-file-name ".emacs-site-lisp/helm-setup.el" "~"))

(if (string= system-name "bdc34-laptop")
    (load-file (expand-file-name "~/.emacs-site-lisp/helm-taskswitch.el")))

