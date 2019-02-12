;; Only run this stuff if we are living a emacs v >= 24

(require 'package)
(defvar my-packages
  '(async
    dash
    ein
    elpy
    epl
    ewmctrl
    flx
    flx-ido
    flycheck
    helm
    helm-core
    helm-projectile
    jabber
    json-mode
    json-reformat
    json-snatcher
    magit
    magit-gh-pulls
    pkg-info
    projectile
    py-autopep8
    request
    s
    seq
    tt-mode
    ttl-mode
    web-beautify
    with-editor
    )
  "List of packages that should be installed.")

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

;; install any packages in my-packages, if they are not installed already
(let ((refreshed nil))
  (when (not package-archive-contents)
    (package-refresh-contents)
    (setq refreshed t))
  (dolist (pkg my-packages)
    (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install pkg))))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `my-packages'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                            (not (package-built-in-p x))
                            (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

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

(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(if (and (string= system-name "bdc34-laptop")
         (window-system))
    (load-file (expand-file-name "~/.emacs-site-lisp/helm-taskswitch.el")))

(require 'spaceline-config)
(spaceline-spacemacs-theme)
