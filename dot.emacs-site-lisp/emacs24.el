;; Only run this stuff if we are living a emacs v >= 24

(eval-when-compile
  (add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/use-package"))
  (require 'use-package))

;; https://camdez.com/blog/2015/04/03/switching-to-melpa-stable/
(setq my/packages '(ag alert beacon circe company-quickhelp diminish
		       dimmer dockerfile-mode
		       doom-modeline doom-themes all-the-icons auto-complete eldoc-eval company emojify eslint-fix find-file-in-project flx-ido
		       flx flycheck-pycheckers gntp helm-ag
		       helm-projectile helm-swoop helm helm-core highlight-indentation
		       imenus ivy jinja2-mode json-mode json-reformat
		       json-snatcher log4e lua-mode  magit git-commit gh logito
		       magit-popup markdown-toc markdown-mode marshal ht memoize multi-term
		       oauth2 org-jira pcache php-mode popup pos-tip projectile
		       py-autopep8 request-deferred request
		       deferred rjsx-mode shrink-path f skewer-mode js2-mode simple-httpd
		       smartparens sphinx-doc tablist terraform-mode hcl-mode tide flycheck
		       seq let-alist pkg-info epl s dash typescript-mode
		       undo-tree use-package bind-key vlf web-beautify web-mode websocket
		       with-editor async yaml-mode yasnippet))
;; removed due to not in stable
;;  ewmctrl flycheck-mypy ido-better-flex pyenv-mode pipenv tt-mode ttl-mode

(defun my/install-packages ()
  "Ensure the packages I use are installed.  See `my/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))
(my/install-packages)

(setq focus_follows_mouse t) ;; if using ffm in the wm
(setq mouse-autoselect-window nil)

;; Helm for many things
;; Especally select buffers
(load-file (expand-file-name ".emacs-site-lisp/helm-setup.el" "~"))

;; I used IDO fo a couple years for find-file and it
;; was fine. I liked some things about it once I got use to it.
;; I liked C-d to open a file, I liked C-f to fall back to nomrmal find-file.
;; But I liked the space for search in ivy better.
;; but ido-mode for find-file
(require 'ido)
(ido-mode t)
(custom-set-variables
 '(ido-enable-flex-matching t)
 '(ido-file-extensions-order (quote (".ts"))) ;; complete to typescript instead of js
 '(ido-mode 'file)
)

;; Tried ivy as a replacement for ido for a couple months
;; I liked the way space worked in searches.
;; I didn't like a lot of the other stuff such as how tabs worked
;; and I didn't take time to get use to it.
;; (use-package ivy
;;   :ensure t
;;   :init
;;   (setq ivy-use-virtual-buffers t)
;; ;  (setq enable-recursive-minibuffers t)
;;   :bind
;;   (;("C-s" . swiper)
;;    ;("M-x") . counsel-M-x)
;;    ("C-c C-r" . ivy-resume)
;;    ("<f6>" . ivy-resume)
;;    ("C-x C-f" . counsel-find-file)
;;    ("C-c g" . counsel-git)
;;    ("C-c j" . counsel-git-grep)
;;    ("C-c k" . counsel-ag)
;;    ("C-x l" . counsel-locate))
;;   :config
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; (use-package ivy-prescient
;;   :ensure t
;;   :after (ivy)
;;   :config
;;   (prescient-persist-mode)
;;   (ivy-prescient-mode))

(load-file (expand-file-name "~/.emacs-site-lisp/org-setup.el"))

;;(use-package magit-gh-pulls
;;  :ensure t)
;;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;;(define-key magit-mode-map (kbd ",") nil)
;;(define-key magit-mode-map (kbd "k") nil)
;;(define-key magit-mode-map (kbd "C-k") 'magit-delete-thing)


(when window-system
   (set-cursor-color "coral")

   (use-package doom-modeline
     :ensure t
     :hook (after-init . doom-modeline-mode))
   (use-package doom-themes
     :ensure t
     :after (doom-modeline)
     :config (progn
    ;;           (load-theme 'doom-one t)
               ;(doom-themes-org-config)
               ;; undo somethings in doom-themes-org-config
               (setq org-hide-leading-stars t
                     org-hide-leading-stars-before-indent-mode t)))
   )

(use-package vlf :ensure t) ;; View Large Files
(global-so-long-mode)

(use-package json-mode :ensure t)
(use-package json-reformat :ensure t)

(use-package beacon
  :ensure t
  :config (beacon-mode 1))

(when (and (string= (system-name) "xr23-Latitude-7390") (window-system))
  (load-file (expand-file-name "~/.emacs-site-lisp/helm-taskswitch.el")))
