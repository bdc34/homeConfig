(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/local/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/elib-1.0"))

;;(add-hook 'after-init-hook 'global-company-mode)

(use-package flycheck
  :ensure t
  :if (version<= "24.4" emacs-version)
  :commands flycheck-mode
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'tide-mode))

(use-package flycheck-tip
  :ensure t
  :commands 'flycheck-tip-cycle
  :after flycheck
  :bind (:map flycheck-mode-map
              ("C-c C-n" . flycheck-tip-cycle)))

;; somehow flake8 always seems a mess
;;(setq-default flycheck-disabled-checkers '(python-flake8))
(use-package flycheck-pyflakes
  :ensure t
  :after flycheck
  :init
  ;; If you want to use pyflakes you probably don't want pylint or
  ;; flake8. To disable those checkers, add the following to your
  ;; init.el:
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (flycheck-add-next-checker 'python-pyflakes 'python-pycompile))

(use-package flycheck-mypy
  :ensure t
  :after flycheck)

;; Perl config
(defalias 'perl-mode 'cperl-mode)
;;(require 'inf-perl)
(setq cperl-indent-level 4)
;; (setq cperl-brace-offset 0)
;; (setq cperl-continued-brace-offset -2)
;; (setq cperl-label-offset -2)
;; (setq cperl-continued-statement-offset 2)

(defun java-mode-untabify ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
	(untabify (1- (point)) (point-max))))
  nil)

;; java eclipse communication via eclim
;(require 'eclim)
;(global-eclim-mode)

;;(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/gradle.el"))
;;(require 'gradle)

;;Displaying compilation error messages in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.3)
(help-at-pt-set-timer)

;; Turn on font lock when in N3 mode
(add-to-list 'load-path "~/.emacs-site-lisp/prog-modes/n3-mode.el")
(autoload 'n3-mode "n3-mode" "Major mode for OWL or N3 files" t)
(add-hook 'n3-mode-hook
          'turn-on-font-lock)
(setq auto-mode-alist
      (append
       (list
        '("\\.n3" . n3-mode)
        '("\\.owl" . n3-mode))
       auto-mode-alist))

;; (setq load-path (append '("~/.emacs-site-lisp/owl/" "~/.emacs-site-listp/w3/") load-path))
;; (autoload 'owl-mode "owl-mode" "OWL mode." t)
;; (push (cons "\\.owl" 'owl-mode) auto-mode-alist)


;; LUA mode
(add-to-list 'load-path "~/.emacs-site-lisp/immerrr-lua-mode-a070284" )
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; OCaml
;; (load "/home/bdc34/.opam/4.05.0/share/emacs/site-lisp/tuareg-site-file")
;; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;   (when (and opam-share (file-directory-p opam-share))
;;     ;; Register Merlin
;;     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;     (autoload 'merlin-mode "merlin" nil t nil)
;;     ;; Automatically start it in OCaml buffers
;;     (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;     (add-hook 'caml-mode-hook 'merlin-mode t)
;;     ;; Use opam switch to lookup ocamlmerlin binary
;;     (setq merlin-command 'opam)))


;; Tide is for Javascript and Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setqf lycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (company-quickhelp-local-mode)
  (set (make-local-variable 'company-dabbrev-downcase nil))
  (set (make-local-variable 'company-idle-delay .15))
  (set (make-local-variable 'company-minimum-prefix-length 0)))


(use-package web-mode
  :ensure t
  :mode
  (("\\.erb\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.json\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ;; TODO: Fix flycheck in order to use web-mode with .scss files
   ;; ("\\.scss\\'" . web-mode)
   ("\\.less\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.hbs\\'" . web-mode))
  :custom
  ;; Some from https://github.com/fxbois/web-mode/issues/872#issue-219357898
  ;; (web-mode-markup-indent-offset 4)
  ;; (web-mode-css-indent-offset 4)
  ;; (web-mode-code-indent-offset 4)
  ;; (web-mode-script-padding 4)
  ;; (web-mode-attr-indent-offset 4)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-element-highlight t)

  ;; Indent inline JS/CSS within HTML
  ;; https://stackoverflow.com/a/36745155/3516664
  ;; (web-mode-script-padding 4)
  ;; (web-mode-style-padding 4)
  ;; (web-mode-block-padding 4)
  (web-mode-comment-formats
   '(("java"       . "/*")
     ("javascript" . "//")
     ("php"        . "/*")
     ))
  :config
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-quotes" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))
  

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Code analysis for Javascript
(add-to-list 'load-path "/home/bdc34/src/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


(use-package projectile
  :ensure t)

(use-package elpy
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :bind (:map elpy-mode-map
	      ("<M-left>" . nil)
	      ("<M-right>" . nil)
	      ("<M-S-left>" . elpy-nav-indent-shift-left)
	      ("<M-S-right>" . elpy-nav-indent-shift-right)
	      ("M-." . elpy-goto-definition)
	      ("M-," . pop-tag-mark))
    :config
    (setq elpy-rpc-backend "jedi")
    (setq elpy-eldoc-show-current-function nil)
    )

(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4)
  (elpy-enable))


(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :after (elpy)
  )

(use-package pyenv-mode-auto
  :ensure t
  :after (pyenv-mode))

; alternative  pyimport pyimpsort
(use-package importmagic
  :ensure t)

(use-package pycoverage)
;; Above is not working 
;; look at this other coverate pacakge
;; https://github.com/wbolster/emacs-python-coverage/blob/master/python-coverage.el


(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)
;There is also a handy command `pylint-insert-ignore-comment' that
;makes it easy to insert comments of the form `# pylint:
;ignore=msg1,msg2,...'.

(use-package k8s-mode
 :ensure t
 :config
 (setq k8s-search-documentation-browser-function 'browse-url-firefox)
 :hook (k8s-mode . yas-minor-mode))

(use-package rg
  :ensure t)

(use-package dumb-jump  
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-force-searcher 'rg)
  )
