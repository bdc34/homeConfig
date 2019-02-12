(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/local/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/elib-1.0"))

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Perl config
(defalias 'perl-mode 'cperl-mode)
(require 'inf-perl)
;; (setq cperl-indent-level 2)
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

(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/gradle.el"))
(require 'gradle)

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
(load "/home/bdc34/.opam/4.05.0/share/emacs/site-lisp/tuareg-site-file")
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))


;; Tide is for Javascript and Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setqf lycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (company-quickhelp-local-mode)
  (set (make-local-variable 'company-dabbrev-downcase nil))
  (set (make-local-variable 'company-idle-delay .15))
  (set (make-local-variable 'company-minimum-prefix-length 0)))

(require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
(flycheck-add-mode 'typescript-tslint 'tide-mode)

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


;; Python
(defun elpy-module-company-quickhelp (command &rest _args)
  "Enable company-quickhelp support for Python.
Adds doc for completion to company's popup.
COMMAND is elpy-module command."
  (pcase command
    (`global-init
     (require 'company-quickhelp))
    (`buffer-init
     (company-quickhelp-local-mode)) ))

;;(pyenv-mode)
(elpy-enable)

(when (require 'flycheck nil t)
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;using flycheck instead
(remove-hook 'elpy-modules 'elpy-module-flymake)

;;(add-to-list 'flycheck-disabled-checkers 'python-flake8)
;(add-to-list 'flycheck-disabled-checkers 'python-pylint)

;; enable autopep8 formatting on save
;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

