(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/local/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/elib-1.0"))

;; Perl config
(defalias 'perl-mode 'cperl-mode)
(require 'inf-perl)
;; (setq cperl-indent-level 2)
;; (setq cperl-brace-offset 0)
;; (setq cperl-continued-brace-offset -2)
;; (setq cperl-label-offset -2)
;; (setq cperl-continued-statement-offset 2)

;; java eclipse communication via eclim
;(require 'eclim)
;(global-eclim-mode)

;;Displaying compilation error messages in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.3)
(help-at-pt-set-timer)

;; regular auto-complete initialization
;(require 'auto-complete-config)
;(ac-config-default)
;(ac-set-trigger-key "TAB")
;(setq ac-auto-start nil)

;; add the emacs-eclim source
;(require 'ac-emacs-eclim-source)
;(ac-emacs-eclim-config)

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

   ;;; ocaml
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(add-hook 'css-mode-hook 
      (lambda () 
        (setq c-basic-offset 2) 
        (setq indent-tabs-mode nil)))

;; Scala Mode
;; (let ((path "~/.emacs-site-lisp/scala"))
;;   (setq load-path (cons path load-path))
;;   (load "scala-mode-auto.el"))
;; (defun scala-turnoff-indent-tabs-mode ()
;;   (setq indent-tabs-mode nil))
;; ;; scala mode hooks
;; (add-hook 'scala-mode-hook 'scala-turnoff-indent-tabs-mode)
;; ;; Load the ensime lisp code...
;; (add-to-list 'load-path "~/.emacs-site-lisp/ensime/elisp/")
;; (require 'ensime)
;; ;; This step causes the ensime-mode to be started whenever
;; ;; scala-mode is started for a buffer. You may have to customize this step
;; ;; if you're not using the standard scala mode.
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; ;; for scala sbt
;; ;; this is messing up normal compile so I'm commenting it out
;; ;;(load "~/.emacs-site-lisp/sbt.el")


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

(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/gradle.el"))
(require 'gradle)
