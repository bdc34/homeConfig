;; this gets run for GNU emacs
;; snagged from various places like www.dotfile.com
;; bdc 01dec2004

(load-file (expand-file-name ".emacs-common" "~"))


;; Turn on font lock when in n3 mode
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

(add-to-list 'load-path "~/.emacs-site-lisp/immerrr-lua-mode-a070284" )
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

   ;;don't run /usr/shared/emacs/site-list/default.el 
   ;;(setq inhibit-default-init t)

   (add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/"))
   (add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/local/"))
   (add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/"))
   (add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/elib-1.0"))

   (tool-bar-mode -1);;lose tool bar, bleck
   (global-font-lock-mode 1)            ;font-lock; everywhere!
   (scroll-bar-mode -1);;lose scrollbars, oph.
   
   ;;; ocaml
   (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
   (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
   (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
   
   ;;; html and css editing
   (autoload 'css-mode "css-mode")
   (setq auto-mode-alist
         (cons '("\\.css\\'" . css-mode) auto-mode-alist))

   (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
   (setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))

  
   ;;nXML mode
   (add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/nxml"))
   (load-file "~/.emacs-site-lisp/nxml/rng-auto.el")
   (add-to-list 'auto-mode-alist
                (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xsl" "xslt" "svg" "rss") t) "\\'")
                      'nxml-mode))
   ;;end nXML
   ;; Bracket/brace/parentheses highlighting:
   ;; The following is the command for Emacs 20.1 and later:
   (show-paren-mode 1)
   ))
;; ;;;;;;;;;;;;;;;;; end of stuff xemacs hates ;;;;;;;;;;;;;;;;;;;;
     

;; I want to hit M-p and have the previous command run no matter
;; where I am in that shell buffer.
;;BROKEN!!!
;; (add-hook 'shell-mode-hook
;;    '(lambda()
;;      (define-key shell-mode-map "\M-p"
;;        '(lambda() (interactive)
;;          (goto-char (point-max)) (comint-previous-input 0)))))


;window (x and win32) stuff
   (cond ( (not (null window-system))
	   (setq x-select-enable-clipboard 1) ;;use x clipboard?
	   ;; resize straight away
;	(setq initial-frame-alist '((top . 0)(left . 0) 
;				    (width . 75) (height . 40)))
	   ))


;;; Emacs/W3 Configuration
(setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
(condition-case () (require 'w3-auto "w3-auto") (error nil))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(jde-ant-working-directory "./")
 '(jde-sourcepath (quote ("./core/src" "./webapp/src" "./ingestTool/src" "./webservices/src"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; Scala Mode

(let ((path "~/.emacs-site-lisp/scala"))
  (setq load-path (cons path load-path))
  (load "scala-mode-auto.el"))

(defun scala-turnoff-indent-tabs-mode ()
  (setq indent-tabs-mode nil))

;; scala mode hooks
(add-hook 'scala-mode-hook 'scala-turnoff-indent-tabs-mode)

;; Load the ensime lisp code...
(add-to-list 'load-path "~/.emacs-site-lisp/ensime/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; MINI HOWTO: 
;; Open .scala file. M-x ensime (once per project)

;; Ctags used by scala mode
;;semantic-ectag-util.el

(defcustom semantic-ectag-program "~/.emacs-site-lisp/ectags-5.8" 
  "The Exuberent CTags program to use."
  :group 'semantic
  :type 'program)

;;cedet
;(load "/usr/share/emacs/site-lisp/cedet-common/cedet.el")
;(load "~/.emacs-site-lisp/scala/cedet-scala.el")
;;(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;;(semantic-load-enable-primary-exuberent-ctags-support)

;; ECB
;(add-to-list 'load-path
;             "/usr/share/emacs/site-lisp/ecb")
;(require 'ecb)

;; for scala sbt
(load "~/.emacs-site-lisp/sbt.el")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(dabbrev-case-fold-search nil)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(which-function-mode nil))
