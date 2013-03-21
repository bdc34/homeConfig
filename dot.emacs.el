;; this gets run for GNU emacs
;; snagged from various places like www.dotfile.com
;; bdc 01dec2004

(load-file (expand-file-name ".emacs-site-lisp/emacs-common.el" "~"))
(load-file (expand-file-name ".emacs-site-lisp/mode-config.el" "~"))

(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/local/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/prog-modes/"))
(add-to-list 'load-path (expand-file-name "~/.emacs-site-lisp/elib-1.0"))

(tool-bar-mode -1);;lose tool bar, bleck
(global-font-lock-mode 1);font-lock; everywhere!
(scroll-bar-mode -1);;lose scrollbars, oph.
;; Bracket/brace/parentheses highlighting:
(show-paren-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-prompt-read-only t)
 '(compilation-buffer-name-function (quote sbt-build-buffer-name) t)
 '(compilation-error-regexp-alist (quote (("^\\[error\\] \\([.a-zA-Z0-9/-]+[.]scala\\):\\([0-9]+\\):" 1 2 nil 2 nil))))
 '(compilation-mode-font-lock-keywords (quote (("^\\[error\\] Error running compile:" (0 compilation-error-face)) ("^\\[warn\\][^
]*" (0 compilation-warning-face)) ("^\\(\\[info\\]\\)\\([^
]*\\)" (0 compilation-info-face) (1 compilation-line-face)) ("^\\[success\\][^
]*" (0 compilation-info-face)))) t)
 '(custom-enabled-themes (quote (misterioso)))
 '(dabbrev-case-fold-search nil)
 '(inhibit-startup-screen t)
 '(scala-compile-error-regex (quote ("^\\[error\\] \\([.a-zA-Z0-9/-]+[.]scala\\):\\([0-9]+\\):" 1 2 nil 2 nil)) t)
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(which-function-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
