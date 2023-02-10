;;; init.el --- Brian's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023 Brian Caruso

;; Author: Brian Caruso <briancaruso@gmail.com>
;; Keywords: internal

;; Commentary:
;; An Emacs configuration. IDO is used for find-file and
;; helm is used for M-x and buffer.

(progn ; Basic configurations that could be used in any 24+ emacs
  (setq create-lockfiles nil)
  (show-paren-mode 1);; bracket/brace/parentheses
  (tool-bar-mode -1);;lose tool bar, bleck
  (scroll-bar-mode -1);;lose scrollbars, oph.

  (setq inhibit-startup-message t)
  (setq transient-mark-mode t)
  (setq delete-selection-mode t)
  (display-time)
  (setq column-number-mode t
        mode-line-in-non-selected-windows t)
  (setq ring-bell-function 'ignore)
  (setq use-short-answers t)
  (xterm-mouse-mode 1)
  (setq confirm-kill-emacs 'yes-or-no-p)

  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  (setq find-file-visit-truename t)
  (setq vc-follow-symlinks t)   ;; Automatically visit symlink

  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default fill-column 80)

  (add-hook 'dired-mode-hook ;do vi movement in dired
            #'(lambda ()
                (define-key dired-mode-map "j" 'dired-next-line)
                (define-key dired-mode-map "k" 'dired-previous-line)
                (define-key dired-mode-map "h" 'dired-up-directory)))

  (setq compilation-scroll-output t)
  (add-hook 'term-mode-hook 'compilation-shell-minor-mode)
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt)
  (with-eval-after-load "term"
    (defun term-send-Cright () (interactive) (term-send-raw-string "\e[1;5C"))
    (defun term-send-Cleft  () (interactive) (term-send-raw-string "\e[1;5D"))
    (define-key term-raw-map (kbd "C-<right>")      'term-send-Cright)
    (define-key term-raw-map (kbd "C-<left>")       'term-send-Cleft))

  (setq kill-read-only-ok 1)	;;allow cutting of text from read only buffers

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror))

  (save-place-mode 1) ;point goes to same place when file revisited
  (global-auto-revert-mode) ;; Automatically update buffers when contents change on disk

  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  (if (display-graphic-p)
      (global-unset-key [(control z)])
      (add-to-list 'initial-frame-alist '(fullscreen . maximized))
      (add-to-list 'default-frame-alist '(fullscreen . maximized)))

  (if (< emacs-major-version 29) ;;use smooth scroll in >29
      (setq-default scroll-margin 1 ;; set scrolling to be a bit less frantic
                    scroll-conservatively 0
                    scroll-up-aggressively 0.40
                    scroll-down-aggressively 0.25))

  (server-start)

  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (set-face-font 'default "-1ASC-Droid Sans Mono Slashed-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" )

  (setq backup-directory-alist
        '(("." . "~/.emacs.d/backups/")))

  (put 'narrow-to-region 'disabled nil)
  (ido-mode) ;; I just want ido for find-file, I'd like better suggestions but it's fine for now
  (setq ido-enable-flex-matching t)
  )

;;
;; * Tramp, SSH and SUDO *
;;
;; I need to to be able to do the following:
;; 1. edit a local file as root via sudo
;; 2. edit a remote file as root via sudo
;; 3. edit a local dir as root with sudo with dired
;; 4. edit a remote dir as root with sudo with dired

;;1: use Tramp like this: C-xC-f /sudo::/some/file
;;2. use Tramp with proxy C-xC-f /sudo:root@remoteHost.edu:/some/file
;(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;;3. use Tramp like this: M-x dired-find-file /sudo::/some/file
;;4. use Tramp like this: M-x dired-find-file /sudo:root@remoteHost.edu:/some/file

;; ;; display host in mode line
;; (defconst my-mode-line-buffer-identification
;;   (list
;;    "emacs "
;;    '(:eval
;;      (let ((host-name
;;             (or (file-remote-p default-directory 'host)
;;                 (system-name))))
;;        (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
;;            (substring host-name 0 (match-beginning 1))
;;           host-name)))
;;    ": %12b"))

;; (setq-default
;;  mode-line-buffer-identification
;;  my-mode-line-buffer-identification)

;; (add-hook 'dired-mode-hook
;;  '(lambda () (setq
;;               mode-line-buffer-identification
;;               my-mode-line-buffer-identification)))

;; ;; show header warning when editing file as root or sudo
;;  (defun my-tramp-header-line-function ()
;;    (when
;;        (or (string-match "^/sudo:root.*$" default-directory)
;;            (string-match "^/sudo::.*$" default-directory)
;;            (string-match "^/ssh:root.*$" default-directory) )
;;      (setq header-line-format
;;            (propertize "*** The buffer bellow is visited as Root  ***"
;;                'face '(:background "salmon"
;;                        :foreground "black"
;;                        :weight "bold"
;;                        :box t) ))))

;; (add-hook 'find-file-hooks 'my-tramp-header-line-function)
;; (add-hook 'dired-mode-hook 'my-tramp-header-line-function)

;; ;; Don't backup tramp files. see tramp INFO 4.18
;; (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

;; ;; Don't make tramp history files everywhere
;; (setq tramp-persistency-file-name "/tmp/tramp_connection_history")

;; ;;Get rid of the .tamp_history files
;; (setq tramp-histfile-override t )

;; new:
;; (use-package tramp
;;   :defer t
;;   :config
;;   (setq vc-handled-backends '(Git)
;;         file-name-inhibit-locks t
;;         tramp-inline-compress-start-size 1000
;;         tramp-copy-size-limit 10000
;;         tramp-verbose 1)
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
;; (setq tramp-use-ssh-controlmaster-options nil)


(when (>= emacs-major-version 24)
  ; an alternative to company https://github.com/minad/corfu
  (use-package company
    :ensure t
    :init
    (set 'company-dabbrev-downcase nil)
    (set 'company-idle-delay .3)
    (set 'company-minimum-prefix-length 0)
    (setq company-tooltip-align-annotations t))

  (use-package ws-butler
    :ensure t
    :init
    (add-hook 'prog-mode-hook #'ws-butler-mode))

  (use-package recentf
    :ensure t
    :config
    (setq recentf-auto-cleanup 'never) ;; Prevent auto-cleanup during tramp use
    (run-at-time nil (* 5 60) 'recentf-save-list) ;; save list every 5 min
    (setq recentf-max-menu-items 20)
    (setq recentf-max-saved-items 50)
    )

  (use-package rg
    :ensure t)

  (when (executable-find "rg")
    (setq grep-program "rg"))

  ;; org mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package org
    :init
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (append org-babel-load-languages
                                         '((python     . t))))
    (setq org-babel-python-command "python3")
    (setq org-agenda-files (list "~/Dropbox/work/journal/bdc34workjournal.org"
                            "~/Dropbox/personalNotes/"
                            "~/Dropbox/1008NCayuga/"
                            "~/Dropbox/712/"
                            "~/Dropbox/sailing/"))
    (setq org-capture-templates
     '(("j" "WorkJournal" entry (file+olp "~/Dropbox/work/journal/bdc34workjournal.org" "2023" )
        "* %T %?\n%i\n  %a" :tree-type month)
       ("m" "Modui3Journal" entry (file "~/Dropbox/work/journal/modui3.org"  )
        "* %T %?\n%i\n  %a")
       ("p" "PersonalJournal" entry (file+olp "~/Dropbox/personalNotes/journal.org" "2023")
        "* %T %?\n" :tree-type month)
       ("s" "Search" entry (file "~/Dropbox/personalNotes/2022_job_search.org")
        "* %T %?\n" )
       ))
    :bind
    ("C-c l" . org-store-link)
    ("C-c a" . org-agenda)
    ("C-c c" . org-capture)
    :hook
    (org-mode . (lambda () (electric-indent-local-mode -1)))
    )
  ;; end org mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; helm mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (use-package helm
    :ensure t
    :bind (("M-x" . helm-M-x)
           ("M-y" . helm-show-kill-ring)
           ("C-x b" . helm-buffers-list))
    :bind (:map helm-map
                ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
	            ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
                ("C-z"  . helm-select-action) ; list actions using C-z
                )
    :init 
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    :config 
    (setq helm-echo-input-in-header-line        t ; show input in helm buffer
          helm-split-window-in-side-p           t ; helm in same window
          ;;        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          ;;      helm-move-to-line-cycle-in-source     t ; cycle in source list
          helm-ff-file-name-history-use-recentf t
          helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match    t
          ))

  (use-package projectile
    :ensure t
    :config
    (projectile-global-mode)
    (helm-projectile-on)
    (setq projectile-enable-caching t
          projectile-completion-system 'helm))

  (use-package helm-projectile
    :ensure t
    :bind ("M-t" . helm-projectile-find-file)
    :config
    (helm-projectile-on))

  (use-package helm-swoop
    :bind (("M-m" . helm-swoop)
	       ("M-M" . helm-swoop-back-to-last-point))
    :init
    (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

  (load-file (expand-file-name "~/src/helm-taskswitch/helm-taskswitch.el"))
  ;; helm mode setup ends here
  )

(when (>= emacs-major-version 27)
  (setq switch-to-buffer-obey-display-actions t))
)

(when (>= emacs-major-version 29)
  (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 32.0)

  (use-package tree-sitter :ensure t
    :init (global-tree-sitter-mode))
  (use-package tree-sitter-langs :ensure t)
  (use-package vterm
    :init
    (defun my-vterm-copy-mode-on () (interactive) (vterm-copy-mode 1))
    (defun my-vterm-copy-mode-off () (interactive) (vterm-copy-mode 0))
    ;; Jump to errors in vterm
    (add-hook 'vterm-mode-hook 'compilation-shell-minor-mode)
    :bind
    (:map vterm-mode-map ("C-c C-j" . my-vterm-copy-mode-on)
     :map vterm-copy-mode-map ("C-c C-k" . my-vterm-copy-mode-off))
  )
  (use-package eglot)
)

(use-package python
  :config
  (setq python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode))

(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)

(use-package blacken
  :bind ("C-c p" . blacken-mode)
  :after (python))

(use-package tramp-venv
  :bind
  (("C-c t v a" . tramp-venv-activate)
   ("C-c t v d" . tramp-venv-deactivate)))

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

(use-package poly-markdown
  :after (markdown-mode)
  :mode ("\\.md" . poly-markdown-mode)
  :config
  (setq markdown-command "markdown_py"))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package restart-emacs
  :bind ("C-c x r" . restart-emacs))

(use-package eglot
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f f" . eglot-format)
         ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown)))

(when (boundp 'treesit-extra-load-path)
  (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
  (add-to-list 'treesit-extra-load-path "~/.local/lib/"))

(use-package treesit-auto
  :demand t
  :config
  (add-to-list 'treesit-auto-fallback-alist '(bash-ts-mode . sh-mode))
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(setq-default treesit-font-lock-level 3)

(provide 'init.el)
;;; init.el ends here
