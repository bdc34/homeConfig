
(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
	      ("M-i" . helm-previous-line)
	      ("M-k" . helm-next-line)
	      ("M-I" . helm-previous-page)
	      ("M-K" . helm-next-page)
	      ("M-h" . helm-beginning-of-buffer)
	      ("M-H" . helm-end-of-buffer)
          ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
	      ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
          ("C-z"  . helm-select-action) ; list actions using C-z
          )
  :init (progn
        ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
        ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
        ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
        (global-set-key (kbd "C-c h") 'helm-command-prefix)
        (global-unset-key (kbd "C-x c"))
        )
  :config (progn


        (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	          helm-move-to-line-cycle-in-source     t ; cycle in source list  
	          helm-ff-file-name-history-use-recentf t)
        (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
        (setq helm-buffers-fuzzy-matching t
	          helm-recentf-fuzzy-match    t)
    (helm-mode 1)
        ))


(use-package projectile
  :ensure t
  :bind (("s-p" . projectile-switch-open-project)
	 ("C-c p" . projectile-switch-project))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :ensure t
  :bind ("M-t" . helm-projectile-find-file)
  :config
  (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :bind (("M-m" . helm-swoop)
	     ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

(use-package helm-ag
  :ensure helm-ag
  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
	      helm-ag-command-option "--path-to-ignore ~/.agignore"))



