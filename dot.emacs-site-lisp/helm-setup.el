

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
  ;; :bind (:map helm-map
	;;       ("M-i" . helm-previous-line)
	;;       ("M-k" . helm-next-line)
	;;       ("M-I" . helm-previous-page)
	;;       ("M-K" . helm-next-page)
	;;       ("M-h" . helm-beginning-of-buffer)
	;;       ("M-H" . helm-end-of-buffer)
  ;;       ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
	;;       ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
  ;;       ("C-z"  . helm-select-action) ; list actions using C-z
  ;;       )
  :init 
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))        
  :config 
  (setq helm-echo-input-in-header-line        t ; show input in helm buffer
        helm-split-window-in-side-p           t ; helm in same window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-move-to-line-cycle-in-source     t ; cycle in source list
        helm-ff-file-name-history-use-recentf t))

        ;; ;; from https://www.reddit.com/r/emacs/comments/3o7a9i/using_helm_with_flx_for_better_fuzzy_matching/
        ;; ;; make sure you have flx installed
        ;; (require 'flx)
        ;; ;; this is a bit hackish, ATM, redefining functions I don't own
        ;; (defvar helm-flx-cache (flx-make-string-cache #'flx-get-heatmap-str))
        
        ;; (defun helm-score-candidate-for-pattern (candidate pattern)
        ;;   (or (car (flx-score candidate pattern helm-flx-cache)) 0))

        ;; (defun helm-fuzzy-default-highlight-match (candidate)
        ;;   (let* ((pair (and (consp candidate) candidate))
        ;;          (display (if pair (car pair) candidate))
        ;;          (real (cdr pair)))
        ;;     (with-temp-buffer
        ;;       (insert display)
        ;;       (goto-char (point-min))
        ;;       (if (string-match-p " " helm-pattern)
        ;;           (cl-loop with pattern = (split-string helm-pattern)
        ;;                    for p in pattern
        ;;                    do (when (search-forward p nil t)
        ;;                         (add-text-properties
        ;;                          (match-beginning 0) (match-end 0) '(face helm-match))))
        ;;         (cl-loop with pattern = (cdr (flx-score display
        ;;                                                 helm-pattern helm-flx-cache))
        ;;                  for index in pattern
        ;;                  do (add-text-properties
        ;;                      (1+ index) (+ 2 index) '(face helm-match))))
        ;;       (setq display (buffer-string)))
        ;;     (if real (cons display real) display)))
        
;;        (setq helm-mode-fuzzy-match t)
;;        (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
;;        (setq helm-buffers-fuzzy-matching t)
;;	      (setq helm-recentf-fuzzy-match    t)
;;        ))

;; (use-package recentf
;;   :ensure t
;;   :config
;;   (setq recentf-auto-cleanup 'never) ;; Prevent auto-cleanup during tramp use
;;   (run-at-time nil (* 5 60) 'recentf-save-list) ;; save list every 5 min
;;   (setq recentf-max-menu-items 20)
;;   (setq recentf-max-saved-items 50)
;;   (defun undo-kill-buffer (arg)
;;     "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
;;     (interactive "p")
;;     (let ((recently-killed-list (copy-sequence recentf-list))
;;           (buffer-files-list
;;            (delq nil (mapcar (lambda (buf)
;;                                (when (buffer-file-name buf)
;;                                  (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
;;       (mapc
;;        (lambda (buf-file)
;;          (setq recently-killed-list
;;                (delq buf-file recently-killed-list)))
;;        buffer-files-list)
;;       (let ((killed-file (if arg (nth arg recently-killed-list)
;;                            (car recently-killed-list))))
;;         (if killed-file (find-file killed-file)
;;           (message "All files on recentf list already open")))))
;;   (bind-key "C-S-t" 'undo-kill-buffer)
;;   )


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
;;              helm-ag-command-option "--path-to-ignore ~/.agignore"
              helm-grep-ag-command "ag --line-numbers -S --hidden --color --color-match '31;43' --nogroup %s %s %s"
              helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'")))

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;;
;; Set up keys to replace normal emacs features with helm features
;;

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; Add action of normal find-files
;; (require 'helm-files)
;; (defun old-ff (&optional no-op) (interactive)
;;        (call-interactively 'find-file))

;; (helm-add-action-to-source "Fallback find-file"
;;                        'old-ff
;;                        helm-source-find-files)

;; (define-key helm-map (kbd "C-f")
;;   (lambda () (interactive)
;;     (helm-quit-and-execute-action 'old-ff)))
  
;; set C-c C-l to
;; (require 'helm-eshell)
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;(helm-mode 1)

(setq projectile-enable-caching t)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;; helm-setup.el ends here
