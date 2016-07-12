;; from http://tuhdo.github.io/helm-intro.html

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-move-to-line-cycle-in-source     t ; cycle in source list  
      helm-ff-file-name-history-use-recentf t)

;;
;; Set up keys to replace normal emacs features with helm features
;;

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "C-x C-f") 'helm-find-files)

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

(define-key helm-map (kbd "C-f")
  (lambda () (interactive)
    (helm-quit-and-execute-action 'old-ff)))
  
;; set C-c C-l to 
(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

(helm-mode 1)

(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(projectile-global-mode)
(helm-projectile-on)
