
;; keybindings
;; how to find a key to bind:
;; use M-x global-set-key then hit the key you want
;; then recall the previous command with C-x esc esc and use that
;; similarly use m-x view-lossage or read-kbd-macro
;; C-h k will also describe a key

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
(global-set-key [(control z)] 'undo)

;(global-set-key (kbd "C-c b") 'some-thing)
;(global-set-key (kbd "<f3>") 'some-thing)
;(define-key global-map "\C-xm" 'ignore)
;(global-set-key [delete] 'delete-char)
;(global-set-key [C-home] 'beginning-of-buffer)   
;(global-set-key "^?" 'backwards-delete-char-untabify)

;; mouse bindings
(global-set-key [mouse-9] 'next-buffer)
(global-set-key [mouse-8] 'previous-buffer)

(add-hook 'dired-mode-hook ;do vi movement in dired
          '(lambda ()
            (define-key dired-mode-map "j" 'dired-next-line)
            (define-key dired-mode-map "k" 'dired-previous-line)
            (define-key dired-mode-map "h" 'dired-up-directory)))

;;
;; Miscellaneous settings
;;
(hl-line-mode)
(setq inhibit-startup-message t)
(display-time)

(setq line-number-mode t)
(setq column-number-mode t)
(setq menu-bar-enable-clipboard t)  ;make menu edit do clipboard stuff
(setq disabled-command-hook nil)  ; disable command disabling
(setq visible-bell t);; i hate my computer beeping at me 

;; shell-mode ; why is this not enabled by default?
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

(setq kill-read-only-ok 1)	;;allow cutting of text from read only buffers
;; no tab chars in files, spaces only! important for source control 
(setq-default tab-width 4 indent-tabs-mode nil)
;;Don't try to truncate lines in partial width windws
(setq truncate-partial-width-windows nil) 

;; code folding 
(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key "\M-+" 'jao-toggle-selective-display)

(setq-default save-place t)


;; make it so that C-x k will close a emacsclient
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

(server-start);; start server for emacsclient

;; java hooks
(add-hook 'java-mode-hook 
  (lambda () "set line highlight " (hl-line-mode 1)))
              

