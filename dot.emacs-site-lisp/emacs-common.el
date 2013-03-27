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

;;
;; Miscellaneous settings
;;
(hl-line-mode)

;; no tab chars in files, spaces only! important for source control 
(setq-default tab-width 4 indent-tabs-mode nil)
;;Don't try to truncate lines in partial width windws
(setq truncate-partial-width-windows nil) 

(setq-default save-place t)

(server-start);; start server for emacsclient
              

