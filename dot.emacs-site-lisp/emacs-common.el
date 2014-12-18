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


;; Miscellaneous settings

;; highlight current line
(hl-line-mode)


;; eval .dir.local files on remote systems
(setq enable-remote-dir-locals t)

;; no tab chars in files, spaces only! important for source control 
;;(setq-default tab-width 4 indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

;;Don't try to truncate lines in partial width windws
(setq truncate-partial-width-windows nil) 

;; This means when you visit a file, point goes to the last place
;; where it was when you previously visited the same file.
(setq-default save-place t)
(require 'saveplace)

;; make it so that C-x k will close a emacsclient
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

(server-start);; start server for emacsclient
              

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
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;;3. use Tramp like this: M-x dired-find-file /sudo::/some/file
;;4. use Tramp like this: M-x dired-find-file /sudo:root@remoteHost.edu:/some/file

;; display host in mode line
(defconst my-mode-line-buffer-identification
  (list
   "emacs "
   '(:eval
     (let ((host-name
            (or (file-remote-p default-directory 'host)
                (system-name))))
       (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
           (substring host-name 0 (match-beginning 1))
          host-name)))
   ": %12b"))

(setq-default
 mode-line-buffer-identification
 my-mode-line-buffer-identification)

(add-hook 'dired-mode-hook
 '(lambda () (setq 
              mode-line-buffer-identification
              my-mode-line-buffer-identification)))

;; show header warning when editing file as root or sudo
(defun my-tramp-header-line-function ()
  (when 
      (or (string-match "^/sudo:root.*$" default-directory)
          (string-match "^/sudo::.*$" default-directory)
          (string-match "^/ssh:root.*$" default-directory) )
    (setq header-line-format
          (propertize "*** The buffer bellow is visited as Root  ***"
              'face '(:background "salmon" 
                      :foreground "black" 
                      :weight "bold" 
                      :box t) ))))

(add-hook 'find-file-hooks 'my-tramp-header-line-function)
(add-hook 'dired-mode-hook 'my-tramp-header-line-function)
