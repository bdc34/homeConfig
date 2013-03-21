;; emacs commands that can be run in GNU emacs or xemacs
(add-hook 'dired-mode-hook ;do vi movement in dired
          '(lambda ()
            (define-key dired-mode-map "j" 'dired-next-line)
            (define-key dired-mode-map "k" 'dired-previous-line)
            (define-key dired-mode-map "h" 'dired-up-directory)))


;; keybindings
;; how to find a key to bind:
;; use M-x global-set-key then hit the key you want
;; then recall the previous command with C-x esc esc and use that
;; similarly use m-x view-lossage or read-kbd-macro
;; C-h k will also describe a key

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
;; Windows Style Undo
(global-set-key [(control z)] 'undo)

;(global-set-key (kbd "C-c b") 'some-thing)
;(global-set-key (kbd "<f3>") 'some-thing)
;(define-key global-map "\C-xm" 'ignore)
;(global-set-key [delete] 'delete-char)
;(global-set-key [C-home] 'beginning-of-buffer)   
;(global-set-key "^?" 'backwards-delete-char-untabify)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-v" 'yank)

;; mouse bindings
(global-set-key [mouse-9] 'next-buffer)
(global-set-key [mouse-8] 'previous-buffer)

;; Miscellaneous settings
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
;;Don't try to truncate lines in partial width windws, if I wanted that I'd set it.
(setq truncate-partial-width-windows nil) 

;; code folding 
(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key "\M-+" 'jao-toggle-selective-display)

;; Set up default editing mode.
(setq default-major-mode 'indented-text-mode)
(defun my-text-hook ()
  "Set up for editing text, my way"
  (interactive)
  (set-fill-column 70)
  (turn-on-auto-fill))
(setq indented-text-mode-hook 'my-text-hook)
(setq-default save-place t)
(toggle-text-mode-auto-fill)       ;always auto-fill in text mode

(load "cl-indent") 
(setq lisp-indent-function 'common-lisp-indent-function)


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

;;this may interact badly with jsp mode
(add-hook 'java-mode-hook 
	  '(lambda ()
	    (make-local-variable 'write-contents-hooks)
	    (add-hook 'write-contents-hooks 'java-mode-untabify)))

(defun my-jde-import-cleanup () "add all imports, remove unnecessary ones, and organize"
       (interactive "P")
       (let 
	   (jde-import-expand-imports) 
	 (jde-import-all)
	 ;; borken? (jde-import-kill-extra-imports)
	 (jde-import-organize)))

;;java mode key bindings 
(add-hook 'java-mode-hook
	  '(lambda ()
	    (define-key jde-mode-map [f3] 'jde-open-class-at-point)
	    (define-key jde-mode-map "\C-o" 'my-jde-import-cleanup)))

(set-cursor-color "deep pink")
(set-foreground-color "black")
;;(set-background-color "cornsilk2") ;I like this on ctr
(set-background-color "ivory")	; and this on lcd

(setq initial-frame-alist
      '((foreground-color . "black")
	(background-color     . "ivory")))

;; Enable dictionary mode (dictionary.el).  With this, you can do a
;;  M-x dictionary-search and search a number of free
;;  dictionaries on the web for a definition.  Works great.
;; http://www.in-berlin.de/User/myrkr/dictionary.html
   (autoload 'dictionary-search "dictionary"
   "Ask for a word and search it in all dictionaries" t)
   (autoload 'dictionary-match-words "dictionary"
   "Ask for a word and search all matching words in the dictionaries" t)
   (autoload 'dictionary "dictionary"
   "Create a new dictionary buffer" t)
;; Assign keyboard shortcuts C-c s and C-c m.
   (global-set-key "\C-cs" 'dictionary-search)
   (global-set-key "\C-cm" 'dictionary-match-words)

;; * Here is some Emacs Lisp that will make the % key show the matching
;; parenthesis, like in vi.	In addition, if the cursor isn't over a
;; parenthesis, it simply inserts a % like normal.  (`Parenthesis' actually
;; includes and character with `open' or `close' syntax, which usually means "()[]{}".)
;; By an unknown contributor
   (global-set-key "%" 'match-paren)
   (defun match-paren (arg)
   "Go to the matching parenthesis if on parenthesis otherwise insert %."
   (interactive "p")
   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	 ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	 (t (self-insert-command (or arg 1)))))

;; These two bindings make it easier to find a mismatched parenthesis:
;(global-set-key "\e'" 'forward-sexp)
;(global-set-key "\e;" 'backward-sexp)

;;JSP mode bdc 20050114
;; from http://www.thispiratekillsfascists.com/blog/2004/06/jsp_mode_for_em.html
;; Get <a href="http://www.tug.org/tex-archive/support/iso-tex/multi-mode.el">multi-mode.el and put it in your 
;; <a href="http://www.anc.ed.ac.uk/%7Edcs/nmlnapster-repository/neuron/load-path.html">load path. 
;; multi-mode lets you set up mode switching based on regular expressions.
;; Follow <a href="http://ccm.redhat.com/bboard-archive/java/000cIT.html">John Stauffer�s instructions</a> 
;; for configuring multi-mode for JSP.
;; (load-file (expand-file-name "multi-mode.el" 
;;             (expand-file-name "prog-modes" 
;;              (expand-file-name ".emacs-site-lisp" "~"))))
;; (require 'multi-mode)
;; (defun jsp-mode () (interactive)
;;        (multi-mode 1
;; 		   'html-mode
;; 		   '("<%--" indented-text-mode)
;; 		   '("<%@" indented-text-mode)
;; 		   '("<%=" html-mode)
;; 		   '("<%" java-mode)
;; 		   '("%>" html-mode)
;; 		   '("<script" javascript-mode)
;; 		   '("</script>" html-mode)
;; 		   ))
;; ;; Add jsp-mode to your <a href="http://www.gnu.org/software/emacs/elisp-manual/html_node/elisp_351.html#IDX1077">auto-mode-alist
;; (setq auto-mode-alist (append '(("\\.jsp\\'" . jsp-mode)) auto-mode-alist))
;; ;;end jsp mode

;; (load-file (expand-file-name "javascript-mode.el" 
;;             (expand-file-name "prog-modes" 
;;              (expand-file-name ".emacs-site-lisp" "~"))))
;; (require 'javascript-mode)
;; (add-to-list 'auto-mode-alist
;; 	     (cons (concat "\\." (regexp-opt '("js" "jsc") t) "\\'")
;; 		   'javascript-mode))

(setq load-path (append '("~/.emacs-site-lisp/owl/" "~/.emacs-site-listp/w3/") load-path))
(autoload 'owl-mode "owl-mode" "OWL mode." t)
(push (cons "\\.owl" 'owl-mode) auto-mode-alist)
