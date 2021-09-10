;; The goal of this file is to allow a single file that
;; make emacs behave in a way that is comfortable.
;; Then this can be loaded from git on some machine that is 
;; not mine.
;; This should work for v24 and pre-v24

(show-paren-mode 1);; Bracket/brace/parentheses highlighting: 

(tool-bar-mode -1);;lose tool bar, bleck
(scroll-bar-mode -1);;lose scrollbars, oph.

(setq inhibit-startup-message t)
(setq transient-mark-mode t)
(setq delete-selection-mode t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-prompt-read-only t)

;; set scrolling to be a bit less frantic
(setq-default scroll-margin 1
              scroll-conservatively 0
              scroll-up-aggressively 0.40
              scroll-down-aggressively 0.25)

;; mouse bindings for the fancy forwrd and back buttons
(global-set-key [mouse-9] 'next-buffer)
(global-set-key [mouse-8] 'previous-buffer)

;; slow scrolling with wheel and ctrl
(global-set-key (kbd "<C-mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<C-mouse-5>") 'scroll-up-line)

(add-hook 'dired-mode-hook ;do vi movement in dired
          '(lambda ()
            (define-key dired-mode-map "j" 'dired-next-line)
            (define-key dired-mode-map "k" 'dired-previous-line)
            (define-key dired-mode-map "h" 'dired-up-directory)))

;; get compile-mode like error jumping in term
(add-hook 'term-mode-hook 'compilation-shell-minor-mode)

(display-time)

(setq line-number-mode t)
(setq menu-bar-enable-clipboard t)  ;make menu edit do clipboard stuff
(setq disabled-command-hook nil)  ; disable command disabling
(setq visible-bell t);; i hate my computer beeping at me 

;; shell-mode ; why is this not enabled by default?
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(setq kill-read-only-ok 1)	;;allow cutting of text from read only buffers

;; make a command like vi's o and O
;; from http://www.emacswiki.org/emacs/OpenNextLine

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

(global-set-key [S-return]   'open-next-line)
(global-set-key [C-S-return] 'open-previous-line)

;; don't keep backups in working directories 
(setq backup-directory-alist '((".*" . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 10
  kept-old-versions 2
  version-control t
  vc-make-backup-files t)

;;  #foo# auto-save files to a directory
(setq auto-save-file-name-transforms `((".*" "~/.emacs-auto-saves/" t)))

;; Don't make emacs lock files like .#something.json -> hostx.23432:161139029
(setq create-lockfiles nil)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

