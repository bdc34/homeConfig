;;;some tramp related stuff from emacswiki:
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo::" )
  "*The filename prefix used to open a file with `find-file-root'.")
(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")
(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")
(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."  
  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
		 ;; use a separate history list for "root" files.
		 (file-name-history find-file-root-history)
		 (name (or buffer-file-name default-directory))
		 (tramp (and (tramp-tramp-file-p name)
					 (tramp-dissect-file-name name)))
		 path dir file)
	;; If called from a "root" file, we need to fix up the path.
	(when tramp
	  (setq path (tramp-file-name-path tramp)
			dir (file-name-directory path)))
	(when (setq file (read-file-name "Find file (UID = 0): " dir path))
	  (find-file (concat find-file-root-prefix file))
	  ;; If this all succeeded save our new history list.
	  (setq find-file-root-history file-name-history)
	  ;; allow some user customization
	  (run-hooks 'find-file-root-hook))))

;;C-xC-r will open a file as root
(global-set-key [(control x) (control r)] 'find-file-root)

;;make a buffer with root perm have a warning
(defface find-file-root-header-face
	'((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root."
)
(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `find-file-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
		 (space (+ 6 (- (frame-width) (length warning))))
		 (bracket (make-string (/ space 2) ?-))
		 (warning (concat bracket warning bracket)))
	(setq header-line-format
		  (propertize  warning 'face 'find-file-root-header-face))))
(add-hook 'find-file-root-hook 'find-file-root-header-warning)

(defun my-find-file-root-hook ()
  "Some personal preferences."
  ;; Turn auto save off and simplify backups (my version of tramp
  ;; barfs unless I do this:-)
  (setq buffer-auto-save-file-name nil)
  (set (make-local-variable 'backup-by-copying) nil)
  (set (make-local-variable 'backup-directory-alist) '(("."))))
(add-hook 'find-file-root-hook 'my-find-file-root-hook)