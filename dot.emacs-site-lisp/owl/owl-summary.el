;;; owl-summary.el --- a part of the simple OWL mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro

;;; This package is designed to be used with XEmacs and Gnu Emacs



;;; OWL SUMMARY MODE and display fns - like a headers list in VM MODE

;;; see owl-display-summary-list
(defvar owl-summary-list nil "alist of references in summary-buffer")
(make-variable-buffer-local 'owl-summary-list)


;;; these vars should have the same values in the owl buffer and its summary buffer
(defvar owl-buffer-definition-list nil "List of OWL definitions culled from current buffer.")
(make-variable-buffer-local 'owl-buffer-definition-list)

(defvar owl-summary-buffer nil "buffer containing summary of current owl buffer")
(make-variable-buffer-local 'owl-summary-buffer)

(defvar owl-buffer nil "current owl content buffer buffer")
(make-variable-buffer-local 'owl-buffer)

(defvar owl-summary-config nil)
(make-variable-buffer-local 'owl-summary-config)

(defvar owl-summary-mode-hook nil)

;;;These function are basically the same as what vm uses to enable mouse highlighting
;;; in the vm summary window.  

(defun owl-fsfemacs-mouse-p ()
  (and (not in-xemacs-p)
       (fboundp 'set-mouse-position)))

(defun owl-xemacs-mouse-p ()
  (and in-xemacs-p
       (fboundp 'set-mouse-position)))

(defvar owl-popup-menu-on-mouse-3 nil) ;; not implemented

(defun owl-install-summary-mouse (map)
  (cond ((owl-xemacs-mouse-p)
	 (define-key map 'button3 'owl-mouse-button-2))
	((owl-fsfemacs-mouse-p)
	 (define-key map [mouse-3] 'owl-mouse-button-2)
;	 (if owl-popup-menu-on-mouse-3
;	     (progn
;	       (define-key map [mouse-3] 'ignore)
;	       (define-key map [down-mouse-3] 'owl-mouse-button-3)))
	)
  ))


(defvar owl-summary-mode-map 
    (let ((map (make-sparse-keymap)))
      (define-key map "n" 'owl-next-def-summary)
      (define-key map "p" 'owl-previous-def-summary)
      (define-key map " " 'owl-current-def-summary)
      (define-key map "s" 'owl-refresh-summary)
      (owl-install-summary-mouse map)
      map
      )
  "Keymap for VM Summary mode")


(defun owl-list-forms-buffer (&optional buf)
  (interactive)
  (let* ((curbuf (or (and buf (get-buffer buf))(current-buffer)))
	 (curpos (point))
	 (cbufname (buffer-name curbuf))
	 (sbufname (concat cbufname " Summary"))
	 )
    (save-excursion
      (set-buffer curbuf)
;      (message "Collecting definitions list...")
      (beginning-of-buffer)
      (when (and (re-search-forward "<rdf:RDF" nil t)
		 (re-search-forward ">" nil t))
	(let ((next (point))
	      (defs nil) 
	      def)
	  (setq next (cdr (owl-goto-tag-begin)))
	  (while (and next
		      (not (eobp))
		      (not (looking-at "</"))) ;; e.g. </rdf:RDF>
;	    (dbg (format "list loop at %s %s" (point)(bs 10)))

	    (cond ((looking-at "<\\(rdfs:comment\\|!--\\)")
;		   (dbg "skip comment ")
		   (setq next (and (owl-forward-tag)
				   (cdr (owl-goto-tag-begin))))
		   )
		  (t 
		   ;;; read tag and ID now moves past the def
		   (setq def (owl-read-tag-and-id))
;		   (dbg "Def: %s" def)
		   (push def defs)
		   (setq next (cdr (owl-goto-tag-begin)))
		   ))

	    )
;	  (message nil)
	  (goto-char curpos)
	  (setq owl-buffer-definition-list (reverse defs)))))))

;; return tag info as (begpt endpt tag id|about url)
(defun owl-read-tag-and-id ()
  (interactive)
  (let* ((begpt (point))
	 (endpt (owl-forward-tag))
	 (ebtagpt 0)
	 (idinfo (list endpt begpt)))
    (when (and begpt endpt)
      (goto-char begpt)
      (setq ebtagpt (owl-search-fwd ">"))
      (goto-char begpt)
      (when (and (not (eobp))
		 (looking-at "<[^/]")
		 (re-search-forward "<\\([^ \t\n>]+\\)" nil t))
	(let* ((tb (match-beginning 1))
	       (te (match-end 1))
	       (tag (buffer-substring tb te))
	       )
;;;	    (dbg "tag=%s ebtgpt=%s" tag ebtagpt)
	    (push tag idinfo)
	    (goto-char (match-end 0))
	    (when (re-search-forward 
		   "\\(\\(rdf:\\|\\)\\(ID\\|about\\)[ \t\n]*=\\)\"\\([^\"]*\\)\""
		   ebtagpt t)
	      (let* ((bid (match-beginning 1))
		     (eid (match-end 1))
		     (burl (match-beginning 4))
		     (eurl (match-end 4))
		     (idstr (buffer-substring bid eid))
		     (urlstr (buffer-substring burl eurl)))
		(push (list idstr urlstr) idinfo)
		;;; now look thru subtags for a subclass/subproperty
		(when (re-search-forward ">" nil t)
		  (unless (looking-back-at "/>")
		  (push (owl-read-parents) idinfo)))
		)))))
    
;    (goto-char begpt)
    (setq idinfo (reverse idinfo))
;    (dbg "res=%S" idinfo)
    idinfo
    ))

(defun owl-sub-id () 
  (when (looking-at 
   "<rdfs:sub\\(Property\\|Class\\)Of[^>]+rdf:resource[ ]*=[ ]*\"\\([^\"]*\\)")
;;;    (dbg "matched beg %s end %s" (match-beginning 2) (match-end 2))
    (buffer-substring (match-beginning 2) (match-end 2))))


(defun owl-read-parents ()
  (let ((done nil)(subs nil) (sub nil))
    (while (and (not done)
		(owl-goto-tag-begin))
;      (dbg "in readparents at %s\n" (bs 10))
      (cond ((looking-at "</")(setq done t) (owl-up-tag-end))
	    ((setq sub (owl-sub-id))
	     (push sub subs)
	     (owl-forward-tag)
	     (owl-goto-tag-begin))
	  (t (owl-forward-tag)
;	     (dbg "after ft %s" (bs 10))
	     (owl-goto-tag-begin)
	     )))
  subs))
	   

;; for testing
;(define-key owl-mode-map "\e," 'owl-read-tag-and-id)

(defun owl-display-summary-list (&optional recomputep owl-buf summary-buf)
  (let* ((dbuf (or (and owl-buf (get-buffer owl-buf)) owl-buffer))
	 (sbuf (or (and summary-buf (get-buffer summary-buf)) owl-summary-buffer))
	 (deflist (cond ((and (not recomputep) owl-buffer-definition-list))
			((consp recomputep) recomputep)
			(t (owl-list-forms-buffer dbuf))))
	 (summary-list nil)
	 (maxlen 0)
	 )
;    (scratch-msg "in display-list defs=%s sbuf=%s" deflist sbuf)
    (save-excursion
      (set-buffer sbuf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq owl-buffer-definition-list deflist)
;      (scratch-msg "in display sum buflist=%s" deflist)
      ;; header line
      (let ((pt (point)))
	(insert (format "<!-- Definitions in %s (& parents)--> \n" (buffer-name dbuf)))
	(owl-mouse-set-mouse-track-highlight pt (point))
	(push (cons pt nil) summary-list)
	)
      ;; find out how long things are (for column creation)
      (dolist (elt deflist)
	(let ((len (length (third elt))))
	  (if (> len maxlen)(setq maxlen len))))
      (dolist (elt deflist)
	(let* ((pt (point)) 
	       (tag (third elt))
	       (id (first (fourth elt)))
	       (name (second (fourth elt)))
	       (parents (fifth elt))
	       (taglen (length tag))
	       (idlen (length id))
	       )
	  (push (cons pt elt) summary-list)
	  
	  (insert (format "%s %s%s%s %s    %s" 
			  tag (make-string (- maxlen taglen) ?\  ) 
			  (make-string (- 10 idlen) ?\ )
			  id
			  name
			  (if parents parents "")
			  ))
	  (owl-mouse-set-mouse-track-highlight pt (point))
	  (insert "\n")
	  ))
      (setq owl-summary-list summary-list)
      (font-lock-mode 1)
      (setq buffer-read-only t)
      )))


(defun owl-refresh-summary () 
  "Refresh the OWL Summary window's contents after the OWL file changed."
  (interactive)
  (set-window-configuration owl-summary-config)
  (owl-display-summary-list t))


;;; create an overlay for mouse highlighting
;;; if already had an overlay then move it. 
(defun owl-mouse-set-mouse-track-highlight (start end &optional overlay)
  (if (null overlay)
	(cond (in-xemacs-p
	       (let ((o (make-extent start end)))
		 (set-extent-property o 'start-open t)
		 (set-extent-property o 'priority 10)
		 (set-extent-property o 'highlight t)
		 o ))
	      (t
	       (let ((o (make-overlay start end)))
		 (overlay-put o 'mouse-face 'highlight)
		 o )))

    (cond (in-xemacs-p
	   (set-extent-endpoints overlay start end))
	  (t
	   (move-overlay overlay start end)))))



;;; other widnow
;;; get-buffer-window - window displaying  buffer
;;; window-height
;;; split-window


;;; starting from owl buffer, split window, save configuration
(defun owl-make-summary-config ()
  (interactive)
  (cond ((and (boundp 'owl-summary-buffer) owl-summary-buffer
	      (boundp 'owl-summary-config owl-summary-config))
	 (set-window-configuration owl-summary-config)
	 owl-summary-buffer
	 )
	(t 
	 (delete-other-windows) ;; one window to start
	 (let* ((owl-buf (current-buffer))
		(win (get-buffer-window owl-buf))
		(frame-ht (window-height))
		(num-forms (if (and (boundp 'owl-buffer-definition-list)
				      owl-buffer-definition-list)
			       (length owl-buffer-definition-list)
			     10))
		(sum-ht (if (< (/ frame-ht 2) (min num-forms 10))
			    (/ frame-ht 2)
			  (min (+ 3 num-forms) (/ frame-ht 4))))
		(lower-window (split-window win sum-ht))
		(summary-buffer
		 (get-buffer-create (format "%s Summary" (buffer-name))))
		)
	 ;; switch to the summary window, set buffer and resave vars
	   (set-window-buffer win summary-buffer)
;; save the configuration on both windows
	   (let ((config (current-window-configuration)))
	     (setq owl-summary-config config)
	     (setq owl-summary-buffer summary-buffer)
	     (setq owl-buffer owl-buf)
;	     (trace-msg "buffer=%s\n" (current-buffer))
	     (font-lock-mode 0)
	     (other-window 1)
	     ;; switch back to owl window
	     (setq owl-summary-config config)
	     (setq owl-summary-buffer summary-buffer) ;; in owl-buffer
	     (setq owl-buffer owl-buf)
	     summary-buffer
	     )))))
	

(defun owl-mouse-button-2 (event)
  (interactive "e")
  ;; go to where the event occurred
  (cond ((owl-xemacs-mouse-p)
	 (set-buffer (window-buffer (event-window event)))
	 (and (event-point event) (goto-char (event-point event))))
	((owl-fsfemacs-mouse-p)
	 (set-buffer (window-buffer (posn-window (event-start event))))
	 (goto-char (posn-point (event-start event)))))
  ;; now dispatch depending on where we are
  (mouse-set-point event)
  (beginning-of-line)
  (setq this-command 'owl-current-def-summary)
  (call-interactively 'owl-current-def-summary))



(defun owl-check-for-killed-summary ()
  (and (bufferp owl-summary-buffer) 
       (or (null (buffer-name owl-summary-buffer))
	   (not (buffer-live-p owl-summary-buffer)))
       (let ((mp owl-buffer-definition-list))
	 (setq owl-summary-buffer nil)
	 )))


(defun owl-next-def-summary () 
  (interactive)
  (forward-line 1)
  (end-of-line)
  (when (eobp) (forward-line -1))
  (owl-current-def-summary)
  )

(defun owl-previous-def-summary ()
  (interactive)
  (beginning-of-line)
  (unless (bobp)   (forward-line -1))
  (owl-current-def-summary)
  )


;;; to do this the robust way, we don't use the file positions, but do
;;; owl-find-def-in-buffer instead
(defun owl-current-def-summary ()
  (interactive)
  (beginning-of-line)
  (let* ((pt (point))
	 (rec (assoc pt owl-summary-list))
	 tag id name match-string)
    (when rec
      (setq tag (fourth rec))
      (setq id (car (fifth rec)))
      (setq name (second (fifth rec)))
      (setq match-string (format  "<%s[ \t\n]+%s[ \t\n]*\"%s\"" tag id name))
      )
    ;; make sure 2 windows showing, the other being the owl buffer
    (let ((cb (current-buffer))
	  (other (window-buffer (next-window (selected-window)))))
      (unless (eq other owl-buffer)
	(set-window-configuration owl-summary-config)))
    (save-excursion
      (other-window 1)
;      (set-buffer owl-summary-of-buffer)
;      (goto-char newpt)
      (let ((oldpt (point)))
	(beginning-of-buffer)
	(cond ((eq pt 1)) ;; on header line
	      ((and match-string (search-forward-regexp match-string nil t))
	       (beginning-of-line))
	      (t
	       (goto-char oldpt)
	       (message "Definition not found."))
	      ))
      (recenter 1)
      (other-window 1)
      )))


; old     '("\\(owl:\\|rdfs?:\\|\\)\\(Class\\|Ontology\\) +\\(rdf:\\|\\)\\(ID\\|about\\)= *\\([^ \n]*\\)"

(defvar owl-summary-font-lock-keywords
  (let ()
    (list 
     ;; black for the def parts of PROPERTY DEFINITION
     ;; and of TransitiveProperty UnambiguousProperty UniqueProperty
     (list
      (concat 
       "^\\(owl:\\|rdfs?:\\|owl:\\|\\)\\(Class\\|Ontology\\|\\([A-Za-z]*Property\\)\\)"
       " +\\(rdf:\\|\\)\\(ID\\|about\\)= *\\([^ \n]*\\)")
      '(1 owl-normal-face t)
      '(2 owl-keyword-face t) 
      '(4 owl-normal-face t)
      '(5 owl-keyword-face t) 
      '(6 (if (match-beginning 3) owl-property-face owl-class-face) t))
     '("\\([^:]+:\\|\\)\\(\\([A-Z]\\)?[^ ]*\\)[ ]+\\(rdf:\\|\\)\\(ID\\|about\\)=[ ]*\\([^ \n]*\\)"
       (1 owl-normal-face nil)
       (2 (if (match-beginning 3) owl-class-face owl-property-face) nil) 
       (4 owl-normal-face nil)
       (4 owl-keyword-face nil)
       (5 owl-string-face nil))
     ;; XML Comments: <!-- ... -->. - here the header.
     '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)"
       1 owl-comment-face t)
     )
    ))


(put 'owl-summary-mode 'font-lock-defaults '(owl-summary-font-lock-keywords nil nil))

(defun owl-summary-mode-internal ()
  "Mode for the summary window associated with OWL files. 
   mouse middle click on a line jumps the main window to that definition. 
Typing n or p moves next or previous line, and refocusses the main window.
Typing a space refocusses on the current line. 
Typing an 's' rebuilds the summary. "

  (setq mode-name "OWL Summary"
	major-mode 'owl-summary-mode
;	mode-line-format vm-mode-line-format
	;; must come after the setting of major-mode
;	mode-popup-menu (and vm-use-menus vm-popup-menu-on-mouse-3
;			     (vm-menu-support-possible-p)
;			     (vm-menu-mode-menu))

	buffer-read-only t
	truncate-lines t)
  ;; horizontal scrollbar off by default
  ;; user can turn it on in summary hook if desired.
  (and in-xemacs-p (featurep 'scrollbar)
       (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (use-local-map owl-summary-mode-map)
  (cond	(in-xemacs-p
	 (put major-mode 'font-lock-keywords-case-fold-search nil))
	((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
	 (make-local-variable 'font-lock-defaults)
	 (setq font-lock-defaults '(owl-summary-font-lock-keywords nil nil)))

	;; this probably isnt working anyway:
	(t ; Emacs 19.28 and older
	 (make-local-variable 'font-lock-keywords-case-fold-search)
	 (make-local-variable 'font-lock-keywords)
	 (make-local-variable 'font-lock-no-comments)
	 (setq font-lock-keywords-case-fold-search nil) ; was t
	 (setq font-lock-keywords owl-summary-font-lock-keywords)
	 (setq font-lock-no-comments t)))

  (run-hooks 'owl-summary-mode-hook)
  )

;(fset 'vm-summary-mode 'vm-mode)
;(put 'vm-summary-mode 'mode-class 'special)



(defun owl-summarize (&optional display raise)
  "Summarize the contents of the current OWL buffer in a summary buffer."
  (interactive ) ;"p\np"
  (owl-check-for-killed-summary)
  (if (null owl-summary-buffer)
      (let ((dbuf (current-buffer))
	    (list (owl-list-forms-buffer))
	    (sbuf (owl-make-summary-config)))
	(save-excursion
	  (set-buffer sbuf)
	  (abbrev-mode 0) ;; turn off abbrev mode
	  (auto-fill-mode 0) ;; turn off autofill mode
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (owl-summary-mode-internal)
	  (owl-display-summary-list list dbuf sbuf)
	  )
	;; back in main buffer
	)
    (owl-refresh-summary)
    ))



