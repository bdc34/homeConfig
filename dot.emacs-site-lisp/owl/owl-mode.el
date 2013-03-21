;;; owlite.el --- a simple OWL mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro
;;; updated for OWL by David Rager


;;; This package is designed to be used with XEmacs and Gnu Emacs

;;; XEmacs is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; XEmacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with XEmacs; see the file COPYING.  If not, write to the Free
;;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'easymenu)

;;; which emacs are we in?
(defvar in-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))


(defvar owl-mode-syntax-table
  (let ((s (copy-syntax-table text-mode-syntax-table)))
;;; from html mode
    (modify-syntax-entry ?< "(>" s)
    (modify-syntax-entry ?> ")<" s)
    (modify-syntax-entry ?\" ".   " s)
    (modify-syntax-entry ?\\ ".   " s)
    (modify-syntax-entry ?'  "w   " s)
  ;;; change : and _ to be part of words for RDF
    (modify-syntax-entry ?:  "w   " s)
    (modify-syntax-entry ?_  "w   " s)
    s))


;;; define some shortcuts (future use)
(defvar owl-mode-abbrev-table nil
  "Abbrev table in use in sgml-mode.")

(define-abbrev-table 'owl-mode-abbrev-table ())

;;; The OWL command key table
(defvar owl-mode-map nil "Keymap for OWL mode")
(unless owl-mode-map
  (setq owl-mode-map (make-sparse-keymap)))

(defgroup owl nil
  "OWL Markup Language")

;;; ================ user variables ================

(defcustom owl-helper-build-new-buffer t
  "*If not nil, then insert `owl-helper-new-buffer-strings' for new buffers."
  :type 'boolean
  :group 'owl)

;(defcustom owl-mode-hook '(owl-parse-prolog t)
;  "A hook or list of hooks to be run when entering OWL mode."
;  :type 'hook
;  :group 'owl)

(defcustom owl-indent-step 2
  "Number of spaces to indent succeeding layers of tag forms."
  :type 'integer
  :group 'owl)

;;; ================================================================
;;; The next two strings define what is inserted into a new OWL buffer
;;; when creating it from scratch. It consists of a default XML prolog
;;; and a default RDF Tag body, including XMLNS definitions that should 
;;; be most commonly used.

;;; DER - removed ".daml"
;;; DER - namespaces are not allowed to start with "xml" (reserved).
(defcustom owl-helper-xml-version 
  (concat
    "<?xml version='1.0' encoding='ISO-8859-1'?>\n<!DOCTYPE uridef[\n"
"  <!ENTITY owl \"http://www.w3.org/2002/07/owl\">\n"
"  <!ENTITY rdf \"http://www.w3.org/1999/02/22-rdf-syntax-ns\">\n"
"  <!ENTITY rdfs \"http://www.w3.org/2000/01/rdf-schema\">\n"
"  <!ENTITY xsd \"http://www.w3.org/2001/XMLSchema\">\n]>\n\n"

   )
  "first lines of file if specifying xml version and doctype" 
  :type 'string
  :group 'owl)

(defun owl-helper-created-string ()
  (format "<!-- Created: %s -->\n" (current-time-string)))


;;; DER - namespaces are not allowed to start with "xml" (reserved).
(defcustom owl-helper-new-buffer-template
    '(owl-helper-xml-version
      "<rdf:RDF\n"
      "  xmlns:owl  =\"http://www.w3.org/2002/07/owl#\"\n"
      "  xmlns:rdf  =\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n"
      "  xmlns:rdfs =\"http://www.w3.org/2000/01/rdf-schema#\"\n"
      "  xmlns:xsd  =\"http://www.w3.org/2001/XMLSchema#\"\n"
      "  xmlns      =\"\"\n"
      " >\n\n"

      "<owl:Ontology rdf:about=\"\">\n"
      " <owl:versionInfo>$Id$</owl:versionInfo>\n"
      " <rdfs:comment>\n"
      " </rdfs:comment>\n"
      "</owl:Ontology>\n\n"
      (owl-helper-created-string) ;; put date/time in the file (future)
      "\n</rdf:RDF>\n"
      )


  "*Template for new OWL buffers.
Inserted by `owl-helper-insert-new-buffer-strings' if
`owl-helper-build-new-buffer' is set to t"
  :type 'sexp
  :group 'owl)



;;; ================ REQUIRED FILES ================

(load "owl-menu") ;; defines menu of commands 
(load "owl-fontify") ;; font lock definitions
(load "owl-insert")  ;; insert new definition commands
(load "owl-motion") ;; defines commands for moving about among tags TBD
(load "owl-w3") ;; command for finding definitions (meta-dot)
(load "owl-summary") ;; summary mode window

;;; ================================================

(defun owl-mode ()
  "Major mode for editing OWL documents.
OWL is an XML-based knowledge
representation language for describing semantic content. See
http://www.w3.org/2001/sw for more information. 

This current mode is NOT based on PSGML mode, which uses an SGML parser.
Instead, we use local heuristics for navigation and lookup that 
enable its use with 'mostly syntactically correct' files that 
occur while the ontologies are under development. 

Comments to burstein@bbn.com
Key bindings:
 \\{owl-mode-map}

Note also that M-C-f and M-C-b move forward, backward over balanced
< > pairs as though they were parenthasized expressions. 
"
  (interactive)
  (kill-all-local-variables)
  (setq local-abbrev-table owl-mode-abbrev-table)
  (use-local-map owl-mode-map)
  (setq mode-name "OWL")
  (setq major-mode 'owl-mode)
  (make-local-variable 'owl-default-doctype-name)
  (setq 
	owl-default-doctype-name    "owl"
	owl-always-quote-attributes t 
	owl-xml-p                   t
	)

  ;; font-lock setup for various emacsen: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; By Ulrik Dickow <dickow@nbi.dk>.  (Last update: 05-Sep-1995).
  (cond	(in-xemacs-p
	 (put major-mode 'font-lock-keywords-case-fold-search nil))
	((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
	 (make-local-variable 'font-lock-defaults)
	 (setq font-lock-defaults '(owl-font-lock-keywords nil nil)))

	;; this probably isnt working anyway:
	(t ; Emacs 19.28 and older
	 (make-local-variable 'font-lock-keywords-case-fold-search)
	 (make-local-variable 'font-lock-keywords)
	 (make-local-variable 'font-lock-no-comments)
	 (setq font-lock-keywords-case-fold-search nil) ; was t
	 (setq font-lock-keywords owl-font-lock-keywords)
	 (setq font-lock-no-comments t)))
  
  (set-syntax-table owl-mode-syntax-table)
  (easy-menu-add owl-menu)		; in xemacs/lisp/easymenu.el
  (easy-menu-add owl-insert-menu)
  (easy-menu-add owl-insert-class-menu)
  (easy-menu-add owl-insert-property-menu)
  (let ((newbuf (zerop (buffer-size))))
    (if (and owl-helper-build-new-buffer newbuf)
	(owl-helper-insert-new-buffer-strings))

  ;; Set up the syntax table. -- using xml-syntax so should be ok. 

    (run-hooks 'text-mode-hook 'owl-mode-hook)

    (font-lock-mode 1) ;; turn on font-lock mode to see the pretty colors!
;; this doesn't work for some reason
;    (when newbuf
;      (end-of-buffer)
;      (search-backward "<!-- Created" nil t)
;      (dbg "after created")
;      (next-line 1))
    ))


(put 'owl-mode 'font-lock-defaults '(owl-font-lock-keywords nil nil))


;;; This function replaces the only piece of  tempo.el we still are using:
;;; the template code for this simple case
(defun owl-insert-strings (template &optional sublist)
    (cond ((stringp template) (insert template))
	  ((symbolp template)
	   (when (symbol-value template)
	     (owl-insert-strings (symbol-value template))))
	  ((and (consp template) sublist)
	   (owl-insert-strings (eval template)))
	  ((consp template)
	   (dolist (elt template)
	     (owl-insert-strings elt t)))))


(defun owl-helper-insert-new-buffer-strings ()
  "Insert `owl-helper-new-buffer-template'."
;  (tempo-template-owl-skeleton)
  (owl-insert-strings owl-helper-new-buffer-template)
  (end-of-buffer)
  (previous-line 2)
  )

(add-hook 'owl-mode-hook 'turn-on-font-lock)

;;;###autoload
(autoload 'owl-mode "owl-mode" "OWL mode." t)

(push (cons "\\.owl" 'owl-mode) auto-mode-alist)

