;;; owl-menu.el --- a part of the simple OWL mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro
;;; updated for OWL by David Rager

;;; This package is designed to be used with XEmacs and Gnu Emacs



;;;; Definition of the menu and keystroke commands


(unless (boundp 'in-xemacs-p)
  (setq in-xemacs-p (string-match "XEmacs" (emacs-version))))

;;; COMPAT for DAML/OWL mode: 
;;; functions used by OWL mode not found in GNU emacs
(unless (string-match "XEmacs\\|Lucid" (emacs-version))

(defun mapvector (fn list)
  (coerce (mapcar '(lambda (elt) (funcall fn elt))
		  list) 'vector))

(defmacro char= (a b) `(and (numberp ,a)(numberp ,b)(= ,a ,b)))
(defmacro characterp (c) `(and (numberp ,c)(< ,c 256)))

) ;; end unless for COMPAT


;;; hide conditionalization of keystroke definitions between XEmacs and GNU Emacs
;;; for GNU:
;;; take lists like (control ?f) (meta ?y) (control meta ?z)
;;; and convert them to strings like "^f" "^[y" "^[^z" for GNU
;;; or to vector of these elts for Xemacs

(unless (symbol-function 'some) ;; some emaxen don't have this???
  (defun some (fn list)
    (let ((res nil) elt)
      (while (and list (not res))
	(setq elt (car list)) 
	(setq list (cdr list))
	(setq res (funcall fn elt)))
      res))
  )



(defun keystroke (list)
  "Convert an Xemacs keystroke representation to GNU form, 
if in XEmacs, then much same as quote, just make a vector if a sequence."
  (if in-xemacs-p
      (if (and (consp list) (some 'consp list)) ;; a complex key seq
	  (mapvector 'keystroke list)
	list)
	;; else GNU
    (cond
     ((integerp list) (char-to-string list))
     ((symbolp list) (symbol-name list)) ;; should be a single character name
     ((consp list)
      (if (some 'consp list)
	  (apply 'concat (mapcar 'keystroke list))
	(let* ((ctrl (member 'control list))
	      (meta  (member 'meta list))
	      (key (car (last list)))
	      (str  ;; note that may be multi-char if not control or meta
	       (cond ((symbolp key) (symbol-name key))
		     ((integerp key)  ;; a char
		      (char-to-string key))
		     ((stringp key) key))))
	  (when ctrl ;; then key should be 1 char
	    (setq key (elt str 0))
	    (setq key (1+ (- key ?a))) ;; convert to control char
	    (setq str (char-to-string key))) ; redo string
	  (if meta (setq str (concat "\e" str)))
	  str)))
     (t list))))


;;; printable representation appropriate for menus

(defun keystroke-string (list)
  "Convert an keystroke representation to printable form"
  (cond
   ((if in-xemacs-p 
	(characterp list)
      (integerp list))
    (char-to-string list))
   ((consp list)
    (if (consp (car list))
	(apply 'concat (mapcar 'keystroke-string list))
      (let* ((ctrl (member 'control list))
	     (meta  (member 'meta list))
	     (key (car (last list)))
	     (str-elts
	      (append (if ctrl '("C-"))
		      (if meta '("M-"))
		      (list (char-to-string key))
		      (list " "))))
	(apply 'concat str-elts))))
	(t list)))


; (define-key owl-mode-map "\C-z\C-z" 'suspend-or-iconify-emacs)


;;; items are each (menu-text action keystroke active?)
(defun owl-make-menu (menuvar name mode-map doc items)
  (let ((menuitems nil)) 
    (dolist (item items)
	(cond ((stringp item)
	       (setq menuitems (nconc menuitems (list item))))
	      (t 
	  ;else a form
	       (let ((menuitem nil)
		     (label (first item))
		     (action (second item))
		     (keys (third item))
		     (active (if (cdddr item) (fourth item) t))
		     )
		 (setq menuitem
		   (mapvector 
		    'identity
		    (list label action 
			  :keys (keystroke-string keys)
			  :active active)))
		 (setq menuitems (nconc menuitems (list menuitem)))
	       ;; also define the key
		 
;;;		 (scratch-msg "before define key %S %s\n" (keystroke keys) action)
		 (define-key mode-map (keystroke keys) action)
	       ))
      ;; this defines the keystroke based on the items
	))
;;   (scratch-msg "Menu is %S" menuitems)
    (easy-menu-do-define menuvar mode-map doc (cons name menuitems))
    ))
    

(defvar owl-menu nil)

;;; also creates keystroke commands
;;; elts are (label fn key active?) active? defaults to t
(defvar owl-menu-items

    '(;; in owl-w3
      ("View URL" owl-view-url (meta ?.)) 
      ("Find Def In Buffer" owl-find-def-in-buffer (meta ?,))
      ("Previous Position" owl-goto-previous-position ((control ?x) ?p))
      ("Remember Position" owl-remember-position ((control ?x) ?.))
      "--"
      ;; in owl-motion
      ("Forward Tag" owl-forward-tag (control meta ?f))
      ("Backward Tag" owl-backward-tag (control meta ?b))
      ("Up Tag" owl-up-tag (control meta ?a))
      ("Up Tag End" owl-up-tag-end (control meta ?e))
      ("Inside Tag" owl-inside-tag (control meta ?i))
      ("Prev Tag Def" owl-prev-tag-def (control meta ?u))
      ("Next Tag Def" owl-next-tag-def (control meta ?d))
      ("Indent Tag Expression" owl-indent-expression (control meta ?q))
      ("Indent Line" owl-indent-line (control ?i)) ;; == tab
      "--"
      ("List Definitions" owl-summarize ((control ?c) ?l ?d))

      ))

(defvar owl-insert-menu nil)

(defvar owl-insert-menu-items
    '( ;; in owl-insert
      ("<Ontology>" owl-insert-ontology ((control ?c) ?o ?t))
      ("<versionInfo>" owl-insert-versionInfo ((control ?c) ?v ?i))
      ("<imports>" owl-insert-imports ((control ?c) ?i ?m))
      ("<priorVersion>" owl-insert-priorVersion ((control ?c) ?p ?v))
      ("<backwardCompatibleWith>" owl-insert-backwardCompatibleWith ((control ?c) ?b ?c))
      ("<incompatibleWith>" owl-insert-incompatibleWith ((control ?c) ?i ?c))
      "--"
      ("<Description ID=?>" owl-insert-description-id ((control ?c) ?d ?i))
      ("<Description about=?>" owl-insert-description-about ((control ?c) ?d ?a))
      ("<comment>" owl-insert-comment ((control ?c) ?c ?m))
      ("<label>" owl-insert-label ((control ?c) ?l ?a))
      ("rdf:resource=" owl-insert-rdf-resource ((control ?c) ?r ?s))
      ("rdf:datatype=" owl-insert-rdf-datatype ((control ?c) ?d ?t))
      "--"
      ("<sameIndividualAs>" owl-insert-sameIndividualAs ((control ?c) ?s ?i))
      ("<sameAs>" owl-insert-sameAs ((control ?c) ?s ?a))
      ("<differentFrom>" owl-insert-differentFrom ((control ?c) ?d ?f)) ;; differentIndividualFrom?
      ("<AllDifferent>" owl-insert-allDifferent ((control ?c) ?a ?d))
      ("<DataRange>" owl-insert-dataRange ((control ?c) ?d ?r))
      "--"
;      ("<List (resource)>" owl-insert-list0 ((control ?c) ?l ?i))
      ("<List>w.<first><rest>" owl-insert-List ((control ?c) ?l ?l))
;;      ("List" owl-insert-List ((control ?c) ?l ?i))
      ("<nil>" owl-insert-nil ((control ?c) ?n ?i))
      "--"
      ("<propTAG [resource=?][/]>" owl-insert-tag ((control ?c) ?t ?r))
      ("<ObjTAG [ID=][/]>" owl-insert-id-tag ((control ?c) ?t ?i))
      ("<ObjTAG [about=][/]>" owl-insert-about-tag ((control ?c) ?t ?a))
      ("</End last open tag>" owl-insert-end-tag ((control ?c) ?/))
      )
  )


(defvar owl-insert-class-menu nil)

(defvar owl-insert-class-menu-items
    '( ;; in owl-insert
      ("<Class ID=?>" owl-insert-class ((control ?c) ?c ?i))
      ("<Class about=?>" owl-insert-class-ref ((control ?c) ?c ?r))
      ("<DeprecatedClass>" owl-insert-deprecatedClass ((control ?c) ?x ?c))
      ("<subClassOf>" owl-insert-subClassOf ((control ?c) ?s ?c))
      ("<Restriction/onProp>" owl-insert-restriction  ((control ?c) ?r ?o)) ;; includes necessary onProperty
      ("<[min|max]cardinality/>" owl-insert-cardinality ((control ?c) ?c ?a))
      ("<hasValue>" owl-insert-hasValue ((control ?c) ?h ?v))
      ("<someValuesFrom>" owl-insert-someValuesFrom ((control ?c) ?s ?v))
      ("<allValuesFrom>" owl-insert-allValuesFrom ((control ?c) ?a ?v))
      "--"
      ("<oneOf>" owl-insert-oneof ((control ?c) ?o ?o))
      ("<unionOf>" owl-insert-unionof ((control ?c) ?u ?o))
      ("<complementOf>" owl-insert-complementof ((control ?c) ?c ?o))
      ("<intersectionOf>" owl-insert-intersectionof ((control ?c) ?i ?o))
      ("<equivalentClass>" owl-insert-equivalentClass ((control ?c) ?e ?c)) ;; equivalentTo? sameClassAs?
      ("<disjointWith>" owl-insert-disjointWith ((control ?c) ?d ?w))
;;;      ("<disjointUnionOf>" owl-insert-disjointUnionOf ((control ?c) ?d ?u)) ;;

      "--"
      ("<subC/Restriction/onProp>" owl-insert-subclass-restriction-onprop ((control ?c) ?s ?r))
      ("<subC/Res/onP/allValuesFrom>" owl-insert-restriction-allValuesFrom ((control ?c) ?s ?v))
      ("<subC/Res/onP/hasValue>" owl-insert-restriction-hasValue ((control ?c) ?s ?h))
      ("<subC/Res/onP/someValuesFrom>" owl-insert-restriction-someValuesFrom ((control ?c) ?s ?s))
      ("<subC/Res/onProp min/max/cardinality>" owl-insert-restriction-cardinality ((control ?c) ?r ?c))

;; separate cardinality inserters? (used to have them)
      "--"
      ("<Nothing/>" owl-insert-nothing ((control ?c) ?n ?o))
      ("<Thing [ID=|res=]/>" owl-insert-thing ((control ?c) ?t ?h))
       ))


(defvar owl-insert-property-menu nil)

(defvar owl-insert-property-menu-items
    '( ;; in owl-insert
      ("Property" owl-insert-property ((control ?c) ?p ?r))
      ("ObjectProperty" owl-insert-object-property ((control ?c) ?o ?p))
      ("DatatypeProperty" owl-insert-datatype-property ((control ?c) ?d ?p))
      ("SymmetricProperty" owl-insert-symmetricProperty ((control ?c) ?s ?y))
      ("TransitiveProperty" owl-insert-transitiveProperty ((control ?c) ?t ?p))
      ("equivalentProperty" owl-insert-equivalentProperty ((control ?c) ?e ?p))
      ("Functional ObjectProperty" owl-insert-functionalObjectProperty ((control ?c) ?f ?o))
      ("Functional DatatypeProperty" owl-insert-functionalDatatypeProperty ((control ?c) ?f ?d))
      ("InverseFunctionalProperty" owl-insert-inverseFunctionalProperty ((control ?c) ?i ?f))
      ("DeprecatedProperty" owl-insert-deprecatedProperty ((control ?c) ?x ?p))
      ("subPropertyOf" owl-insert-subPropertyOf ((control ?c) ?s ?p))  ;; samePropertyAs
      ("inverseOf" owl-insert-inverseOf ((control ?c) ?i ?v))
      ("domain" owl-insert-domain ((control ?c) ?d ?o))
      ("range" owl-insert-range ((control ?c) ?r ?g))

      ;; someValuesFrom
      ))
;      "--"


;;; note also that c-m-n and c-m-p do reasonable forward tag, backward tag, ignoring
;;; tag type - uses <> like () to move 




(owl-make-menu 'owl-menu "OWL" owl-mode-map "Defines OWL Menu" owl-menu-items)

(owl-make-menu 'owl-insert-menu "Insert" owl-mode-map 
	       "Defines OWL Insert Menu" owl-insert-menu-items)

(owl-make-menu 'owl-insert-class-menu "<Class Expr>" owl-mode-map 
	       "Defines OWL Insert Class Expression Menu" owl-insert-class-menu-items)

(owl-make-menu 'owl-insert-property-menu "<Property Expr>" owl-mode-map 
	       "Defines OWL Insert Property Expression Menu" owl-insert-property-menu-items)

;;; also make <RET> do owl-newline

;;; apparently this works in GNU too:
(define-key owl-mode-map "\r" 'owl-newline) ;; <RET>
(define-key owl-mode-map "\t" 'owl-indent-line) ;; <TAB>

;;; and "\C-c" is C-c and "\e\t" is M-<tab>

;;;================================================================
;;; from define-key help for XEMACS

;  A `key sequence' is a set of keystrokes.  A `keystroke' is a keysym and some
;  set of modifiers (such as control and meta).  A `keysym' is what is printed
;  on the keys on your keyboard.
;
;  A keysym may be represented by a symbol, or (if and only if it is equivalent
;  to an ASCII character in the range 32 - 255) by a character or its equivalent
;  ASCII code.  The `A' key may be represented by the symbol `A', the character
;  `?A', or by the number 65.  The `break' key may be represented only by the
;  symbol `break'.
;
;  A keystroke may be represented by a list: the last element of the list
;  is the key (a symbol, character, or number, as above) and the
;  preceding elements are the symbolic names of modifier keys (control,
;  meta, super, hyper, alt, and shift).  Thus, the sequence control-b is
;  represented by the forms `(control b)', `(control ?b)', and `(control
;  98)'.  A keystroke may also be represented by an event object, as
;  returned by the `next-command-event' and `read-key-sequence'
;  functions.
;
;  Note that in this context, the keystroke `control-b' is *not* represented
;  by the number 2 (the ASCII code for ^B) or the character `?^B'.  See below.
;
;  The `shift' modifier is somewhat of a special case.  You should not (and
;  cannot) use `(meta shift a)' to mean `(meta A)', since for characters that
;  have ASCII equivalents, the state of the shift key is implicit in the
;  keysym (a vs. A).  You also cannot say `(shift =)' to mean `+', as that
;  sort of thing varies from keyboard to keyboard.  The shift modifier is for
;  use only with characters that do not have a second keysym on the same key,
;  such as `backspace' and `tab'.
;
;  A key sequence is a vector of keystrokes.  As a degenerate case, elements
;  of this vector may also be keysyms if they have no modifiers.  That is,
;  the `A' keystroke is represented by all of these forms:
;	  A	?A	65	(A)	(?A)	(65)
;	  [A]	[?A]	[65]	[(A)]	[(?A)]	[(65)]
;
;  the `control-a' keystroke is represented by these forms:
;	  (control A)	(control ?A)	(control 65)
;	  [(control A)]	[(control ?A)]	[(control 65)]
;  the key sequence `control-c control-a' is represented by these forms:
;	  [(control c) (control a)]	[(control ?c) (control ?a)]
;	  [(control 99) (control 65)]	etc.
;
;  Mouse button clicks work just like keypresses: (control button1) means
;  pressing the left mouse button while holding down the control key.
;  [(control c) (shift button3)] means control-c, hold shift, click right.
;
;  Commands may be bound to the mouse-button up-stroke rather than the down-
;  stroke as well.  `button1' means the down-stroke, and `button1up' means the
;  up-stroke.  Different commands may be bound to the up and down strokes,
;  though that is probably not what you want, so be careful.
;
;  For backward compatibility, a key sequence may also be represented by a
;  string.  In this case, it represents the key sequence(s) that would
;  produce that sequence of ASCII characters in a purely ASCII world.  For
;  example, a string containing the ASCII backspace character, "\^H", would
;  represent two key sequences: `(control h)' and `backspace'.  Binding a
;  command to this will actually bind both of those key sequences.  Likewise
;  for the following pairs:
;
;		  control h	backspace
;		  control i   	tab
;		  control m   	return
;		  control j   	linefeed
;		  control [   	escape
;		  control @	control space
;
;  After binding a command to two key sequences with a form like
;
;	  (define-key global-map "\^X\^I" 'command-1)
;
;  it is possible to redefine only one of those sequences like so:
;
;	  (define-key global-map [(control x) (control i)] 'command-2)
;	  (define-key global-map [(control x) tab] 'command-3)
