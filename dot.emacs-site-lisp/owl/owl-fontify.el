;;; owl-fontify.el --- a part of the simple OWL mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro
;;; updated for OWL by David Rager

;;; This package is designed to be used with XEmacs and Gnu Emacs



;;; faces and font-lock match parameters for OWL/RDF code
;;; requires font-lock.el (normally loaded)

(defgroup owl-faces nil "faces used for OWL mode"  :group 'faces)

;;; GNU requires that the face vars be defined and point to themselves
(defvar owl-keyword-face 'owl-keyword-face 
  "Face to use for OWL keywords like Class, Property.")

(defface owl-keyword-face
    (if in-xemacs-p 
	'((((class color)) (:foreground "black"  :bold t))
	  (t (:foreground "gray" :bold t)))
      ;; in GNU, no bold, so just use color
      '((((class color))(:foreground "black"))))
  "Font Lock mode face used to highlight property names."
  :group 'owl-faces)


;;; used for things like rdf: and owl: 
;;; black, but not bold in xemacs, grey in GNU emacs to downplay them
;;; against the kwds.

(defvar owl-normal-face 'owl-normal-face "regular face")
(defface owl-normal-face
  (if in-xemacs-p     
      '((t (:foreground "black")))
    '((t (:foreground "grey"))))
  "Font Lock mode face used to highlight property names."
  :group 'owl-faces)

(defvar owl-string-face 'owl-string-face "string face")
(defface owl-string-face
    '((t (:foreground "green4")))
  "Font Lock mode face used to highlight strings."
  :group 'owl-faces)


(defvar owl-class-face 'owl-class-face
  "Face to use for OWL Class names in class definitions.")

;; same as function name face
(defface owl-class-face
  (if in-xemacs-p   
      '((((class color)) (:foreground "blue" :bold t))
	(t (:bold t)))
    ;; in GNU, just blue
    '((((class color)) (:foreground "blue"))))
  "Font Lock mode face used to highlight class names in class definitions."
  :group 'owl-faces)

(defvar owl-class-ref-face 'owl-class-ref-face
  "Face to use for OWL Class name references.")

(defface owl-class-ref-face
  '((((class color)) (:foreground "blue" ))
    (t (:bold t)))
  "Font Lock mode face used to highlight class refs."
  :group 'owl-faces)

(defvar owl-property-face 'owl-property-face
  "Face to use for OWL property names in property definitions.")

(defface owl-property-face
  (if in-xemacs-p  
     '((((class color)) (:foreground "darkviolet" :bold t))
       (t (:italic t)))
    ;; in gnu, just magenta
    '((((class color)) (:foreground "darkviolet"))))
     "Font Lock mode face used to highlight property names."
     :group 'owl-faces)

(defvar owl-property-ref-face 'owl-property-ref-face
  "Face to use for OWL property name references.")

(defface owl-property-ref-face
  '((((class color)) (:foreground "darkviolet" ))
    (t (:italic t)))
  "Font Lock mode face used to highlight property refs."
  :group 'owl-faces)

(defvar owl-comment-face 'owl-comment-face
  "Face to use for OWL comments.")

(defface owl-comment-face
  '((((class color) ) (:foreground "red" :italic t))
    (t (:foreground "DimGray" :italic t)))
  "Font Lock mode face used to highlight comments."
  :group 'owl-faces)


(defvar owl-other-face 'owl-other-face
  "Face to use for other keywords.")

(defface owl-other-face
  '((((class color)) (:foreground "peru")))
  "Font Lock mode face used to highlight other OWL keyword."
  :group 'owl-faces)

(defvar owl-tag-face 'owl-tag-face
  "Face to use for tags.")

(defface owl-tag-face
    '((((class color)) (:foreground "violetred" ))
      (t (:foreground "black")))
  "Font Lock mode face used to highlight other untyped tags."
  :group 'owl-faces)


(defvar owl-substitution-face 'owl-substitution-face "face to use for substitution strings")

(defface owl-substitution-face
    '((((class color)) (:foreground "orangered"))
      (t (:foreground "lightgrey")))
  "Face to use for OWL substitutions"
  :group 'owl-faces)



;;;================================================================
;;; these are the regexp matches for highlighting OWL 
;;; forms are (MATCHER (MATCH# FACE-FORM OVERRIDE LAXMATCH)*)
;;; where FACE-FORM is a symbol defining a face, 
;;; override is T | 'KEEP | 'PREPEND | 'APPEND, the latter merge faces in precedence

(defconst owl-font-lock-keywords
  (let ()
    (list 
     ;; owl:Thing and owl:Nothing
     '("<\\(\\|owl:\\)\\(Thing\\|Nothing\\)"
       (1 owl-normal-face t)
       (2 owl-keyword-face t)
       )

     ;; black for the def parts of PROPERTY DEFINITION
     ;; (rdfs | owl):<term> rdf:(ID | about)="class|property"
     ;; <term> = 
     (list 
      (concat "<\\(\\|rdfs?:\\|owl:\\)\\("
	      "\\(\\(Object\\|Datatype\\|Transitive\\|Functional\\|InverseFunctional\\|Symmetric\\|Depreciated\\|\\)Property\\)"
	      "\\|\\(Class\\|Datatype\\|DepreciatedClass\\)\\)"
	      "\\s-+\\(rdf:\\|\\)\\(ID\\|about\\)\\s-*=\\s-*\""
	      "\\([^\"]*\\)\"")

      '(1 owl-normal-face t)
      '(2 owl-keyword-face t)      
      '(6 owl-normal-face t)
      '(7 owl-keyword-face t)
      (list 8 
	    '(if (match-beginning 5) 
		'owl-class-face ; blue
	      'owl-property-face) ; purple
	    t)
      )
     
     ;; rdfs:<term> rdf:resource="Class"...
     ;; <term> == domain | range | subPropertyOf | subClassOf
     (list 
      (concat "<\\(\\|rdfs:\\)" ; 1
	      "\\(domain\\|range\\|subClassOf\\)" ; 2
	      "\\s-+\\(rdf:\\)\\(resource\\)\\s-*=\\s-*\"" ; 3,4
	      "[^\"#]*\\([^\"]*\\)\""
	      )
      '(1 owl-normal-face t)
      '(2 owl-keyword-face t)
      '(3 owl-normal-face t)
      '(4 owl-normal-face t)
      '(5 owl-class-ref-face t t)
      )

     ;; owl:<term> rdf:resource="Class"...
     (list 
      (concat "<\\(\\|owl:\\)" ; 1
	      "\\(allValuesFrom\\|someValuesFrom\\|Class\\|Datatype\\|DepricatedClass\\|"
	      "disjointWith\\|sameAs\\|sameIndividualAs\\|equivalentClass\\|differentFrom\\|"
	      "complementOf\\|intersectionOf\\|hasValue\\)" ; end 2, 5
	      "\\s-+\\(rdf:resource\\)\\s-*=\\s-*\"" ; 6
	      "[^\"#]*\\([^\"]*\\)\""
	      )
      '(1 owl-normal-face t)
      '(2 owl-keyword-face t)
      '(3 owl-normal-face t)
      '(4 owl-class-ref-face t t)
      )


     ;; Property references
     ;; rdfs:<term> rdf:resource="Property"...
     ;; <term> = subPropertyOf
     (list 
      (concat "<\\(\\|rdfs:\\)"
	      "\\(subPropertyOf\\)"
	      "\\s-+\\(rdf:resource\\)\\s-*=\\s-*\""
	      "[^\"#]*\\([^\"]*\\)\"")
      '(1 owl-normal-face t)
      '(2 owl-keyword-face t)
      '(3 owl-normal-face t)
      '(4 owl-property-ref-face t)
      )

     ;; Property references
     ;; owl:<term> rdf:resource="Property"...
     ;; <term> = inverseOf, subPropertyOf, samePropertyAs, onProperty, equivalentProperty
     (list 
      (concat "<\\(\\|owl:\\)"
	      "\\(inverseOf\\|subPropertyOf\\|samePropertyAs\\|onProperty\\|equivalentProperty\\)"
	      "\\s-+\\(rdf:resource\\)\\s-*=\\s-*\""
	      "[^\"#]*\\([^\"]*\\)\"")
      '(1 owl-normal-face t)
      '(2 owl-keyword-face t)
      '(3 owl-normal-face t)
      '(4 owl-property-ref-face t)
      )

	  
     ;; Large-scale structure keywords 
     ;; for now, just rdf:RDF, owl:Ontology, owl:imports
     '("<\\(/\\|\\)\\(rdf:\\|owl:\\|\\)\\(RDF\\|Ontology\\|versionInfo\\|imports\\|priorVersion\\|backwardCompatibleWith\\|incompatibleWith\\)[^>]*>?"
       (1 owl-keyword-face t)
       (2 owl-normal-face t)
       (3 owl-keyword-face t))


     ;; still need these -  for closing tags and starts w/o attributes
     (list 
      (concat "<\\(/\\|\\)\\(\\|owl:\\)"
	      "\\(Class\\|DepreciatedClass\\|DataRange\\|AllDifferent\\|distinctMembers\\|"
	      "\\(Object\\|Datatype\\|Transitive\\|Functional\\|InverseFunctional\\|Symmetric\\|Depreciated\\)Property\\)" )
      '(1 owl-keyword-face t)
      '(2 owl-normal-face t)
      '(3 owl-keyword-face t))

     
     (list 
      (concat "<\\(/\\|\\)\\(\\|rdf:\\)"
	      "\\(Class\\|Datatype\\|Resource\\|List\\|first\\|rest\\|nil\\|Property\\)" )
      '(1 owl-keyword-face t)
      '(2 owl-normal-face t)
      '(3 owl-keyword-face t))




     ;;; rdf:Description, type, value with reference or id
     '("<\\(/\\|\\)\\(rdf:\\)\\(Description\\|type\\|value\\)"
       (1 owl-keyword-face t)
       (2 owl-normal-face t)
       (3 owl-keyword-face t))

     ;;; again, for closing
     (list (concat "<\\(\\|/\\)\\(owl:\\|rdfs:\\)"
		   "\\(seeAlso\\|domain\\|range\\|inverseOf\\|hasValue\\|"
		   "sub\\(Class\\|Property\\)Of\\|"
		   "someValuesFrom\\|allValuesFrom\\|"
		   "disjointWith\\|sameClassAs\\)")
       '(1 owl-keyword-face t)
       '(2 owl-normal-face t)
       '(3 owl-keyword-face t))

     '("<!DOCTYPE" 0 owl-keyword-face t)
;;; in DOCTYPE preface
     '("<\\(!ENTITY\\)[ \t]+\\([^ \t]+\\)[ \t]+\"\\([^\"]*\\)" 
       (1 owl-keyword-face t)
       (2 owl-substitution-face t)
       (3 owl-string-face t) 
       )

     ;; within rdf tag, 
     '("\\(xmlns:?\\)\\([^ \t=]*\\)[ \t]*=" 
       (1 owl-keyword-face t)
       (2 owl-substitution-face t))
 

     
     ;;  owl keywords that usually stand alone or have general range:
     ;;      Restriction intersectionOF disjointUnionOf equivalentTo oneOf 
     ;;      unionOf complementOf inverseOf Disjoint
     ;; and also [min|max]CardinalityQ?  or cardinality
     (list (concat "</?\\(\\|owl:\\)"
		   "\\(\\(Datatype\\|Object\\|\\)Restriction\\|intersectionOf\\|"
		   "sameAs\\|sameIndividualAs\\|differentFrom\\|"
		   "oneOf\\|unionOf\\|complementOf\\|inverseOf\\)")
	   '(1 owl-keyword-face t)
	   '(1 owl-normal-face t)
	   '(2 owl-keyword-face t)
	   )
     '("\\(rdf:\\)\\(parseType\\)\\s-*=\\s-*\"\\([^\"]*\\)\""
       (1 owl-normal-face t)(2 owl-other-face t)(3 owl-other-face t))

     (list (concat
	    "<\\(owl:\\)\\(\\(\\|minC\\|maxC\\|c\\)ardinality\\)\\([^>]*\\)"
	    ">\\s-*\\([0-9]+\\)\\s-*</owl:\\([^>]+\\)>")
	   '(1 owl-normal-face t) 
	   '(2 owl-other-face t)
	   '(4 owl-normal-face t)
	   '(5 owl-other-face t)
	   '(6 owl-other-face t)
	   )



     ;; label rules (both needed?)
     '("<\\(rdfs:\\)\\(label\\)[^>]*>\\([^<]*\\)</\\(rdfs:\\)\\(label\\)>" 
       (1 owl-normal-face t)
       (2 owl-keyword-face t)
       (3 owl-string-face t)
       (4 owl-normal-face t)
       (5 owl-keyword-face t)
       )
     '("<\\(/\\|\\)\\(rdfs:\\)\\(label\\).*>" 
       (1 owl-keyword-face t)
       (2 owl-normal-face t)
       (3 owl-keyword-face t)
       )
     ;; html tag rules
     '("[ \"<>]\\(\\(http\\|file\\|ftp\\|mailto\\):[^ \t\n#>\"]*\\)" 1 owl-tag-face t)



     ;; XML Comments: <!-- ... -->. They traditionally override anything else.
     ;; It's complicated 'cause we won't allow "-->" inside a comment, and
     ;; font-lock colours the *longest* possible match of the regexp.

     '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)"
       1 owl-comment-face t)

     ;; things like <?xml ..?> show up as comments
     '("<[?][^?>]*[?]>"    0 owl-comment-face t)

     ;; RDFS Comments - higlight the tag as we can't cross lines very well.
     '("<\\(\\|rdfs:\\)comment>.*\\(\\|^.*\\)*</\\(\\|rdfs:\\)comment>"
       0 owl-comment-face t) ;; red thru line

     '("</?\\(\\|rdfs:\\)comment.*>"
       0 owl-comment-face t) ;; if on different lines

;;; simple tags and end tags 
     ;; class
     '("</?\\([A-Za-z0-9_]+:\\|\\)\\([A-Z][^ \t\n:>]*[ \t\n]*\\)>" 
       (1 owl-normal-face nil) 
       (2 owl-class-ref-face nil) )

     ;;property
     '("</?\\([A-Za-z0-9_]+:\\|\\)\\([a-z][^ \t\n:>]*[ \t\n]*\\)>" 
       (1 owl-normal-face nil) 
       (2 owl-property-ref-face nil) )


     ;; Any tag, general rule, just after bold/italic stuff.
     ;; FOR NOW USE RULE THAT CHAR AFTER : is Capitalized if class, else property
     ;; a tag with upcase name treated as class ref


;;; tag for property name -- make sure don't overwrite more specific ones above. 

     (list
      (concat
       "<\\([A-Za-z0-9_]+:\\|\\)\\([a-z][^ \t:>]*\\)"
       "\\s-+\\(rdf:\\)\\(resource\\)\\s-*=\\s-*\""
       "\\([^\"]+\\)\"\\s-*\\(/\\|\\)" )

       '(1 owl-normal-face nil)
       '(2 owl-property-ref-face nil)
       '(3 owl-normal-face nil)  
       '(4 owl-keyword-face nil)
       '(5 owl-class-ref-face nil)
       '(6 owl-keyword-face nil)
       )


     (list
      (concat
       "<\\([A-Za-z0-9_]+:\\|\\)\\([A-Z][^ \t:>]*\\)"
       "\\s-+\\(rdf:\\)\\(resource\\|ID\\)\\s-*=\\s-*\""
       "\\([^\"]+\\)\"\\s-*\\(/\\|\\)" )
       '(1 owl-normal-face nil)
       '(2 owl-class-ref-face nil)
       '(3 owl-normal-face nil)  
       '(4 owl-keyword-face nil)
       '(5 owl-class-ref-face nil)
       '(6 owl-keyword-face nil)
      )


;;;; END IGNORE
     
     
     ;; ID or about
;     '("\\(rdf:\\|\\)\\(resource\\|ID\\)[ \t]*=" 
;       (1 owl-normal-face t)
;       (2 owl-keyword-face t))

    ;; in definitions
     '("rdf:\\(\\(ID\\|resource\\|about\\|datatype\\)[ \t]*=\\)" 
       (1 owl-keyword-face nil))


     ;; any other tag
;     '("<\\([^!? ][^ \t>]+\\)" 1 owl-tag-face nil) 

     ;; strings are mostly URIs, so just use this for now, except for the above
     '("=[ \t\n]*\"\\([^\"]+\\)\"" 1 owl-string-face nil) ;; was override= t


     ;; slashes in end tags and empty end tags
     '("<\\(/\\)" 1 owl-keyword-face t)
     '("\\(/\\)>" 1 owl-keyword-face t)
     


     '("&[^;]+;" 0 owl-substitution-face t)

     ;; HTML special characters
     '("&[^;\n]*;" 0 owl-string-face nil)


;;; END OF LIST ELTS
     ))
    "Additional expressions to highlight in OWL mode.")



(put 'owl-mode 'font-lock-defaults '(owl-font-lock-keywords nil nil))

(defun re-font-lock () (interactive) (font-lock-mode 0)(font-lock-mode 1))
