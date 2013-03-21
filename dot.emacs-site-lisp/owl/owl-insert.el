;;; owl-insert.el --- a part of the simple OWL mode for Xemacs

;;; (C) 2001 BBN Technologies
;;; by Mark Burstein, with assistance from Ken Anderson and Richard Shapiro
;;; updated for OWL by David Rager

;;; This package is designed to be used with XEmacs and Gnu Emacs

;;; functionality for inserting new expressions into daml/rdf files
(defun empty-p (x)
  (or (null x) (and (stringp x)(string= x ""))))

;; Insert a tag with an attribute and value along with the closing tag.
(defun owl-insert0 (tag &optional attribute value no-end-tag)
  (unless (empty-p tag)
    (unless (owl-empty-line-p)
      (owl-open-new-line-for-insert)) ;; in owl-motion
    (owl-indent-line)
    ;; if no attribute and value then just do a <tag>
    (let ((et (if no-end-tag "/>" ">")))

    (insert (cond ((and (empty-p attribute) (empty-p value))
		   (format "<%s%s"  tag et))
		  ((and attribute value)
		   (format "<%s %s=\"%s\"%s"  tag attribute value et))))
  (unless no-end-tag
    (insert "\n")
    (owl-indent-line)
    (let ((p (point)))
      (insert (concat "\n</" tag ">"))
      (owl-indent-line)
      (goto-char p))))))

;; Insert a tag with a tag attribute/value and content followed by the closing tag 
;; all on one line
(defun owl-insert1 (tag &optional attribute value content)
  (unless (empty-p tag)
    (unless (owl-empty-line-p)
      (owl-open-new-line-for-insert)) ;; in owl-motion
    (owl-indent-line)
    (insert (cond ((and (empty-p attribute) (empty-p value))
		   (format "<%s>"  tag))
		  ((and attribute value)
		   (format "<%s %s=\"%s\">"  tag attribute value))))
    (insert (format "%s</%s>" content tag ))
;    (owl-indent-line)
    ))



;;; functionality for inserting new expressions into owl/rdf files

(defun owl-insert-label (name)
  (interactive "sLabel name: ")
  (owl-indent-line)
  (insert (format "<rdfs:label>%s</rdfs:label>" name))
  (owl-indent-line)
  )
  
;;; owl:Class
(defun owl-insert-class (name)
  (interactive "sClass Name: ")
  (owl-insert0 "owl:Class" "rdf:ID" name)
  (owl-insert-label name)
  )

(defun owl-insert-class-ref (name)
  (interactive "sClass Name: ")
  (owl-insert0 "owl:Class" "rdf:about" name t)
  )


;;; owl:DeprecatedClass
(defun owl-insert-deprecatedClass (id)
  (interactive "sDeprecatedClass Name: ")
  (owl-insert0 "owl:DeprecatedClass" "rdf:ID" id)
;  (owl-insert-label id)
  )



;;; rdfs:subClassOf 
(defun owl-insert-subClassOf (name)
  (interactive "sParent Class Name (ENTER for none): ")
  (if (empty-p name) (setq name nil))
  (owl-insert0 "rdfs:subClassOf" (if name "rdf:resource") name name))


;;; owl:Restriction
(defun owl-insert-restriction (onprop)
  (interactive "sonProperty: ")
  (owl-insert0 "owl:Restriction")
  (owl-insert0 "owl:onProperty" "rdf:resource" onprop t)
  )

;;; rdfs:domain 
(defun owl-insert-domain (name)
  (interactive "sdomain Class Name: ")
  (owl-insert0 "rdfs:domain" "rdf:resource" name t))

;;; rdfs:range 
(defun owl-insert-range (name)
  (interactive "srange Class Name: ")
  (owl-insert0 "rdfs:range" "rdf:resource" name t))

;;; rdf:Property 
(defun owl-insert-property (name domain range)
  (interactive "sProperty Name: \nsDomain Resource (ENTER for none): \nsRange Resource (ENTER for none): ")
  (owl-insert0 "rdf:Property" "rdf:ID" name)
  (owl-insert-label name)
  (when (> (length domain) 0) (owl-insert0 "rdfs:domain" "rdf:resource" domain t))
  (when (> (length range) 0)  (owl-insert0 "rdfs:range" "rdf:resource" range t))
  (next-line 1)
  (end-of-line)
  )

;;; owl:ObjectProperty
(defun owl-insert-object-property (name domain range)
  (interactive "sObjectProperty Name: \nsDomain Resource (ENTER for none): \nsRange Resource (ENTER for none): ")
  (owl-insert0 "owl:ObjectProperty" "rdf:ID" name)
  (owl-insert-label name)
  (when (> (length domain) 0) (owl-insert0 "rdfs:domain" "rdf:resource" domain t))
  (when (> (length range) 0)  (owl-insert0 "rdfs:range" "rdf:resource" range t))
  (next-line 1)
  (end-of-line)
  )

;;; owl:DatatypeProperty
(defun owl-insert-datatype-property (name domain range)
  (interactive "sDatatypeProperty Name: \nsDomain Resource (ENTER for none): \nsRange Resource (ENTER for none): ")
  (owl-insert0 "owl:DatatypeProperty" "rdf:ID" name)
  (owl-insert-label name)
  (when (> (length domain) 0) (owl-insert0 "rdfs:domain" "rdf:resource" domain t))
  (when (> (length range) 0)  (owl-insert0 "rdfs:range" "rdf:resource" range t))
  (next-line 1)
  (end-of-line)
  )

;;; rdfs:subPropertyOf 
(defun owl-insert-subPropertyOf (name)
  (interactive "sParent Property Name: ")
  (owl-insert0 "rdfs:subPropertyOf" "rdf:resource" name t))

;;; owl:SymmetricProperty
(defun owl-insert-symmetricProperty (name domain range)
  (interactive "sSymmetricProperty Name: \nsDomain Resource (ENTER for none): \nsRange Resource (ENTER for none): ")
  (owl-insert0 "owl:SymmetricProperty" "rdf:ID" name)
  (owl-insert-label name)
  (when (> (length domain) 0) (owl-insert0 "rdfs:domain" "rdf:resource" domain t))
  (when (> (length range) 0)  (owl-insert0 "rdfs:range" "rdf:resource" range t))
  (next-line 1)
  (end-of-line)
;;; build a form rather than just add type info
;;;  (owl-insert0 "rdf:type" "rdf:resource" "&owl;#SymmetricProperty" t)
  )

;;; owl:TransitiveProperty
(defun owl-insert-transitiveProperty (name domain range)
  (interactive "sTransitiveProperty Name: \nsDomain Resource (ENTER for none): \nsRange Resource (ENTER for none): ")
  (owl-insert0 "owl:TransitiveProperty" "rdf:ID" name)
  (owl-insert-label name)
  (when (> (length domain) 0) (owl-insert0 "rdfs:domain" "rdf:resource" domain t))
  (when (> (length range) 0)  (owl-insert0 "rdfs:range" "rdf:resource" range t))
  (next-line 1)
  (end-of-line)
;;; build a form rather than just add type info
;;;  (owl-insert0 "rdf:type" "rdf:resource" "&owl;#TransitiveProperty" t)
  )

;;; functional properties can be Object or Datatype Properties. Thus,
;;; it makes sense to do the type declaration as an insert. 
;;; owl:FunctionalProperty is global cardinality=1
(defun owl-insert-functionalDatatypeProperty (name domain range)
  (interactive "sFunctional DatatypeProperty Name: \nsDomain Resource (ENTER for none): \nsRange Resource (ENTER for none): ")
  (owl-insert-datatype-property name domain range)
  (re-search-backward "</rdfs:label>")
  (end-of-line)
  (owl-insert0 "rdf:type" "rdf:resource" "&owl;#FunctionalProperty" t)
  (owl-up-tag-end)
  )

(defun owl-insert-functionalObjectProperty (name domain range)
  (interactive "sFunctional ObjectProperty Name: \nsDomain Resource (ENTER for none): \nsRange Resource (ENTER for none): ")
  (owl-insert-object-property name domain range)
  (re-search-backward "</rdfs:label>")
  (end-of-line)
  (owl-insert0 "rdf:type" "rdf:resource" "&owl;#FunctionalProperty" t)
  (owl-up-tag-end)
  )


;;; owl:InverseFunctionalProperty
(defun owl-insert-inverseFunctionalProperty (name domain range)
  (interactive "sInverseFunctionalProperty Name: \nsDomain Resource (ENTER for none): \nsRange Resource (ENTER for none): ")
  (owl-insert0 "owl:InverseFunctionalProperty" "rdf:ID" name)
  (owl-insert-label name)
  (when (> (length domain) 0) (owl-insert0 "rdfs:domain" "rdf:resource" domain t))
  (when (> (length range) 0)  (owl-insert0 "rdfs:range" "rdf:resource" range t))
  (next-line 1)
  (end-of-line)
;;; build a form rather than just add type info
;;;  (owl-insert0 "rdf:type" "rdf:resource" "&owl;#InverseFunctionalProperty" t)
  )

;;; owl:inverseOf
(defun owl-insert-inverseOf (property)
  (interactive "sinverseOf Property: ")
  (owl-insert0 "owl:inverseOf" "rdf:resource" property t)
  )

;;; rdf:Description rdf:ID=
(defun owl-insert-description-id (name)
  (interactive "sDescription ID: ")
  (owl-insert0 "rdf:Description" "rdf:ID" name)
  )

;;; rdf:Description rdf:about=
(defun owl-insert-description-about (name)
  (interactive "sDescription About(URL): ")
  (owl-insert0 "rdf:Description" "rdf:about" name))

;;; rdfs:comment
(defun owl-insert-comment ()
  (interactive)
  (owl-insert0 "rdfs:comment"))

;;; owl:versionInfo
(defun owl-insert-versionInfo ()
  (interactive)
  (owl-insert0 "owl:versionInfo"))


(defun owl-insert-subclass-restriction-onprop (onprop)
  (interactive "sonProperty: ")
  (owl-insert0 "rdfs:subClassOf")
  (owl-insert0 "owl:Restriction")
  (owl-insert0 "owl:onProperty" "rdf:resource" onprop t)
  )

;;; owl:onProperty
(defun owl-insert-onproperty (prop)
  (interactive "sonProperty: ")
  (owl-insert0 "owl:onProperty" "rdf:resource" prop t)
  )


;; Restriction types:

;;; owl:allValuesFrom
(defun owl-insert-allValuesFrom (class)
  (interactive "sallValuesFrom Class or Datatype: ")
  (owl-insert0 "owl:allValuesFrom" "rdf:resource" class t)
  )

(defun owl-insert-restriction-allValuesFrom (onprop class)
  (interactive "sonProperty: \nsallValuesFrom Class or Datatype: ")
  (owl-insert-subclass-restriction-onprop onprop)
  (owl-insert-allValuesFrom class)
  )

;;; owl:hasValue
;; hasValue (either a instance resource or a datatype)
(defun owl-insert-hasValue (class)
  (interactive "shasValue Resource= (Hit Enter for datatype or expression): ")
  (if (> (length class) 0)
      (owl-insert0 "owl:hasValue" "rdf:resource" class t)
    (owl-insert0 "owl:hasValue"))
  )


;; hasValue (either a instance resource or a datatype)

(defun owl-insert-restriction-hasValue (onprop class)
  (interactive "sonProperty: \nshasValue Resource= (Hit Enter for datatype or expression): ")
  (owl-insert-subclass-restriction-onprop onprop)
  (owl-insert-hasValue class)
  )

;;; owl:someValuesFrom (class expression of Datatype references)
(defun owl-insert-someValuesFrom (class)
  (interactive "ssomeValuesFrom Class or Datatype: ")
  (if (> (length class) 0)
      (owl-insert0 "owl:someValuesFrom" "rdf:resource" class t)
    (owl-insert0 "owl:someValuesFrom")
  ))


(defun owl-insert-restriction-someValuesFrom (onprop class)
  (interactive "sonProperty: \nssomeValuesFrom Class or Datatype: ")
  (owl-insert-subclass-restriction-onprop onprop)
  (owl-insert-someValuesFrom class)
  )

(defun owl-insert-cardinality-restriction (str val)
  (let ((cpos (point))
	pt
	(etag (owl-goto-tag-begin t)))

    (if (char= (car etag) ?>)(search-backward "<"))
    (cond ((looking-at "<owl:Restriction")
	   (when (search-forward-regexp "/?>" nil nil)
	     (goto-char (match-beginning 0))
	     (insert (format " %s=\"%s\"" str val))
	     ))
	  (t (goto-char cpos)
	     (owl-open-new-line-for-insert)
	     (owl-indent-line)
	     (insert "<owl:Restriction %s=\"%s\">" str val)
	     (backward-char)
	     )
	  )))
    
;;; for integers i, j, n: "n" -> n and "i,j" -> (i j)
(defun read-num-pair (numstring)
  (let ((cpos (position ?\, numstring)))
    (if cpos 
	(list (if (eql cpos 0) nil (read (subseq numstring 0 cpos)))
	      (if (eql cpos (- (length numstring) 1)) nil (read (subseq numstring (+ cpos 1)))))
      (read numstring))))


(defun owl-insert-minCardinality (value)
  (interactive "sminCardinality Value: ")
  (owl-insert-cardinality-restriction "owl:minCardinality" value))

(defun owl-insert-maxCardinality (value)
  (interactive "smaxCardinality Value: ")
  (owl-insert-cardinality-restriction "owl:maxCardinality" value))


(defun owl-insert-cardinality-form (ctyp val)
  (owl-insert1 ctyp "rdf:datatype" "&xsd;nonNegativeInteger" val))

(defun owl-insert-cardinality (ints)
  (interactive "sCardinality (or <minC>, or ,<maxC>): ")
  (let* ((card-vals (read-num-pair ints))
	 lab1 v1 v2 ; lab2
	 )
;    (dbg "%s" card-vals)
    (cond ((and (consp card-vals)(not (equal (car card-vals) (second card-vals))))
	   (setq v1 (car card-vals) v2 (second card-vals))
	   (if (and v1 v2) 
	       (if (eql v1 0) 
		   (setq v1 nil) 
		 (message
		  (format "Only one value per restriction - ignoring maxCardinality"))))
	   (cond (v1 (setq lab1 "owl:minCardinality"))
		 (t (setq lab1 "owl:maxCardinality")
		    (setq v1 v2)))
;	   (setq lab2 (if (and v1 v2) "owl:maxCardinality"))
	   )
	  (t (setq lab1 "owl:cardinality" v1 card-vals)))
    (if lab1
	(owl-insert-cardinality-form lab1 v1))
;;; NEW: only one per restriction - ignore rest
;    (if lab2
;	(owl-insert-cardinality-restriction lab2 v2))
    ))

;;; owl:cardinality
(defun owl-insert-restriction-cardinality (onprop ints)
  (interactive "sonProperty: \nscardinality (or <min#>, or ,<max#>): ")
  (owl-insert-subclass-restriction-onprop onprop) ;; does a subclass also 
  (owl-insert-cardinality ints)
  (owl-up-tag-end)
  (owl-up-tag-end)
  )

;;; owl:minCardinality
;(defun owl-insert-restriction-mincardinality (onprop int)
;  (interactive "sonProperty: \nsminCardinality: ")
;  (owl-insert0 "rdfs:subClassOf")
;  (owl-insert0 "owl:Restriction" "owl:minCardinality" int)
;  (owl-insert0 "owl:onProperty" "rdf:resource" onprop t)
;  )

;;; owl:maxCardinality
;(defun owl-insert-restriction-maxcardinality (onprop int)
;  (interactive "sonProperty: \nsmaxCardinality: ")
;  (owl-insert0 "rdfs:subClassOf")
;  (owl-insert0 "owl:Restriction" "owl:maxCardinality" int)
;  (owl-insert0 "owl:onProperty" "rdf:resource" onprop t)
;  )

;;; owl:complementOf
(defun owl-insert-complementof (name)
  (interactive "scomplementOf Name: ")
  (owl-insert0 "owl:complementOf" "rdf:resource" name t))

;;; owl:intersectionOf
(defun owl-insert-intersectionof ()
  (interactive)
  (owl-insert0 "owl:intersectionOf" "rdf:parseType" "Collection" nil))



;;; owl:unionOf
(defun owl-insert-unionof ()
  (interactive)
  (owl-insert0 "owl:unionOf" "rdf:parseType" "Collection" nil))

;;; owl:oneOf
(defun owl-insert-oneof ()
  (interactive)
  (owl-insert0 "owl:oneOf" "rdf:parseType" "Collection" nil))

;;; owl:imports
(defun owl-insert-imports (url)
  (interactive "simports: ")
  (owl-insert0 "owl:imports" "rdf:resource" url t)
  )

;;; owl:sameIndividualAs
(defun owl-insert-sameIndividualAs (resource)
  (interactive "ssameIndividualAs: ")
  (owl-insert0 "owl:sameIndividualAs" "rdf:resource" resource t))

;;; owl:sameAs
(defun owl-insert-sameAs (resource)
  (interactive "ssameAs: ")
  (owl-insert0 "owl:sameAs" "rdf:resource" resource t))

;;; owl:differentFrom
(defun owl-insert-differentFrom (resource)
  (interactive "sdifferentFrom: ")
  (owl-insert0 "owl:differentFrom" "rdf:resource" resource t))

;;; owl:equivalentClass
(defun owl-insert-equivalentClass (resource)
  (interactive "sEquivalentClass: ")
  (owl-insert0 "owl:equivalentClass" "rdf:resource" resource t))

;;; owl:equivalentProperty
(defun owl-insert-equivalentProperty (resource)
  (interactive "sEquivalentProperty: ")
  (owl-insert0 "owl:equivalentProperty" "rdf:resource" resource t))

;;; owl:AllDifferent
;;; owl:distinctMembers
(defun owl-insert-allDifferent ()
  (interactive)
  (owl-insert0 "owl:AllDifferent")
  (owl-insert0 "owl:distinctMembers"))

;;; owl:DataRange 
(defun owl-insert-dataRange ()
  (interactive)
  (owl-insert0 "owl:DataRange")
  (owl-insert0 "owl:oneOf"))

;;; owl:DeprecatedProperty
(defun owl-insert-deprecatedProperty (id)
  (interactive "sDeprecatedProperty Name: ")
  (owl-insert0 "owl:DeprecatedProperty" "rdf:ID" id)
;  (owl-insert-label id)
  )

;;; owl:Nothing
;;; insert <owl:Nothing/>
(defun owl-insert-nothing ()
  (interactive)
  (owl-insert0 "owl:Nothing" nil nil t)) ;; tag attrib value no-end-tag?

;;; owl:Thing
;;; insert <owl:Thing rdf:ID="name"/>
(defun owl-insert-thing (name)
  (interactive "sname (=<IDname> or <aboutname> or ENTER for none):")
  (if (empty-p name) (setq name nil))
  (let ((id (if (and name (char= (elt name 0) ?=)) (subseq name 1))))
    (owl-insert0 "owl:Thing" (if id "rdf:ID" (if name "rdf:about"))
		 (or id name) t)))

;;; owl:Ontology
(defun owl-insert-ontology (name)
  (interactive "srdf:about= ")
  (owl-insert0 "owl:Ontology" "rdf:about" name))

;;; owl:backwardCompatibleWith
(defun owl-insert-backwardCompatibleWith (resource)
  (interactive "sOntology: ")
  (owl-insert0 "owl:backwardCompatibleWith" "rdf:resource" resource t))

;;; owl:incompatibleWith
(defun owl-insert-incompatibleWith (resource)
  (interactive "sOntology: ")
  (owl-insert0 "owl:incompatibleWith" "rdf:resource" resource t))

;;; owl:disjointWith
(defun owl-insert-disjointWith (class)
  (interactive "sClass: ")
  (owl-insert0 "owl:disjointWith" "rdf:resource" class t))

;;; owl:priorVersion
(defun owl-insert-priorVersion (resource)
  (interactive "sOntology: ")
  (owl-insert0 "owl:priorVersion" "rdf:resource" resource t))

;;; rdf:List
;;; rdf:first
;;; rdf:rest
(defun owl-insert-List ()
  (interactive)
  (owl-insert0 "rdf:List")
  (owl-insert0 "rdf:first")
  (let ((p (point)))
    (next-line 1)
    (end-of-line)
    (owl-insert0 "rdf:rest")
    (goto-char p)))

;;; added by DER
(defun owl-insert-list2 (resource)
  (interactive "sResource: ")
  (owl-insert0 "rdf:List")
  (owl-insert0 "rdf:first" "rdf:resource" resource t)
  (owl-insert0 "rdf:rest"))


;;; rdf:nil
(defun owl-insert-nil ()
  (interactive)
  (owl-insert0 "rdf:nil" "" "" t))

(defun owl-build-type-from-input (typestr)
  (let ((char (elt typestr 0)))
    (cond ((char= char ?\;) (concat "&xsd" typestr))
	  ((char-equal char ?I) "&xsd;integer")
	  ((char-equal char ?S) "&xsd;string")
	  ((char-equal char ?.) "&xsd;decimal")
	  ((char-equal char ?N) "&xsd;nonNegInteger")
	  ((char-equal char ?B) "&xsd;boolean")
	  ((char-equal char ?D) "&xsd;date")
	  ((char-equal char ?T) "&xsd;time")
	  ((char-equal char ?E) "&xsd;dateTime"))))
	  

;;; insert rdf:datatype="" in the current/previous tag
(defun owl-insert-rdf-datatype (type)
  (interactive "s[I]nt|[N]onNeg|[S]tr|[.]decimal|[B]ool|[D]ate|[T]ime|[E]dateTime|;<specified>|ENTER: ")
  (let ((typestr (owl-build-type-from-input type))
	p
	(etag (owl-goto-tag-begin t)))
    (if (char= (car etag) ?>)
	(search-backward-regexp "/?>")
      (search-forward-regexp "/?>")
      )
    (goto-char (match-beginning 0))
    (insert " rdf:datatype=\"") 
    (setq p (point))
    (insert (format "%s\"" typestr))
    (goto-char p)
    ))


(defun owl-insert-rdf-resource ()
  (interactive)
  (let (p
	(etag (owl-goto-tag-begin t)))
    (if (char= (car etag) ?>)
	(search-backward-regexp "/?>")
      (search-forward-regexp "/?>")
      )
    (goto-char (match-beginning 0))
    (insert " rdf:resource=\"") 
    (setq p (point))
    (insert "\"")
    (goto-char p)
    ))


(defun owl-insert-tag (name)
  (interactive "sTag Label:")
  (let ((res (read-input "rdf:resource= (ENTER for none): "))
	)
    (if (empty-p res) (setq res nil))
;    (dbg "res=%s" res)
    ;; last arg - if res then do empty end />

    (owl-insert0 name (if res "rdf:resource") res 
		 (or res 
		     (not (empty-p
			   (read-input "<Self-ending/>? (Y or ENTER for no)")))))))


(defun owl-insert-id-tag (name)
  (interactive "sTag Label:")
  (let ((res (read-input "(ENTER for none) rdf:ID=")))
    (if (empty-p res) (setq res nil))
;    (dbg "res=%s" res)
    (owl-insert0 name (if res "rdf:ID") res 
		     (not (empty-p
			   (read-input "<Self-ending/>? (Y or ENTER for no)")))
		     )))


(defun owl-insert-about-tag (name)
  (interactive "sTag Label:")
  (let ((res (read-input "(ENTER for none) rdf:about=")))
    (if (empty-p res) (setq res nil))
;    (dbg "res=%s" res)
    (owl-insert0 name (if res "rdf:about") res 
		     (not (empty-p
			   (read-input "<Self-ending/>? (Y or ENTER for no)")))
		     )))



;;; if inside a begin tag, make do />
;;; else open a line and insert end corresponding to prev unclosed begin.
(defun owl-insert-end-tag ()
  (interactive)
  (let ((curpos (point))
	(beg (owl-goto-tag-begin t))
	tag
	)
;    (dbg "beg=%s point %s" beg (point))
    (cond ((char= (car beg) ?>) ;; we were after a tag
	   (when (and (owl-up-tag)
		      (looking-at "<\\([^/][^ \t\n>]*\\)"))
	     (setq tag (buffer-substring
			(match-beginning 1)
			(match-end 1)))
	     (goto-char curpos)
	     ;; unless at beginning of line or (owl-empty-line-p)
	     (unless (or (looking-at "^.*") (owl-empty-line-p))
	       (insert "\n"))
	     (insert (format "</%s>" tag))
	     ;; DER - only insert line if the tag is not at the end of the line.
	     (unless (looking-at "$")
	       (open-line 1))
	     (owl-indent-line)
	     ;; DER - I don't like being at the beginning of the tag I just inserted.
	     ;; This moves to the end of the tag.
	     (owl-search-fwd ">")(forward-char 1)
	     ))
	  ;; else, in a tag so found its beginning
	  ;;  if in an end tag, go past it and try again
	  ((looking-at "</")
	   (when (owl-search-fwd ">")
	     (forward-char 1)
	     (owl-insert-end-tag)))
	  ;; if a begin tag, see if it has
	  ;; a close bracket, and put slash before it
	  (t 
	   (forward-char 1) ;; past <
	     (setq curpos (point))
	     (owl-search-fwd "[<>]")
	     (backward-char 1)
	     (if (looking-at ">")
		 (insert "/")
	       (goto-char curpos)
	       (end-of-line)
	       (insert "/>"))))))


