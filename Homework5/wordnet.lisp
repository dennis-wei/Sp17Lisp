(defpackage "WORDNET"
  (:use :common-lisp)
  (:nicknames "WN")
  (:export "LOAD-WN"
	   "SYNSET"
	   "LOOKUP"
	   "FIND-HYPERNYMS"
	   "FIND-HYPONYMS"
	   ))

(in-package :wn)

;;;================================================================================

(defun listify (x)
  (if (listp x)
      x
    (list x)))

(defun alist-push (alist key newval)
  (let ((cur (assoc key alist)))
    (if cur
	(rplacd cur (cons newval (cdr cur)))
      (push (list key newval) alist))
    alist))

(defun mapappend (function list)
  (loop for i in list
	append (funcall function i)))

(defun split-sequence-by-delimiter (delimiter seq &key (count nil) (remove-empty-subseqs nil) ;
		       (from-end nil) (start 0) (end nil) (test nil test-supplied)
		       (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (if from-end
      (loop for right = end then left
	  for left = (max (or (apply #'position delimiter seq 
				     :end right
				     :from-end t
				     other-keys)
			      -1)
			  (1- start))
	  unless (and (= right (1+ left))
		      remove-empty-subseqs) ; empty subseq we don't want
	  if (and count (>= nr-elts count))
	     ;; We can't take any more. Return now.
	  return (values (nreverse subseqs) right)
	  else 
	  collect (subseq seq (1+ left) right) into subseqs
	  and sum 1 into nr-elts
	  until (< left start)
	  finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
	  for right = (min (or (apply #'position delimiter seq 
				      :start left
				      other-keys)
			       len)
			   end)
	  unless (and (= right left) 
		      remove-empty-subseqs) ; empty subseq we don't want
	  if (and count (>= nr-elts count))
	     ;; We can't take any more. Return now.
	  return (values subseqs left)
	  else
	  collect (subseq seq left right) into subseqs
	  and sum 1 into nr-elts
	  until (>= right end)
	 finally (return (values subseqs right))))))

;;;======================================================================== 


;; copy wordnet data from here: http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz

(defparameter *wordnet-data-dir* "~/Code/Lisp/Homework5/dict/")

(defparameter *synset-relation-types*
  '(:Noun (:antonym :attribute :derivationally-related-form :domain-of-synset-region :domain-of-synset-topic :domain-of-synset-usage
	   :hypernym :hyponym :instance-hypernym :instance-hyponym :instance-hyponym :member-holonym :member-meronym
	   :member-of-this-domain-region :member-of-this-domain-topic :member-of-this-domain-usage :part-holonym
	   :part-meronym :substance-holonym :substance-meronym)
      :verb (:also-see :antonym :attribute :derivationally-related-form
	     :domain-of-synset-region :domain-of-synset-topic :domain-of-synset-usage
	     :participle-of-verb :pertainym :similar-to :verb-group)
      :adjective  (:also-see :antonym :attribute :derivationally-related-form
		   :domain-of-synset-region :domain-of-synset-topic :domain-of-synset-usage
		   :participle-of-verb :pertainym :similar-to)
      :adverb (:antonym :derivationally-related-form :derived-from-adjective
	       :domain-of-synset-region :domain-of-synset-topic :domain-of-synset-usage)))

(defun ptr-symbols-from-strings (pos sym-strings)
  (labels
      ((lookup-symbol (alist)
	 (loop for s in sym-strings
	    collect (or (cdr (assoc s alist :test 'string=))
			(error "WARNING: unknown ptr sym ~a for pos=~a sym-strings=~a" s pos sym-strings)))))
    (case pos

      (:n (lookup-symbol '(("!" . :antonym)
			   ("@" . :hypernym)
			   ("@i" . :instance-hypernym)
			   ("~" . :hyponym)
			   ("i" . :instance-hyponym)
			   ("~i" . :instance-hyponym)
			   ("#m" . :member-holonym)
			   ("#s" . :substance-holonym)
			   ("#p" . :part-holonym)
			   ("%m" . :member-meronym)
			   ("%s" . :substance-meronym)
			   ("%p" . :part-meronym)
			   ("=" . :attribute)
			   ("+" . :derivationally-related-form)
			   (";c" . :domain-of-synset-topic)
			   ("-c" . :member-of-this-domain-topic)
			   (";r" . :domain-of-synset-region)
			   ("-r" . :member-of-this-domain-region)
			   (";u" . :domain-of-synset-usage)
			   ("-u" . :member-of-this-domain-usage )
			   ("\\" . :unknown) ;; new in wn 3.1
			   )))

      (:v (lookup-symbol '(("!" . :antonym)
			   ("@" . :hypernym)
			   ("~" . :hyponym)
			   ("*" . :entailment)                 
			   (">" . :cause)
			   ("^" . :also-see)
			   ("$" . :verb-group)
			   ("+" . :derivationally-related-form)
			   (";c" . :domain-of-synset-topic)
			   (";r" . :domain-of-synset-region)
			   (";u" . :domain-of-synset-usage))))

      ((:a :s) ;; s is adjective satellite
       (lookup-symbol '(("!" . :antonym)
			("&" . :similar-to)
			("<" . :participle-of-verb)
			("\\" . :pertainym)                 
			("=" . :attribute)
			("^" . :also-see)
			("+" . :derivationally-related-form)
			(";c" . :domain-of-synset-topic)
			(";r" . :domain-of-synset-region)
			(";u" . :domain-of-synset-usage))))

      (:r (lookup-symbol '(("!" . :antonym)
			   ("\\" . :derived-from-adjective)                 
			   (";c" . :domain-of-synset-topic)
			   (";r" . :domain-of-synset-region)
			   (";u" . :domain-of-synset-usage)
			   ("+" . :derivationally-related-form))))
      (otherwise
       (error "Unknown POS ~a for ~a" pos sym-strings)))))


;;;======================================================================
;;; wordnet class definitions
;;;======================================================================

(defclass synset ()
  ((index :initarg :index :accessor get-index) ;; used in loading. only unique per POS.
   (words :initarg :words :accessor get-words)
   (related-synsets :initarg :related-synsets :accessor get-related-synsets)
   (definition :initarg :definition :accessor get-definition)
   (cached-hypernyms :initform nil :accessor get-cached-hypernyms)))

(defmethod synset-p ((self synset)) t)
(defmethod synset-p ((self t)) nil)

(defmethod dump-form ((self synset))
  `((:index ,(get-index self))
    (:words ,(get-words self))
    (:related-synsets ,(loop for (rel . synsets) in (get-related-synsets self)
                          when (member rel '(:hypernym :hyponym))
                          append (loop for s in synsets
                                       collect (list rel (get-index s) (get-pos s)))))
    ;; (:definition ,(get-definition self))
    ))

;; (defun dump-wn-binary ()
;;   (let ((*print-length* nil))
;;     (with-open-file (s "/tmp/wn-for-lisp-binary.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
;;       (print `(defparameter *wn-data*
;;                 ',(mapcar #'dump-form (all-synsets)))
;;              s))
;;     (compile-file "/tmp/wn-for-lisp-binary.lisp")
;;     ))

(defun dump-wn ()
  (let ((*print-length* nil)
        (animal (car (lookup "animate being"))))
    (with-open-file (s "/tmp/wn-lisp.out" :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop for i in (all-synsets)
         when (or (member animal
                          (find-all-hypernyms i))
                  (member i (wn::find-all-hypernyms animal))
                  (eq i animal))
         do (print (dump-form i) s)))))
       
    ;; ((:INDEX 2452) (:WORDS ("thing")) (:RELATED-SYNSETS ("thing"))
    ;;  (:DEFINITION ("a separate and self-contained entity")))

(defun load-lisp-wn ()
  (setq *wordnet-data* nil)  
  (clrhash *wordnet-name-hash*)
  (clrhash *wordnet-index-hash*)

  (let* ((entries (with-open-file (s (merge-pathnames "wn-lisp.out" *wordnet-data-dir*) :direction :input)
                    (loop for line-input = (read s nil :done)
                       until (eq line-input :done) collect line-input)))
         (syns (loop for e in entries
                  collect (make-instance 'noun
                                         :index (second (assoc :index e))
                                         :words (second (assoc :words e))
                                         :related-synsets (second (assoc :related-synsets e))
                                         :definition (second (assoc :definition e))))))
    (loop for synset in syns
       do
         (dolist (word (get-words synset))
           (push synset
                 (gethash word *wordnet-name-hash*)))
         (when (gethash (list (get-index synset) :noun) *wordnet-index-hash*)
           (error "index ~a already used! Can't add ~a" 
                  (get-index synset) synset))
         (setf (gethash (list (get-index synset) :noun) *wordnet-index-hash*)
               synset)
         (push synset *wordnet-data*))
    (linkup-wordnet-data)
    ))
;;;--------

(defclass noun (synset)
  ())
(defmethod noun-p ((self noun)) t)
(defmethod noun-p ((self t)) nil)

;;;--------

(defclass adjective (synset)
  ())
(defmethod adjective-p ((self adjective)) t)
(defmethod adjective-p ((self t)) nil)

;;;--------

(defclass adverb (synset)
  ())
(defmethod adverb-p ((self adverb)) t)
(defmethod adverb-p ((self t)) nil)

;;;--------

(defclass verb (synset)
  ())
(defmethod verb-p ((self verb)) t)
(defmethod verb-p ((self t)) nil)

;;;--------

;; we get n, v, a, r from wordnet files directly, so support those
;; also support more mneumonic keywords (:verb, :noun, etc)
(defun find-wordnet-class-name (pos)
  (ecase pos
    ((n :noun) 'noun)
    ((v :verb) 'verb)
    ((a :adjective) 'adjective)
    ((r :adverb) 'adverb)))

(defmethod get-pos ((self noun)) :noun)
(defmethod get-pos ((self verb)) :verb)
(defmethod get-pos ((self adjective)) :adjective)
(defmethod get-pos ((self adverb)) :adverb)

;;;====================================================================================
;;; Wordnet data
;;;====================================================================================

(defparameter *wordnet-hash-size* 400000)
(defvar *wordnet-data* nil) ;; keep raw data around globally for now
(defvar *wordnet-name-hash* (make-hash-table :size *wordnet-hash-size* :test #'equalp))

(defun all-synsets ()
  *wordnet-data*)

(defun lookup (string &optional part-of-speech)
  "return list of possible synsets for given string"
  (assert (member part-of-speech '(:noun :verb :adjective :adverb nil)))
  (when (not *wordnet-data*)
    (error "Wordnet not loaded!"))
  (if part-of-speech
    (loop for i in (gethash string *wordnet-name-hash*)
	when (eq (get-pos i) part-of-speech)
	collect i)
    (gethash string *wordnet-name-hash*)))

(defvar *wordnet-index-hash* (make-hash-table :size *wordnet-hash-size* :test #'equal))
(defun lookup-wordnet-index (index pos)
  (gethash (list index (ecase pos
			 ((:n :noun) :noun)
			 ((:v :verb) :verb)
			 ((:a :adjective) :adjective)
			 ((:r :adverb) :adverb)))
	   *wordnet-index-hash*))


;; format: (i think)
;;   <charpos> <lex-filenum> <pos> <num-words-in-synset> (<word> <???>) <num-relations> @ (<relation data fields>) | gloss
;; 02378415 05 n 01 warhorse 0 004 @ 02377703 n 0000 ~ 02378541 n 0000 ~ 02378625 n 0000 ~ 02378755 n 0000 | horse used in war  
(defun load-wordnet-data (pos dictionary-path)
  (with-open-file (file dictionary-path :direction :input)
    (loop with keyword-package = (find-package :keyword)
       with wordnet-class-for-this-pos = (find-wordnet-class-name pos)
       for item = (read-line file nil nil)
       for item-len = (when item (length item))
       for in-copyright = t then (and in-copyright (char= #\space (schar item 0)))
       until (not item)
       unless in-copyright 
       do
       (destructuring-bind (synset-offset lex-filenum  ss-pos  w-cnt &rest more)
	   (split-sequence-by-delimiter #\space item :end (position #\| item :end item-len))
	 (declare (ignore lex-filenum))
	 (let* ((w-cnt (parse-integer w-cnt :radix 16))
		(words (loop repeat w-cnt
			  for (w) on more by #'cddr
			  collecting w)))
	   (destructuring-bind (p-cnt &rest ptr-info)
	       (subseq more (* 2 w-cnt))
	     (let* ((p-cnt (parse-integer p-cnt))
		    (related-synsets
		     (loop repeat p-cnt
			for (pointer-symbol synset-offset pos nil) on ptr-info by #'cddddr
			unless (string= synset-offset "")
			collecting (let ((rel-type (car (ptr-symbols-from-strings 
							 (intern (string-upcase ss-pos) keyword-package)
							 (list pointer-symbol)))))
				     (list rel-type (parse-integer synset-offset)
					   (intern (string-upcase pos)
						   keyword-package)))))
		    (glosses (let ((bar-pos (position #\| item :end item-len)))
				  (when bar-pos
				    (loop for seg in (split-sequence-by-delimiter
						      #\;
						      (string-trim " " (subseq item (1+ bar-pos) item-len)))
				       collecting (string-trim " \"" seg))))))
	       (let* ((index (parse-integer synset-offset))
		      (synset (make-instance wordnet-class-for-this-pos
					     :index index
					     :words (loop for i in words
						       collect (nsubstitute #\space #\_ i))
					     :related-synsets related-synsets
					     :definition glosses 
					     )))
		 (dolist (word (get-words synset))
		   (push synset
			 (gethash word *wordnet-name-hash*)))
		 (setf (gethash (format nil "~a-~a" pos index) *wordnet-name-hash*)
		       (list synset))
		 (when (gethash (list index pos) *wordnet-index-hash*)
		   (error "index ~a already used! Can't add ~a" 
			  index synset))
		 (setf (gethash (list index pos) *wordnet-index-hash*)
		       synset)
		 (push synset *wordnet-data*)
		 )
	       )))))
    ))

(defun linkup-wordnet-data ()
  (dolist (e *wordnet-data*)
    (let ((old (get-related-synsets e)))
      (setf (get-related-synsets e) nil)
      (loop for (relation index pos) in old
	    for synset = (and pos (lookup-wordnet-index index pos))
	    when synset
	    do (setf (get-related-synsets e)
		     (alist-push (get-related-synsets e) relation synset))))))


(defun initialize-wordnet ()
  (setq *wordnet-data* nil)  
  (clrhash *wordnet-name-hash*)
  (clrhash *wordnet-index-hash*)
  (load-wordnet-data :noun (merge-pathnames "data.noun" *wordnet-data-dir*))
  ;; (load-wordnet-data :verb (merge-pathnames "data.verb" *wordnet-data-dir*))
  ;; (load-wordnet-data :adjective (merge-pathnames "data.adj" *wordnet-data-dir*))
  ;; (load-wordnet-data :adverb (merge-pathnames "data.adv" *wordnet-data-dir*))
  ;; link synsets together using stored indices
  (linkup-wordnet-data)
  )

(defun load-wn (&optional force-reload)
  (when (or force-reload (not *wordnet-data*))
    #+lispworks(load-lisp-wn)
    #+sbcl(initialize-wordnet)))

;;;===========================================================================

(defmethod find-synsets :around ((thing t) &optional pos test)
  (declare (ignore pos test))
  (let ((val (call-next-method)))
    (remove-duplicates val)))

(defmethod find-synsets ((word-or-synset-or-list list) &optional pos test)
  (loop for i in word-or-synset-or-list
        append (find-synsets i pos test)))

(defmethod find-synsets ((word-or-synset-or-list string) &optional pos test)
  (declare (ignore test))
  (lookup word-or-synset-or-list pos))

(defmethod find-synsets ((word-or-synset-or-list synset) &optional pos test)
  (declare (ignore pos test))
  (list word-or-synset-or-list))

;;-----

(defun find-related-synsets (word-or-synset-or-list relation &optional test)
  (remove-duplicates
   (loop for rel in (listify relation)
      append (let ((synsets (find-synsets word-or-synset-or-list :noun)))
               (loop for i in synsets
                  when (or (not test)
                           (funcall test i))
                  append (cdr (assoc rel (get-related-synsets i))))))))


;;;===============================================

;; convenience functions
(defun find-wn-parts (word-or-synset-or-list &optional test)
  (remove-duplicates
   (find-related-synsets word-or-synset-or-list :part-meronym test)))
(defun find-wn-wholes (word-or-synset-or-list &optional test)
  (remove-duplicates
   (find-related-synsets word-or-synset-or-list :part-holonym test)))

(defun find-hypernyms (word-or-synset-or-list &optional test)
  (remove-duplicates
   (append (find-related-synsets word-or-synset-or-list :hypernym test)
           (find-related-synsets word-or-synset-or-list :INSTANCE-HYPERNYM test))))
(defun find-hyponyms (word-or-synset-or-list &optional test)
  (remove-duplicates
   (append (find-related-synsets word-or-synset-or-list :instance-hyponym test)
           (find-related-synsets word-or-synset-or-list :hyponym test))))

(defmethod print-object ((self synset) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream (format nil "~s" (get-words self)))))




	       



