; 1.1
; ---

(loop for x from 30 to 50 collect (code-char x))

(loop for x from 0 below 100 sum (if (zerop (mod x 5)) (expt x 2) 0))

(mapc (lambda(x)
	(format t "~%~a: ~a" (+ (car x) 5) (second x)))
	'((1 a) (2 b) (3 c) (4 d) (5 e) (6 f)))

; 1.2
; ---

(defun str-append (str-list &optional (separator ""))
	(reduce (lambda (x y) (concatenate 'string x separator y)) str-list))

; 2.1
; ---

(defun wn::flatten (thing)
  (if (consp thing)
      (loop for i in thing
          append (if (consp i)
                     (wn::flatten i)
                   (list i)))
    thing))

(defun wn::add-res (res acc)
	(let ((flat-res (wn::flatten res)))
		(concatenate 'list acc flat-res)))

(defun wn::hypernyms-driver (net acc)                                                           
	(let ((res (wn::find-hypernyms net)))                                                            
		(if res                                                                                  
			(let ((new_acc (wn::add-res res acc)))                                             
				(wn::hypernyms-driver res new_acc))                                    
			acc)))

(defmethod wn::find-all-hypernyms (init-word)
	(wn::hypernyms-driver init-word ()))

; 2.2
; ---

(defclass wn::synset ()
  ((index :initarg :index :accessor get-index) ;; used in loading. only unique per POS.
   (words :initarg :words :accessor get-words)
   (related-synsets :initarg :related-synsets :accessor get-related-synsets)
   (definition :initarg :definition :accessor get-definition)
   (cached-hypernyms :initform nil :accessor get-cached-hypernyms)))

(defun wn::is-sysnet (o)
	(if (equal (type-of o) 'CONS) T NIL))

(defun wn::lookup-if-needed (o)
	(if (wn::is-sysnet o) o (wn::lookup o)))

(defmethod wn::find-all-hypernyms (init)
	(let ((arg (wn::lookup-if-needed init)))
		(if (wn::get-cached-hypernyms (car arg)) (wn::get-cached-hypernyms (car arg))
			(let ((ret (wn::hypernyms-driver arg ())))
				(setf (wn::get-cached-hypernyms (car arg)) ret) ret))))

; 3.1
; ---

(defun split-lists (input)
	(let ((n (/ (list-length input) 2)))
		(list (subseq input 0 n) (nthcdr n input))))

; pulled from http://lisp.plasticki.com/show?E97
(defun interleave (l1 l2)
  (if (null l1) nil
    (cons (first l1)
          (cons (first l2)
                (interleave (rest l1) (rest l2))))))

(defun outshuffle (input)
	(let ((lists (split-lists input)))
		(interleave (car lists) (second lists))))

(defun inshuffle (input)
	(let ((lists (split-lists input)))
		(interleave (second lists) (car lists))))

(defun determine-shuffle (input shuffle-op)
	(if (= shuffle-op 1) (inshuffle input) (outshuffle input)))

(defun faro-shuffle (input shuffle-ops)
	(let ((result input))
		(mapc (lambda (x) (setq result (determine-shuffle result x))) shuffle-ops) result))

; 3.2
; ---
; As above, indicate outshuffle with 0 and inshuffle with 1
; Hard stops at 1000000 in case

(defun recycle-driver (shuf input orig acc)
	(if (or (equal input orig) (> acc 1000000)) acc 
		(recycle-driver shuf (funcall shuf input) orig (+ acc 1))))

(defun find-recycle-length (input type)
	(let
		((shuf (if (= 0 type) #'outshuffle #'inshuffle)))
		(recycle-driver shuf (funcall shuf input) input 1)))

