(in-package :cl-user)

;;;======================================================================== 
;;; utility code
;;;======================================================================== 

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro printing-object (thing stream form)
    `(if *print-readably*
	 (call-next-method)
	 (or (ignore-errors
	       (print-unreadable-object (,thing ,stream :identity t :type t)
		 (format ,stream "~a" ,form))
	       ,thing)
	     (call-next-method))))

  (defmacro if-bound-slots ((slots self) then else)
    `(if (every (lambda (x) (slot-boundp ,self x)) ',slots)
	 (with-slots ,slots ,self
	   ,then)
	 ,else))
  )

;; used by copy
(defun get-class-slots (class)
  (or #+lispworks(clos:class-slots class)
      #+sbcl(sb-mop:class-slots class)
      (error "get-class-slots not implemented")))

;; used by copy
(defun get-slot-name (slot)
  (or #+lispworks(clos:slot-definition-name slot)
      #+sbcl(sb-mop:slot-definition-name slot)
      (error "get-slot-name not implemented")))

;;;======================================================================== 
;;; declare these to avoid warnings when loading 
;;;======================================================================== 

(defgeneric get-width (ob))
(defgeneric get-height (ob))
(defgeneric draw (ob canvas))
(defgeneric get-rgb (ob))
(defgeneric copy (ob &optional top-level))

(defmethod draw ((ob t) (canvas t))
  (error "Not implemented drawing ~a on ~a" ob canvas))

;;;======================================================================== 
;;; code for automatically copying objects
;;;======================================================================== 

;; mixing in class with other classes makes COPY work on those classes
(defclass copy-mixin () ())

;; deep copy of instances that use copy-mixin
(defmethod copy ((ob copy-mixin) &optional (top-level t))
  (declare (ignore top-level))
  (let ((slots (get-class-slots (find-class (type-of ob))))
	(copy (make-instance (type-of ob))))
    (loop for s in slots
       for name = (get-slot-name s)
       when (slot-boundp ob name)
       do
	 (setf (slot-value copy name)
	       (copy (slot-value ob name) nil)))
    copy))

;; deep copy of lists
(defmethod copy ((list list) &optional (top-level t))
  (declare (ignore top-level))
  (loop for i in list
        collect (copy i nil)))

;; shallow copy of array
;; from http://stackoverflow.com/questions/7912232/how-do-you-copy-an-array-in-common-lisp
(defun copy-array-new (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

(defmethod copy ((array array) &optional (top-level t))
  (declare (ignore top-level))
  (copy-array-new array))

;; for other atoms, don't make a copy.
(defmethod copy ((thing t) &optional (top-level t))
  (declare (ignore top-level))
  thing)

;;;======================================================================== 
;;; *** Graphics classes and methods ***
;;;======================================================================== 

(defclass SVG-CANVAS ()
  ((stream :initarg :stream :accessor get-stream)))

(defun make-svg-canvas (stream)
  (make-instance 'svg-canvas :stream stream))

;;;----------

(defclass COLOR (copy-mixin)
     ((red :initform 1.0 :initarg :red :accessor get-red)
      (green :initform 1.0 :initarg :green :accessor get-green)
      (blue :initform 1.0 :initarg :blue :accessor get-blue)))

(defun make-color (&optional (r 1.0) (g 1.0) (b 1.0))
  (make-instance 'color :red r :green g :blue b))

(defmethod get-rgb ((self color))
  (with-slots (red green blue) self
    (values (floor (* red 255)) (floor (* green 255)) (floor (* blue 255)))))

(defmethod print-object ((self color) stream)
  (if-bound-slots ((red green blue) self)
		  (printing-object self stream (format nil "~a ~a ~a" red green blue))
		  (call-next-method)))

;;;---------

(defclass label-mixin (copy-mixin)
  ((label :initform nil :initarg :label :accessor get-label)))

(defmethod draw :after ((self label-mixin) (canvas svg-canvas))
  (with-slots (x-pos y-pos label) self
    (when label
      (format (get-stream canvas) "<text x=~a y=~a>~a</text>~%"
              (write-to-string (+ x-pos (floor (get-width self) 2))) 
              (write-to-string (+ y-pos (get-height self)))
              label))))

;;;----------

(defclass color-mixin (copy-mixin)
  ((color :initarg :color :initform (make-color) :accessor get-color)))

;;;----------

(defclass shape-mixin (copy-mixin)
  ((x-pos :initarg :x :accessor get-x-pos)
   (y-pos :initarg :y :accessor get-y-pos)
   (area :accessor get-area)))

;;;---------

(defclass rectangle (shape-mixin color-mixin label-mixin copy-mixin)
  ((width :initarg :width :accessor get-width)
   (height :initarg :height :accessor get-height)))

(defun make-rectangle (x y w h &key (color (make-color)) label)
  (make-instance 'rectangle :width w :height h :x x :y y
		 :color color :label label))

(defmethod initialize-instance :after ((self rectangle) &key)
  (when (and (slot-boundp self 'width)
	     (slot-boundp self 'height))
    (setf (get-area self)
	  (* (get-width self) (get-height self)))))

(defmethod print-object ((self rectangle) stream)
  (if-bound-slots ((width height x-pos y-pos) self)
		  (printing-object self stream (format nil "(~ax~a) pos=~a,~a" width height x-pos y-pos))
		  (call-next-method)))

(defmethod draw ((ob rectangle) (canvas svg-canvas))
  (with-slots (width height x-pos y-pos color) ob
    (multiple-value-bind (r g b)
	(get-rgb color)
      (format (get-stream canvas) "<rect x=~s y=~s width=~s height=~s "
	      (write-to-string x-pos) (write-to-string y-pos)
	      (write-to-string width) (write-to-string height))
      (format (get-stream canvas) "style=\"fill:rgb(~a,~a,~a);\"/>~%" r g b))))

;;;-----

(defclass circle (shape-mixin color-mixin label-mixin copy-mixin)
  ((radius :initarg :radius :accessor get-radius)))

(defun make-circle (x y r &key (color (make-color)) label)
  (make-instance 'circle :radius r :x x :y y :color color :label label))

(defmethod initialize-instance :after ((self circle) &key)
  (when (slot-boundp self 'radius)
    (setf (get-area self)
	  (* pi (get-radius self) (get-radius self)))))

(defmethod print-object ((self circle) stream)
  (if-bound-slots ((radius x-pos y-pos) self)
		  (printing-object self stream (format nil "(r=~a) pos=~a,~a" radius x-pos y-pos))
		  (call-next-method)))

(defmethod draw ((ob circle) (canvas svg-canvas))
  (with-slots (radius x-pos y-pos color) ob
    (multiple-value-bind (r g b)
	(get-rgb color)
    (format (get-stream canvas) "<circle cx=~s cy=~s r=~s "
	    (write-to-string x-pos) (write-to-string y-pos)
	    (write-to-string radius))
      (format (get-stream canvas) "style=\"fill:rgb(~a,~a,~a);\"/>~%" r g b))))

;;;-----

(defclass ellipse (shape-mixin color-mixin label-mixin copy-mixin)
  ((radius-x :initarg :rx :accessor get-radius-x)
   (radius-y :initarg :ry :accessor get-radius-y)))

(defun make-ellipse (x y rx ry &key (color (make-color)) label)
  (make-instance 'ellipse
		 :rx rx :ry ry
		 :x x :y y
		 :color color :label label))

(defmethod print-object ((self ellipse) stream)
  (if-bound-slots ((radius-x radius-y x-pos y-pos) self)
		  (printing-object self stream (format nil "(r=~ax~a) pos=~a,~a" radius-x radius-y x-pos y-pos))
		  (call-next-method)))

(defmethod draw ((ob ellipse) (canvas svg-canvas))
  (with-slots (radius-x radius-y x-pos y-pos color) ob
    (multiple-value-bind (r g b)
	(get-rgb color)
      (format (get-stream canvas) "<ellipse cx=~s cy=~s rx=~s ry=~s "
	      (write-to-string x-pos) (write-to-string y-pos)
	      (write-to-string radius-x) (write-to-string radius-y))
      (format (get-stream canvas) "style=\"fill:rgb(~a,~a,~a);\"/>~%" r g b))))

(defmethod get-width ((self ellipse))
  (* (get-radius-x self) 2))

(defmethod get-height ((self ellipse))
  (* (get-radius-y self) 2))

;;;-----

(defclass combo-shape (copy-mixin)
  ((shapes :initform nil :initarg :shapes :accessor get-shapes)))

(defmethod print-object ((self combo-shape) stream)
  (if-bound-slots ((shapes) self)
		  (printing-object self stream (format nil "~a elements" (length (get-shapes self))))
		  (call-next-method)))

(defun make-combo-shape (shapes)
  (make-instance 'combo-shape :shapes shapes))

(defmethod draw ((ob combo-shape) (canvas svg-canvas))
  (dolist (sub (get-shapes ob))
    (draw sub canvas)))

;;;======================================================================== 
;;; Face definition and test functions
;;;======================================================================== 

(defparameter *face* (make-combo-shape 
		      `(
			,@(loop for x from 200 to 300 by 10 ;; hair
			     collect (make-rectangle x 90 2 50 :color (make-color 0.0 0.0 0.0)))
			,(make-ellipse 250 250 100 150 :color (make-color 0.9 0.8 .7)) ; :label "head"

			  ;; left eye and eyebrow
			,(make-circle 200 200 20 :color (make-color 0.0 0.0 .5))
			,(make-circle 200 200 10 :color (make-color 1.0 1.0 1.0))
			,(make-circle 200 200 5 :color (make-color 0 0 0))
			,(make-rectangle 180 180 40 3 :color (make-color 0.0 0.0 0.0))

			  ;; right eye and eyebrow
			,(make-circle 260 200 20 :color (make-color 0.0 0.0 .5))
			,(make-circle 260 200 10 :color (make-color 1.0 1.0 1.0))
			,(make-circle 260 200 5 :color (make-color 0 0 0))
			,(make-rectangle 240 180 40 3 :color (make-color 0.0 0.0 0.0))

			  ;; nose
			,(make-ellipse 230 250 10 20 :color (make-color 0.7 0.3 .2))

			  ;; mouth
			,(make-ellipse 230 350 40 10 :color (make-color 0.7 0.3 .2))
			,(make-ellipse 230 350 30 5 :color (make-color 0.9 0.5 .4))
			)))

(defun write-to-svg (file shapes &key (w 600) (h 500))
  (with-open-file (s file :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (let ((svg-canvas (make-svg-canvas s)))
      (format s "<!DOCTYPE html>~%<html>~%<body>~%")

      ;; (format s "<svg width=\"~a\" height=\"~a\">~%" w h) 
      ;; the xmlns makes it work with any extension on the file (rather than just .html)
      (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink= \"http://www.w3.org/1999/xlink\" width=\"~a\" height=\"~a\">~%" w h) 

      (dolist (sh shapes)
	(draw sh svg-canvas))
      (format s "~%</svg>~%")
      (format s "</body>~%</html>~%"))))


(defun test-svg (file)
  (let ((new (copy *face*)))
    (write-to-svg file (list new))))
		  



	 
