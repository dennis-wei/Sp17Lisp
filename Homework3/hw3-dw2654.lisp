; 1.1
; ---

(defun rec-extreme (l test curr-extreme &key (key #'identity))
	(cond
		((null l)
			curr-extreme
		)
		((funcall test (funcall key (car l)) (funcall key curr-extreme))
			(rec-extreme (cdr l) test (car l))
		)
		(t
			(rec-extreme (cdr l) test curr-extreme :key key)
		)
	)
)

(defun find-extreme (l extreme)
	(cond
		((eq (car l) extreme) l)
		(t
			(find-extreme (cdr l) extreme)
		)
	)
)

(defun get-extreme (l test &key (key #'identity))
	(let ((extreme (rec-extreme l test (car l) :key key)))
	(values extreme (find-extreme l extreme)))
)

; 1.2
; ---


(defun (setf get-extreme) (new-val l test &key (key #'identity))
	(multiple-value-bind (ex subl) (get-extreme l test :key key)
		(let
			((pos (position ex l)) (nv new-val))
			(print (cons nv (cdr subl)))
			(setf (cdr (nthcdr (- pos 1) l)) (cons nv (cdr subl)))
		)
	)
)

; 2.1
; ---

(defun collect-pixels (stream)
	(loop for pixel = (read stream nil :eof)
		until (eq pixel :eof)
		collect pixel
	)
)

(defun format-pixels (width l fl)
	(cond
		((<= (length l) width)
			(concatenate 'list fl (list l))
		)
		(t
			(format-pixels width (nthcdr width l) (concatenate 'list fl (list (subseq l 0 width))))
		)
	)
)

(defun read-pgm (pathname)
	(with-open-file (stream pathname)
		(let
			(
				(temp (read stream))
				(width (read stream))
				(height (read stream))
				(grayscale (read stream))
				(raw-pixels (collect-pixels stream))
			)
			(declare (ignore temp))
			(declare (ignore grayscale))
			(let
				(
					(pixels (format-pixels width raw-pixels ()))
				)
				(make-array (list height width) :element-type '(unsigned-byte 8) :initial-contents pixels)
			)
		)
	)
)

; 2.2
; ---

(defun get-inters (l ret)
	(cond
		((= (length l) 1) ret)
		(t (get-inters (cdr l) (append ret (list (list (first l) (cadr l))))))
	)
)

(defun get-blocks (width height scale)
	(let
		(
			(x-list (loop for i from 0 to (/ width scale) collect (* i scale)))
			(y-list (loop for i from 0 to (/ height scale) collect (* i scale)))
		)
	(let
		(
			(x-inters (get-inters x-list ()))
			(y-inters (get-inters y-list ()))
			(ret ())
		)
		(loop for x-inter in x-inters do
			(loop for y-inter in y-inters do
				(setq ret (append ret (list (list x-inter y-inter))))
			)
		)
		ret
	))
)

(defun get-block-avg (bl arr)
	(let
		(
			(x-start (car (first bl)))
			(x-end (- (cadr (first bl)) 1))
			(y-start (car (cadr bl)))
			(y-end (- (cadr (cadr bl)) 1))
			(s 0)
			(n 0)
		)
		(loop for i from y-start to y-end do
			(loop for j from x-start to x-end do
				(setq s (+ s (aref arr j i)))
				(setq n (+ n 1))
			)
		)
		(values (floor (/ s n)))
	)
)

(defun scale (arr scale)
	(let
		((dims (array-dimensions arr)))
	(let
		((blocks (get-blocks (first dims) (cadr dims) scale)))
		(mapcar (lambda(x) (get-block-avg x arr)) blocks)
	))
)

(defun scale-raster (raster scale-factor)
	(let
		(
			(new-height (/ (first (array-dimensions raster)) scale-factor))
			(new-width (/ (cadr (array-dimensions raster)) scale-factor))
			(raw-pixels (scale raster scale-factor))
		)
		(make-array (list new-height new-width) :element-type '(unsigned-byte 8)
			:initial-contents (format-pixels new-width raw-pixels ())
		)
	)
)

; 2.3
; ---

(defun print-arr (array char-mapping)
	(dotimes (y (first (array-dimensions array)))
		(dotimes (x (second (array-dimensions array)))
			(princ (funcall char-mapping (aref array y x)))
		)
		(terpri)
	)
)

(defun map-ascii (num)
	(cond
		((< num 16) #\space)
		((< num 32) #\.)
		((< num 48) #\:)
		((< num 64) #\")
		((< num 80) #\o)
		((< num 96) #\d)
		((< num 112) #\B)
		((< num 128) #\#)
		((< num 144) #\#)
		((< num 160) #\#)
		((< num 176) #\#)
		((< num 192) #\#)
		((< num 208) #\#)
		((< num 224) #\#)
		((< num 240) #\#)
		((< num 256) #\#)
	)
)

(defun print-raster-to-ascii-art (array char-mapping)
	(print-arr array char-mapping)
)

; 2.4
; ---

; taken from https://rosettacode.org/wiki/Factors-of-an-integer#Common-Lisp
(defun factors (n &aux (lows '()) (highs '()))
  (do ((limit (1+ (isqrt n))) (factor 1 (1+ factor)))
      ((= factor limit)
       (when (= n (* limit limit))
         (push limit highs))
       (remove-duplicates (nreconc lows highs)))
    (multiple-value-bind (quotient remainder) (floor n factor)
      (when (zerop remainder)
        (push factor lows)
        (push quotient highs)))))

(defun get-gcd-factors (dim)
	(let
		((dim-gcd (gcd (first dim) (cadr dim))))
		(factors dim-gcd)
	)
)

(defun get-scale-factor (arr max-width)
	(let
		((dim (array-dimensions arr)))
	(let
		((factor-list (get-gcd-factors dim)) (original-width (second dim)))
		(loop for fact in factor-list do
			(if (<= (/ original-width fact) max-width) (return fact))
		)
	))
)

(defun print-pgm-as-ascii-art (pathname max-output-width)
	(let
		(
			(arr (read-pgm pathname))
		)
		(let
			(
				(factor (get-scale-factor arr max-output-width))
			)
			(print-raster-to-ascii-art (scale-raster arr factor) #'map-ascii)
		)
	)
)

; 3.1
; ---

(defun rec-randomize-order (l rl)
	(cond
		((null l) rl)
		(t
			(let
				((rem (nth (random (length l)) l)))
				(rec-randomize-order (remove rem l) (append rl (list rem)))
			)
		)
	)
)

(defun randomize-order (l)
	(rec-randomize-order l ())
)

; 3.2
; ---

(defmacro progn-rand-static (&rest args)
	`(progn ,@(randomize-order args))
)

(defun foo ()
	(progn-rand-static (princ 1) (princ 2) (princ 3) (princ 4))
)

; 3.3 (Extra Credit)
; ------------------

(defun get-progn-list (args rl)
	(let ((ret ()))
		(loop for i in rl do
			(setq ret (cons (nth (- i 1) args) ret))
		)
		(reverse ret)
	)
)

(defun get-random-list (n)
	(randomize-order
		(loop for i from 1 to n collect i)
	)
)

(defmacro progn-rand (&rest args)
	`(progn ,@(get-progn-list args (get-random-list (length args))))
)

(defun foo ()
	(progn-rand (princ 1) (princ 2) (princ 3) (princ 4))
)

(defun bar ()
	(get-random-list 5)
)

