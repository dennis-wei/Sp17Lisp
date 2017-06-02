; 1.1
; ---
(mapcar (lambda (y) (mapcan (lambda (x) (and (not (eq x 'c)) (list x))) y)) '((a b c) (1 2) (10 c) (3 a)))

; 1.2
; ---
(defun tree-remove-if (test l)
  	(cond
  		  ((null l) nil)
        ((atom (car l))
         	  (cond
         		   ((funcall test (car l)) (cons (car NIL)(tree-remove-if test (cdr l))))
            	 (t (cons (car l)(tree-remove-if test (cdr l))))
            )
        )
        (t (cons (tree-remove-if test (car l)) (tree-remove-if test (cdr l))))
    )
)

; 2
; -
(defun partition (test l)  
  (defparameter *equiv-hash* (make-hash-table))
  (loop for x in l do
    (if (gethash (funcall test x) *equiv-hash*)
      (setf (gethash (funcall test x) *equiv-hash*) (nconc (gethash (funcall test x) *equiv-hash*) (list x)))
      (setf (gethash (funcall test x) *equiv-hash*) (list x))
    )
  )
  (setq result '())
  (loop for value being the hash-values of *equiv-hash* do
    (setq result (nconc result (list value)))
  )
  (return-from partition result)
)

; Slightly unfinished, getting the value is incomplete, and due to the way I handle filtering, searching for "function" gets every function

; 3
; -
(defun get-all-symbols(package)
  (let ((lst ()))
   (do-symbols (s (find-package package) lst) (push s lst))
   lst)
)

(defun get-external-symbols(package)
  (let ((lst ()))
   (do-external-symbols (s (find-package package) lst) (push s lst))
   lst)
)

(defun print-apropos(lst)
  (setq alpha_list (sort lst #'string-lessp))
  (mapcar (lambda(x) (format t "~a~%" x)) alpha_list) ()
)

(defun get-symbol-info(sbl)
  (setq name (symbol-name sbl))
  (setq is_func (fboundp sbl))
  (setq postfix
    (cond
      ((null is_func) (concatenate 'string " (value=" (string (symbol-value sbl)) ")"))
      (t " (function)")
    )
  )
  (concatenate 'string name postfix)
)

(defun filter-symbols (symbol_list str)
  (setq filter_list (mapcan (lambda (x) (and (search (string-upcase str) (symbol-name x)) (list x))) symbol_list))
  (mapcar (lambda (x) (get-symbol-info x)) filter_list)
)

(defun my-apropos (str &key (package :common-lisp) (externals-only nil))
  (cond
    ((null externals-only) (print-apropos (filter-symbols (get-external-symbols package) str)))
    (t (print-apropos (filter-symbols (get-all-symbols package) str)))
  )
)



; 4
; -

(defun print-ascii-art (array)
  (dotimes (y (second (array-dimensions array)))
     (dotimes (x (first (array-dimensions array)))
         (princ (aref array x y)))
         (terpri)))

(defun draw-diagonals (array value)
  (let ((w (first (array-dimensions array)))
        (h (second (array-dimensions array))))
    (dotimes (i w)
      (setf (aref array i i) value)
      (setf (aref array i (- w i 1)) value))))

(defun draw-rect (array x y width height value)
  (dotimes (i height)
    (dotimes (j width)
      (setf (aref array (+ x j) (+ y i)) value)
    )
  )
)

;; only really works for odd width
(defun draw-line-centered (array x y radius value)
  (setf (aref array x y) value)
  (dotimes (i radius)
    (setf (aref array (- x i) y) value)
    (setf (aref array (+ x i) y) value)
  )
)

(defun draw-circle (array x y radius value)
    (draw-line-centered array x y radius value)
    (dotimes (i (ceiling (/ radius 2)))
      (draw-line-centered array x (- y i) radius value)
      (draw-line-centered array x (+ y i) radius value)
    )
    (setq i (floor (/ radius 2)))
    (dotimes (j (ceiling (/ radius 2)))
      (draw-line-centered array x (- y i j) (- radius j) value)
      (draw-line-centered array x (+ y i j) (- radius j) value)
    )
)

(defun clear (array value)
  (dotimes (i (first (array-dimensions array)))
    (dotimes (j (second (array-dimensions array)))
      (setf (aref array j i) value)
    )
  )
)

(let (
    (array (make-array (list 19 19) :initial-element #\-))
  )
  (clear array #\.)                  ; bkg
  (draw-diagonals array #\*) ; legs
  (draw-rect array 0 0 19 9 #\|) ; sky
  (draw-circle array 9 8 7 #\+)      ; head
  (draw-circle array 9 8 5 #\space)  ; face
  (draw-rect array 7 7 1 1 #\O)  ; eye
  (draw-rect array 11 7 1 1 #\O) ; eye
  (draw-rect array 7 6 1 1 #\~)  ; eyebrow
  (draw-rect array 11 6 1 1 #\~) ; eyebrow
  (draw-rect array 9 9 1 1 #\V)  ; nose
  (draw-rect array 8 11 3 1 #\~) ; mouth
  (print-ascii-art array)
)

; 5.1
; ---

(defun format-infix (symbol l)
  (setq end-list (mapcar (lambda (x) (concatenate 'string " " symbol " " x)) (cdr l)))
  (setq end-string (apply #'concatenate 'string end-list))
  (concatenate 'string (car l) end-string)
)

(defun to-infix (l) 
  (cond
    ((atom l) (write-to-string l))
    (t
      (setq args (mapcar #' to-infix (cdr l)))
      (if (member nil args) (return nil))
      (setq symbol (string (car l)))
      (if (not (search symbol "+-*/")) (return nil))
      (setq fargs (format-infix symbol args))
      (concatenate 'string "(" fargs ")")
    )
  )
)

; 5.2
; ---
(with-open-file (stream "~/Code/Lisp/Homework2/input.txt")
  (with-open-file (str "~/Code/Lisp/Homework2/output.txt"
    :direction :output
    :if-exists :supersede
    :if-does-not-exist :create)
    
    (do 
      (
        (line (read-line stream nil) (read-line stream nil))
      )
      ((null line))
      (write-line (to-infix (read-from-string line)) str)
    )
  )
)

; 5.3
; ---

(defun lisp-to-infix-converter ()
  (format t "Enter a lisp math expression (or q to quit): ")
  (setq response (read-line))
  (cond
    ((string= response "q") (eval "Goodbye"))
    (t 
      (setq try-infix (ignore-errors (setq output (to-infix (read-from-string response)))))
      (setq try-infix-eval (ignore-errors (eval (read-from-string response))))
      (cond
        ((eq try-infix nil) (format t "Bad Input!~%~%"))
        ((eq try-infix-eval nil) (format t "Infix expression is: ~a~%" output) (format t "Value cannot be computed~%~%"))
        (t
          (format t "Infix expression is: ~a~%" output)
          (format t "Value is: ~a~%~%" (eval (read-from-string response)))
        )
      )
      (lisp-to-infix-converter)
    )
  )
)

