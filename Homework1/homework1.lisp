; Problem 1.1
((cons 'a 'b) 'c)

; Problem 1.2
(list 1 2 (list 3 4) (cons 'a 'b) 5)

; Problem 2
(defun prob2.0 (arg1 arg2) (cons arg1 arg2))
(defun list5 (arg1 arg2 arg3 arg4 arg5) (list arg1 arg2 arg3 arg4 arg5))
(defun list2 (arg1 arg2) (list arg1 arg2))

; 2.1
(prob2.0 (prob2.0 'a 'b) 'c)

; 2.2
(list5 1 2 (list2 3 4) (prob2.0 'a 'b) 5)

; Problem 3
(defun nest (arg num)
	(setq result arg)
	(dotimes (i num) (setq result (list result)))
	result
)

; Problem 4
(defun find-day-symbol (num)
	(case num
		(1 'Sunday)
		(2 'Monday)
		(3 'Tuesday)
		(4 'Wednesday)
		(5 'Thursday)
		(6 'Friday)
		(7 'Saturday)
		(t :BAD-INPUT)
	)
)

; Problem 5
(defun flatten (l)
	(cond
		((atom l) (list l))
		(t (mapcan #'flatten l))
	)
)