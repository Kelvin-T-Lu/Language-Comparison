;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author - Mark Snyder
;; STUDENTS: use in the following way:
;;
;; (1) via manual commands:
;;
;;    os-prompt$ clisp -q
;;    [1]> (load "tester5h.lisp")
;;    T
;;    [2]> (main '("YOURFILE.lisp"))
;;    ....
;;
;; (2) via command line options (sbcl provides these, clisp does not...)
;;
;;    os-prompt$ sbcl --load "tester5h.lisp" --eval '(progn (main (list "YOURFILE.lisp")) (sb-ext:quit))'
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *numwrong* 0)
(defvar *haserrored* nil)

;;(declaim (optimize (SPACE 3) (DEBUG 2))) ; to turn on tail-call optimization

(defun run-tests ()
  "this is the prototype; actual definition is at bottom of file."
  NIL
  )

(defun main (args)
  (load (first args))
  (run-tests))

(defmacro with-gensyms-v2 ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defparameter *tests* nil)
;; Macro used to create test functions
(defmacro define-test (function &body in-outs)
  ;; Gensym for the result from the function
  (let ((name (intern
	       (string-upcase
		(format nil "test-~a"
			(string function))))))
    (with-gensyms-v2 (res)
      ;; Define the function with the name test-<FUNCTION>
      `(progn (defun ,name
		  ;; Test function takes no arguments
		  ()
		;; Set up all the function calls and test harnesses
		,@(mapcar
		   (lambda (pair)
		     `(progn
			;; Call the function, store the result
			(let ((,res
			       (handler-case 
				   (apply #',function (list ,@(first pair)))
				 (condition (caught-err)
				   (progn
				     (incf *numwrong*)
				     (setf *haserrored* t)
				     (print caught-err))))))
			  ;; If it is the expected value...
			  (if (equal ,res ,(second pair))
			      ;; Print a PASSED message
			      (setf *haserrored* nil) ; don't print a passed message
			      ;; Otherwise, Show what failed
			      (if *haserrored*
				  (progn
				    (setf *haserrored* nil)
				    (format t "ERROR: Given \"~{~a~^ ~}\" called function coughed and died."
					    (list ,@(first pair)))
				    ())
				  (progn
				    (format t "FAIL: Expected \"~a\" got \"~a\"~%"
					    ,(second pair)
					    ,res)
				    ;; and reduce grade.
				    (incf *numwrong*)))))))
		   ;; in-outs is the list of list pairs that define the test
		   in-outs))
	      (push (list ',name #',name) *tests*)))))
;; (handler-case
;; 	     (funcall fn)
;; 	   (warning (caught-warn)
;; 	     (progn
;; 	       (incf *numwrong*)
;; 	       (print caught-warn)))
;; 	   (error (caught-err)
;; 	     (progn
;; 	       (incf *numwrong*)
;; 	       (print caught-err))))
(defun repeat-character (char len)
  (let ((str (make-array 0 :element-type 'character :adjustable t
			 :fill-pointer t)))
    (loop for i from 0 below len do
	 (vector-push-extend char str))
    str))

(defun run-tests ()
  (setf *numwrong* 0)
  (mapcar
   (lambda (test-pair)
     (destructuring-bind (name fn) test-pair
       (let* ((top-str (format nil "====Running ~a====~%" name))
	      (bot-str (repeat-character #\= (length top-str))))
	 (format t "~a" top-str)
	 (funcall fn)
	 
	 (format t "~a~%" bot-str))))
   *tests*)
  (if (equalp *numwrong* 0) (format t "OK~%")
      (format t "FAILED (failures=~D)" *numwrong*)))

;; Defines a test function called test-resolution


;; 12 tests
(define-test fib
  ((0) 0)
  ((1) 1)
  ((2) 1)
  ((3) 2)
  ((4) 3)
  ((5) 5)
  ((6) 8)
  ((7) 13)
  ((61) 2504730781961)
  ((43) 433494437)
  ((55) 139583862445)
  ((600)110433070572952242346432246767718285942590237357555606380008891875277701705731473925618404421867819924194229142447517901959200))

;; 9 tests
(define-test reversed
  (('()) '())
  (('(5)) '(5))
  (('(2 4 6 8)) '(8 6 4 2))
  (('(True  3  "hat")) '("hat" 3 True))
  (('(1 1 1 2 1 1)) '(1 1 2 1 1 1))
  (('(1 3 5 2 4 6)) '(6 4 2 5 3 1))
  (('(5 5 5 5 5)) '(5 5 5 5 5))
  (('(1.1 2.2 3.3)) '(3.3 2.2 1.1))
  (('(t t nil t nil)) '(nil t nil t t))
  )
  

;; 13 tests
(define-test is-prime
  ((-5) nil)
  ((0) nil)
  ((1) nil)
  ((2) t)
  ((3) t)
  ((4) nil)
  ((5) t)
  ((39) nil)
  ((41) t)
  ((117) nil)
  ((1009) t)
  ((1001) nil)
  ((1117) t)         
  )

;; 12 tests
(define-test nub
  (('()) '())
  (('(5)) '(5))
  (('(13 13 13)) '(13))
  (('(1 2 3 1 2 3 1 2 3)) '(1 2 3))
  (('(1 1 3 2 2 5 5 5 5 4)) '(1 3 2 5 4))
  (('(1 4 2 5 3 1 2 3 4 5 6 1 3 2)) '(1 4 2 5 3 6))
  (('(1 2 3 4 5)) '(1 2 3 4 5))
  (('(-3 -1 1 3 2 4 -2 -4)) '(-3 -1 1 3 2 4 -2 -4))
  (('(1 1 5 1 10 1 1 15 1 1)) '(1 5 10 15))
  (('(1 2 3 2)) '(1 2 3))
  (('(1 2 3 3)) '(1 2 3))
  (('(1 2 3 1)) '(1 2 3)))


;; 14 tests
(let
    ((add (lambda (x y) (+ x y)))
     (mul (lambda (x y) (* x y))))
  ;; Usually you have to use #' (pronounced "sharp quote") to pass a
  ;; function as a value because it is in the function column of the
  ;; symbol table.  Today I didn't wanna bother running one more
  ;; regex, and put it in the value column instead.
  (define-test zip-with
    ((add  '(1 2 3 4) '(10 10 10 10)) '(11 12 13 14))
    ((add  '(1 2 3 4) '(10 10 10 10)) '(11 12 13 14))
    ((add  '(1 2 3 4) '(5 6 7 8)) '(6 8 10 12))
    ((add  '(1 2 3 4) '(5 6 7 8)) '(6 8 10 12))
    ((mul  '(2 3 4)  '(5 5 5 5 5))  '(10 15 20))
    ((mul  '(2 3 4)  '(5 5 5 5 5))  '(10 15 20))
    ((mul  '(2 3 4 5 6 7 8)  '(5 5 5))  '(10 15 20))
    ((mul  '(2 3 4 5 6 7 8)  '(5 5 5))  '(10 15 20))
    ((mul  '()  '(5 5 5 5 5))  '())
    ((mul  '()  '(5 5 5 5 5))  '())
    ((mul  '(2 3 4 5 6 7 8)   '())  '())
    ((mul  '(2 3 4 5 6 7 8)   '())  '())
    ((mul  '(2 3 4 5 6 7 8)   '())  '())
    ((mul  '(2 3 4 5 6 7 8)   '())  '())))

;; 13 tests
(define-test collatz
  ((1) '(1))
  ((3) '(3  10  5  16  8  4  2  1))
  ((4) '(4  2  1))
  ((5) '(5  16  8  4  2  1))
  ((10) '(10  5  16  8  4  2  1))
  ((11) '(11  34  17  52  26  13  40  20  10  5  16  8  4  2  1))
  ((17) '(17  52  26  13  40  20  10  5  16  8  4  2  1))
  ((42) '(42  21  64  32  16  8  4  2  1))
  ((100) '(100  50  25  76  38  19  58  29  88  44  22  11  34  17  52  26  13  40  20  10  5  16  8  4  2  1))
  ((8192) '(8192  4096  2048  1024  512  256  128  64  32  16  8  4  2  1))
  ((2) '(2 1))
  ((6) '(6 3 10 5 16 8 4 2 1))
  ((99) '(99  298  149  448  224  112  56  28  14  7  22  11  34  17  52  26  13  40  20  10  5  16  8  4  2  1)))


;; 8 tests
(define-test median
  (('(457))     457.0)
  (('(1 1 1))     1.0)
  (('(1 2 3))     2.0)
  (('(1 2 2 3))   2.0)
  (('(1 2 3 4))   2.5)
  (('(1 3 2 3 1)) 2.0)
  (('(5 5 5 25))  5.0)
  (('(13 6 13 3 7 29 12 1 2 14)) 9.5)
  )

;; 7 tests
(define-test mode 
  (('(1 1 1))     '(1))
  (('(1 2 3))     '(1 2 3))
  (('(1 2 2 3))   '(2))
  (('(1 2 3 4))   '(1 2 3 4))
  (('(1 3 2 3 1)) '(1 3))
  (('(5 5 5 25))  '(5))
  (('(13 6 13 3 7 29 12 1 2 14)) '(13))
  )


;; 12 tests
(define-test check-sudoku

  (('((1 2 3  4 5 6  7 8 9) 
      (4 5 6  7 8 9  1 2 3) 
      (7 8 9  1 2 3  4 5 6) 
      
      (2 3 4  5 6 7  8 9 1) 
      (5 6 7  8 9 1  2 3 4) 
      (8 9 1  2 3 4  5 6 7) 
      
      (3 4 5  6 7 8  9 1 2) 
      (6 7 8  9 1 2  3 4 5) 
      (9 1 2  3 4 5  6 7 8) 
      )) t)
  (('((1 2 3  4 5 6  7 8 9) 
      (4 5 6  7 8 9  1 2 3) 
      (7 8 9  1 2 3  4 5 6) 

      (1 2 3  4 5 6  7 8 9) 
      (4 5 6  7 8 9  1 2 3) 
      (7 8 9  1 2 3  4 5 6) 

      (1 2 3  4 5 6  7 8 9) 
      (4 5 6  7 8 9  1 2 3) 
      (7 8 9  1 2 3  4 5 6) 
      )) nil)
  (('((1 2 3  1 2 3  1 2 3) 
      (4 5 6  4 5 6  4 5 6) 
      (7 8 9  7 8 9  7 8 9) 
      
      (2 3 4  2 3 4  2 3 4) 
      (5 6 7  5 6 7  5 6 7) 
      (8 9 1  8 9 1  8 9 1) 

      (3 4 5  3 4 5  3 4 5) 
      (6 7 8  6 7 8  6 7 8) 
      (9 1 2  9 1 2  9 1 2) 
      )) nil)

  (('((1 2 3  4 5 6  7 8 9) 
      (2 3 4  5 6 7  8 9 1) 
      (3 4 5  6 7 8  9 1 2) 
      (4 5 6  7 8 9  1 2 3) 
      (5 6 7  8 9 1  2 3 4) 
      (6 7 8  9 1 2  3 4 5) 
      (7 8 9  1 2 3  4 5 6) 
      (8 9 1  2 3 4  5 6 7) 
      (9 1 2  3 4 5  6 7 8) 
      )) nil)

  (('((1 2 3  4 5 6  7 8 9) 
      (4 5 6  7 8 9  1 2 3) 
      (7 8 9  1 2 3  4 5 6) 
      
      (2 3 4  5 6 7  8 9 1) 
      (5 6 7  8 5 1  2 3 4) 
      (8 9 1  2 3 4  5 6 7) 
      
      (3 4 5  6 7 8  9 1 2) 
      (6 7 8  9 1 2  3 4 5) 
      (9 1 2  3 4 5  6 7 8) 
      )) nil)

  (('((1 1 1  1 1 1  1 1 1) 
      (1 1 1  1 1 1  1 1 1) 
      (1 1 1  1 1 1  1 1 1) 
      
      (1 1 1  1 2 3  1 1 1) 
      (1 1 1  4 5 6  1 1 1) 
      (1 1 1  7 8 9  1 1 1) 
      
      (1 1 1  1 1 1  1 1 1) 
      (1 1 1  1 1 1  1 1 1) 
      (1 1 1  1 1 1  1 1 1) 
      )) nil)

  
  (('((2 7 8  4 6 9  1 5 3) 
      (6 9 3  1 2 5  4 7 8) 
      (4 5 1  7 8 3  2 6 9) 
      
      (7 6 5  8 9 4  3 2 1) 
      (3 4 2  6 1 7  9 8 5) 
      (8 1 9  3 5 2  6 4 7) 
      
      (9 8 4  5 3 6  7 1 2) 
      (5 2 6  9 7 1  8 3 4) 
      (1 3 7  2 4 8  5 9 6) 
      )) t)

  (('((7 8 6  4 9 3  2 5 1) 
      (3 2 1  8 7 5  4 9 6) 
      (9 4 5  6 2 1  8 7 3) 
      
      (1 5 7  9 3 8  6 2 4) 
      (2 9 8  5 4 6  3 1 7) 
      (4 6 3  7 1 2  9 8 5) 
      
      (8 1 2  3 6 7  5 4 9) 
      (6 7 4  2 5 9  1 3 8) 
      (5 3 9  1 8 4  7 6 2) 
      )) t)

 
  (('((1 2 3  4 5 6  7 8 9) 
      (4 5 6  7 8 9  1 2 3) 
      (7 8 9  1 2 3  4 5 6) 
      
      (2 3 4  5 6 7  8 9 1) 
      (5 6 7  8 9 1  2 3 4) 
      (8 9 1  2 3 4  5 6 7) 
      
      (3 4 5  6 7 8  9 1 2) 
      (6 7 8  9 1 2  3 4 5) 
      (9 1 2  3 4 5  6 7 8) 
      )) t)
  (('((1 2 3  4 5 6  7 8 9) 
      (4 5 6  7 8 9  1 2 3) 
      (7 8 9  1 2 3  4 5 6) 

      (1 2 3  4 5 6  7 8 9) 
      (4 5 6  7 8 9  1 2 3) 
      (7 8 9  1 2 3  4 5 6) 

      (1 2 3  4 5 6  7 8 9) 
      (4 5 6  7 8 9  1 2 3) 
      (7 8 9  1 2 3  4 5 6) 
      )) nil)
  (('((1 2 3  1 2 3  1 2 3) 
      (4 5 6  4 5 6  4 5 6) 
      (7 8 9  7 8 9  7 8 9) 
      
      (2 3 4  2 3 4  2 3 4) 
      (5 6 7  5 6 7  5 6 7) 
      (8 9 1  8 9 1  8 9 1) 

      (3 4 5  3 4 5  3 4 5) 
      (6 7 8  6 7 8  6 7 8) 
      (9 1 2  9 1 2  9 1 2) 
      )) nil)

  (('((1 2 3  4 5 6  7 8 9) 
      (2 3 4  5 6 7  8 9 1) 
      (3 4 5  6 7 8  9 1 2) 
      (4 5 6  7 8 9  1 2 3) 
      (5 6 7  8 9 1  2 3 4) 
      (6 7 8  9 1 2  3 4 5) 
      (7 8 9  1 2 3  4 5 6) 
      (8 9 1  2 3 4  5 6 7) 
      (9 1 2  3 4 5  6 7 8) 
      )) nil))



;; (load "yourfile.lisp")
;; (run-tests)

