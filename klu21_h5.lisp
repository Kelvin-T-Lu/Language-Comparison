;; Kelvin Lu


(defun fib (n) 
	"Calculates the fib sequence of n iterations."
	; If n = 0 or 1, return a number, otherwise calls on fibHelper. 
	(cond
		((= n 0) 0) 
		((= n 1) 1)
		(T (fibHelper 0 1 (- n 1)))
	)
)

(defun fibHelper (x y n)
	"Iterates n times until n = 1. Replaces x and y after each calculations."
	#|
	  If n = 1 - Base case, last operation. 
	;    x+y, the final answer to the fib sequence.
	;    Else recusrively call itself and iterate through sequence.
  |# 
	(if (= n 1)
		(+ x y)
		(fibHelper y (+ x y) (- n 1))
	)
)

(defun reversed (xs)
	"Returns a reversed linked list."
	#| 
	 Iterate through entire list. 
	 Once reached to the last element, append it.
	|# 
	(if (null xs)
		NIL
		(append (reversed (rest xs)) (list (first xs)))
	)
)

(defun is-prime(n)
	"Returns a boolean if a number is prime."
	#| If (n<2) - Base case, by definition of prime numbers.
	    Return NIL
	    Otherwise, call on prime helper.
	 |#  
	(if (< n 2)
		NIL
		(isPrimeHelper n 2)
	)
)

(defun isPrimeHelper (n x)
	"Iterates through a divisor of a number to check if a number is prime."
	(cond
		((>= x n) T) ; Passed all divisor
		((= (mod n x) 0) NIL) ; X is a divisor of n. 
		(T (isPrimeHelper n (+ x 1))) ; Recursive call. 
	)
)

(defun filter (p xs)
	"Definition provided by Mark Snyder"
  (if (null xs) NIL
      (if (funcall p (first xs))
	  (cons (first xs) (filter p (rest xs)))
	  (                 filter p (rest xs)))))


(defun nub (xs)
	"Generates a list with unique elements."
	; Defines two variables head and tails.
	; 	Head is the first element of the list.
	; 	Tail is the rest of the list. 
	; Gets the head of a list, filter out any instance of head in thelist.
	; Recursively call the list for other elements. 
	(let (
			(head (first xs))
			(tail (rest xs))
		)
	  (if (null xs)
		  NIL
		  (cons head (nub (filter (lambda (x) (not (equal x head))) xs))) 
	  )
	)
	; (cons head (filter (not (equal x head)) (nub tail))) 
)

(defun zip-with (f xs ys)
	"Return a function that generates an operation done with 2 elements of seperate list."
	; Seperate the two lists by its head and its tail.
	; If any of the list is empty, return NIL. 
	; If not, perform function f and return its output. 
	; Recursively call the rest of the list until list is empty.  
	(let (
	  	(headx (first xs))
			(heady (first ys))
			(tailx (rest xs))
			(taily (rest ys))
		)
		(cond
			((null xs) NIL)
			((null ys) NIL)
			(T (cons (funcall f headx heady) (zip-with f tailx taily)))
	)
	)
	
)

(defun collatz (n)
	"Perform a sequence of operations based on a given integer."
	; If n == 1, append 1. 
	; If n is even, append the quotient of n/2. 
	; Otherwise, append n*3 + 1 to the list. 
	(cond 
		((= n 1) '(1))
		((evenp n) (append (list n) (collatz (/ n 2))))
		(T (append (list n) (collatz (+ (* n 3) 1))))
	)
)


(defun avg (x y)
	(/ (+ x y) 2.0)
)

(defun median (xs)
	"Return the median of the list."
	#|
		First the function with find the middle index of the list.
		Sort the list.  
		Determines if the length of the list is odd.
			If it's odd, return the middle element and type cast it.
			Otherwise, return the average of the middle two elements. 

	|#
	(let (
		(midIndex (floor (length xs) 2))
		(sortedXS (sort xs #'<)) 
		)
		  (if (oddp (length xs))
  	  	(/ (nth midIndex sortedXS) 1.0) 
  			(avg (nth  midIndex sortedXS) (nth (- midIndex 1) sortedXS))
  		)
	)

)

(defun mode (xs)
	"Return the mode of the list."
	#| 
		Keys - List of all unique elements in the list.
		Calls on mode helper to find the mode. 
	|# 
	(let (
		(keys (nub xs))
		)
	  (modeHelper keys (mapcar (lambda (x) (count x xs)) keys) 0 NIL)
	)
)

(defun modeHelper (keys countList maxCount output)
	"Iterate through a list of counts and return the keys with max count."
	#|
		If currCount is greater than list, create a new list with the new key.
		If element with the same currCount is found, append to list.
		Otherwise, iterate through the list.
		Paramters
			Keys - Unique keys of the list
			CountList - A list with the number of occurences of keys.
			maxCount - The current maxCount of the function.
			Output - The final output list of keys. 
	|#
	(let (
			(currCount (first countList))
			(currKey (first keys))
			(tailCount (rest countList))
			(tailKey (rest keys))
		)
		(cond 
			((null keys) output)
			((> currCount maxCount) (modeHelper tailKey	tailCount currCount (list currKey)))
			((= currCount maxCount) (modeHelper	tailKey tailCount maxCount(append output (list currKey))))
			(T (modeHelper tailKey tailCount maxCount output))
		)
	)
)

(defun check-sudoku (grid)
	(and (checkAllRow grid) (checkAllCol 0 grid) (checkGrp grid 0))
)

(defun sum (xs)
	"Return the sum of a list."
	(apply #'+ xs)
)
(defun checkAllRow (xs)
	(let (
			(element (nub (first xs))) ; Extract the row from xs.
			(tail (rest xs))
		)
		(cond 
			((null xs) T) ; If list is NIL, passes check.
			((and (= (length element) 9) (= (sum element) 45)) (checkAllRow tail)) ; Check next row.
			(T NIL) ; Row fails, return NIL. 
		)
	)
)

(defun checkAllCol (index xs)
	"Returns a boolean if all columns meet specifications"
	; Index - The currrent Index being checked.
	; If index is = 9, all columns are checked.
	; 	Else if columns meet specification, then check next column.
	;   Else return false. 
	( let (
			(element (nub (extractCol index xs))) ; The current row being extracted
		)
		(cond 
			((= index 9) T)
			; If current columns passes test, call the next column.
			((and (= (length element) 9) (= (sum element) 45)) (checkAllCol (+ index 1) xs))
			(T NIL) ; Column failed check.

		)
	)
)

(defun extractCol (index xs)
	"Returns a new list of columns."
	(let (
			(element (nth index (first xs)))
			(tail (rest xs))
		) 
		(cond 
			((null xs) NIL)
			(T (cons element (extractCol index tail)))
		)
	)
)

(defun checkGrp (xs rowIndex)
	"Checks all of the 3x3 groupings of a sudoku."
	; Rowindex - The current block being check. Starting row index of blocks.
	; The starting row index will plus 3, to signify the next vertical block checking.
		(cond 
			((= rowIndex 9) T ) ; All blocks are checked.
				((checkHorizontalBlocks xs rowIndex) (checkGrp xs (+ rowIndex 3))) 
				(T NIL) ; Current block doesn't meet specification.

		)
)

(defun checkHorizontalBlocks (xs rowIndex)
	; Takes in a starting row index, checks the blocks horizontally by changing startCol. 
	(and (checkBlock xs rowIndex 0 0 NIL) 
				(checkBlock xs rowIndex 3 0 NIL)
				(checkBlock xs rowIndex 6 0 NIL)
	)
)

(defun checkBlock (grid startRow startCol currRow output)
	"Check each individual 3x3 block by sudoku."
	; Function will iterate over a span of 3 row. If it iterated 3 times, check the list.
	; If not, add the 3 column values of a row to the output. 
	; The blocks will add input vertically. 
	(let (
			(element (nth (+ startRow currRow) grid)) ; The current row being check.
			(finalOutput (nub output)) ; The final output after a span of 3 rows.
		) 
		( cond
			; If the list iterates over 3 rows, check the current 3x3 block.
			((= currRow 3) (if (and (= (length finalOutput) 9) (= (sum finalOutput) 45))
												T
												NIL

											)
			)
			; Recursively call itself to generate a list from the 3x3.
			(T (checkBlock grid startRow startCol (+ currRow 1) (append output (list (nth startCol element) 
																																							 (nth (+ startCol 1) element)
																																							 (nth (+ startCol 2) element)	
																																					)
																													)
				)
			)

		)
	)	
)

