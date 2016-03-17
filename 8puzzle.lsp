(load (merge-pathnames "solvable.lsp" *load-truename*))

(defun 8puzzle (puzzlelist)
	(if (and (validinput puzzlelist 8) (solvable puzzlelist))
		(
			
				;call search functions here with puzzlelist
				;output results from each function
				
				
		)
	(format t "Invalid input. Please check for correct input and that puzzle is solvable.")
	)
)

(defun validinput (puzlist puztype)
	(setf listvalid "T")
	;check for an actual list and correct length
	(if (and (listp puzlist) (= (length puzlist) (1+ puztype)))
		(dotimes (i (1+ puztype) ())
			;check for an actual number, correct range of numbers, and no repeated numbers
			(if (and 
					(numberp (nth i puzlist)) 
					(>= (nth i puzlist) 0) 
					(<= (nth i puzlist) puztype) 
					(not (member (nth i puzlist) (cdr (nthcdr i puzlist)))) ) 
					() (setf listvalid "NIL")
			)
		)
		(setf listvalid "NIL")
	)
	listvalid
)

;(load 'input)

;(load 'BFS)
;load other searches

;enter the program
;(8puzzle (read_start *args*))
;if (length *ARGS*) is 0, the file was loaded in clisp or a filename was not specified, so prompt user for puzzle list
;after the initial load, the 8puzzle funtion can be called with an ordinary puzzlelist
;if (length *ARGS*) is 1, try to open file, if fail, print usage statement
;if (length *ARGS*) is greater than 1, print usage statement