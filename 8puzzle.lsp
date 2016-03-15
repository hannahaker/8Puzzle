(defun 8puzzle (puzzlelist)
	(cond 
		((validinput puzzlelist)
			(cond 
				((solvable puzzlelist)
				;call search functions here with puzzlelist
				;output results from each function
				)
				(t "Puzzle is not solvable"))
		)
		(t "Invalid input")
	)
)

(defun validinput (puzzlelist)
	;check for correct length
	;check for correct range of numbers
	;check for no repeated numbers
)

(load 'input)
(load 'solvable)
(load 'BFS)
;load other searches

;enter the program
(8puzzle (read_start *args*))
;if (length *ARGS*) is 0, the file was loaded in clisp or a filename was not specified, so prompt user for puzzle list
;after the initial load, the 8puzzle funtion can be called with an ordinary puzzlelist
;if (length *ARGS*) is 1, try to open file, if fail, print usage statement
;if (length *ARGS*) is greater than 1, print usage statement