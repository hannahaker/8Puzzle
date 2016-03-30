
(load (merge-pathnames "utilities.lsp" *load-truename*))
(load (merge-pathnames "printstates.lsp" *load-truename*))
(load (merge-pathnames "astar.lsp" *load-truename*))
(load (merge-pathnames "DFID.lsp" *load-truename*))
(load (merge-pathnames "input.lsp" *load-truename*))
(load (merge-pathnames "fileio.lsp" *load-truename*))
(load (merge-pathnames "BFS.lsp" *load-truename*))

(defun 8puzzle (puzzle)
	(if (validinput puzzle 3)
		
		(if (solvable puzzle)
			(progn
				;call search functions here with puzzle
				(BFS puzzle 3)
				(astar puzzle 3)				
				(dfid puzzle 3)
			)
			(format t "Puzzle not solvable.")
		)
		(format t "Invalid input.")
	)
	(values)
)

(defun Npuzzle (puzzle N)
	(if (validinput puzzle N)
	
			(progn
				;call search functions here with puzzle
(			BFS puzzle N)				
				(astar puzzle N)				
				(dfid puzzle N)
			)
			
		(format t "Invalid input.")
	)
	(values)
)

(defun validinput (puzzle puzsize)
	(let ((listvalid) (i))
		(setf listvalid T)
		;check for an actual list and correct length
		(if (and (listp puzzle) (= (length puzzle) (* puzsize puzsize)))
			(dotimes (i (* puzsize puzsize) ())
				;check for an actual number, correct range of numbers, and no repeated numbers
				(if (numberp (nth i puzzle)) 
					(if (and 
						(>= (nth i puzzle) 0) 
						(< (nth i puzzle) (* puzsize puzsize)) 
					(not (member (nth i puzzle) (cdr (nthcdr i puzzle)))) )
					
					() (setf listvalid NIL)
					)
					(setf listvalid NIL)
				)
			)
			(setf listvalid NIL)
		)
		listvalid
		)
	)
	
	
	
	;enter the program
	(8puzzle (input *args*))
	;if (length *ARGS*) is 0, the file was loaded in clisp or a filename was not specified, so prompt user for puzzle list
	;after the initial load, the 8puzzle funtion can be called with an ordinary puzzlelist
	;if (length *ARGS*) is 1, try to open file, if fail, print usage statement
	;if (length *ARGS*) is greater than 1, print usage statement
