#|*****************************************************************************  
  Function:		printstates
  
  Description: 		
	
		This function prints the path of a puzzle from the start to the
	goal. The goal path is formatted cleanly in console for easy reading.
	
  Parameters: 
	puzlist 	   - a list of puzzle states.
	N  			   - size of the puzzle.
*****************************************************************************|#
(defun printstates (puzlist N)
	(let ((puzperline) (x) (i) (j) (k) (m))	;declare local looping variables
		(setf puzperline (floor (/ 60 (+ 4 (* 3 N))))) ;set the number of puzzles per "line"
		;loop though the number of lines of puzzles
		(dotimes (i (ceiling (/ (length puzlist) puzperline)) () )
			;loop through the lines of a puzzle
			(dotimes (j N () )
				;loop through the puzzles
				(if (> (* (1+ i) puzperline) (length puzlist))
					(setf m (- (length puzlist) (* i puzperline)))
				(setf m puzperline)) ;set the number of moves per line
				(dotimes (k m () )
					;from 0 to puzzle size
					(dotimes (x N () ) 
						(format t " ~2d " 
							(nth (+ x (* j N))(nth (+ k (* i puzperline)) puzlist )))
					)
						;once a state
						(if (and (= j (floor (/ N 2))) (not (= (length puzlist) (+ 1 k (* i puzperline)))))
							(format t " -> ")
							(format t "    ")
						)
				)
				(format t "~%")
			)
			(format t "~%")
		)
	)
)