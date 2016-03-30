(defun input(args)
	(let ((filename) (puzList))
		(cond
			((= (length args) 1)    
				(setf filename (car args))
				(setf puzList (fileio filename))
			)
			
			
			((= (length args) 0)    
				(format t "You have entered interactive mode. Provide a list of numbers 0-8 in row major order where 0 is the blank for the puzzle.")
				(setf puzList (read))
				(if ( not ( = (length puzList) 9))
					(format t "Please ensure all 9 unique numbers 0- 8 are entered.")
					;reenter state to read user input
					(input args)
				)
				
				;I do not think we need this
				;(parseList puzList)
			)
			; else statement prints usage statement
			(t "trueeeee")
			
		)
	)
)
