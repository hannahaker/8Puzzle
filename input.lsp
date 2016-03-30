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
			)
			; else statement prints usage statement
			(t "true")
			
		)
	)
)
