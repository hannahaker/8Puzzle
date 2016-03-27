#|*****************************************************************************
  Function:		generate-successors
  
  Description: 		
	
		This function accepts a list (puzzle state) of size N*N 
	and returns a list of lists (puzzle states) that describe
	each possible move. Seperated into four parts:
				
	move right - Check if the blank position % N + 1 is less than N
	move left - Check if the blank position % N is greater than N
	move down - Check if the blank position + N is less than N^2
	move up - 
				
  Parameters: 
	L - a list (puzzle state)
	N - The size of the puzzle (N*N)
*****************************************************************************|#
(defun generate-successors (L N) 
	
	(let (blankpos x) ;set blankpos = where the zero is in the list()
		
		(setf blankpos (position 0 L :test #'equal))
		
		(setf x '()) ;an empty list that will contain generated states
	
		;generate a successor if empty space can move right
		;if (blankpos % N) + 1 < N
		(when (< (+ (mod blankpos N) 1) N)   
			;make list with blankpos and blankpos+1 swapped.
			(setf x (cons (swap L blankpos (+ blankpos 1) ) x ) ) 
		) 
		
		;generate a successor if empty space can move left
		;if blankpos % N > 0
	 	(when (> (mod blankpos N) 0) 
				;make list with blankpos and blankpos-1 swapped.
				(setf x (cons (swap L blankpos (- blankpos 1) ) x ) )
		)

		;generate a successor if empty space can move down
		;if blankpos + N < N^2
	    (when (< (+ blankpos N) (* N N) )
				;make list with blankpos and blankpos+N swapped.
				(setf x (cons (swap L blankpos (+ blankpos N) ) x ) )
		)

		;generate a successor if empty space can move up
		;if blankpos - N >= 0
	    (when (>= (- blankpos N) 0)
				;make list with blankpos and blankpos-N swapped.
				(setf x (cons (swap L blankpos (- blankpos N) ) x ) )
		)
		
		(return-from generate-successors x)
	)
)	

#|*****************************************************************************
  Function:		swap
  
  Description: 		
	
		This function accepts a list and two elements to be swapped.
	A temporary list (M) is used to return the results.
				
  Parameters: 
	L - a list (puzzle state)
	elem1 - the atom of a list to be swapped with elem2.
	elem2 - the atom of a list to be swapped with elem1.
*****************************************************************************|#
(defun swap (L elem1 elem2) 

	(let (M)
		(setf M (copy-list L) ) ;copy L into M
		(setf (nth elem1 M) (nth elem2 L) ) ;swap elem2 to elem1 and save in M  
		(setf (nth elem2 M) (nth elem1 L) ) ;swap elem1 to elem2 and save in M
	
		(return-from swap M) ;return a list with elem1 and elem2 swapped.
	)
)