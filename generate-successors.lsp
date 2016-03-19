;generate successors
(defun gen (L N) ;pass a puzzle list L and puzzle size N
	
	(let (blankpos x) ;set blankpos = where the zero is in the list()
		
		(setf blankpos (position 0 L :test #'equal))
		
		(setf x '())
	
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
		
		;(break) ;view generated successors by typing x into REPL
		
		(return-from gen x)
	)
)	

;swaps elem1 with elem2 in list L and saves the swap in list M
(defun swap (L elem1 elem2) 

	(let (M)
		(setf M (copy-list L) ) ;copy L into M
		(setf (nth elem1 M) (nth elem2 L) ) ;swap elem2 to elem1 and save in M  
		(setf (nth elem2 M) (nth elem1 L) ) ;swap elem1 to elem2 and save in M
	
		(return-from swap M)
	)
)

	