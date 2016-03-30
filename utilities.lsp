#|****************************** GLOBALS ************************************|#

(defvar *goal-states* (list '(1 2 3 8 0 4 7 6 5) 						 
'(1 2 3 4 8 7 6 5 9 10 11 12 0 15 14 13))) 

;globals for search statistics
(defvar *nodes-distinct* 0)	
(defvar *nodes-generated* 0)
(defvar *nodes-expanded* 0)

#|****************************** STRUCTS ***********************************|#
; Node structure: stores state, parent, depth, and f'(n).
(defstruct node state parent depth fn)

#|*****************************************************************************
Authors: Hannah Aker, Derek Lane, Savoy Schuler

Function:		goal-state?

Description: 		

This function simply accepts a list (puzzle state) and compares it to
the declared goal state.

Parameters: 
n1 - a struct of type node
n2 - a struct of type node

Returns: T - If the provided list is identical to the goal state.
NIL - If the provided list is not identical to the goal state.
*****************************************************************************|#
(defun goal-state? (L N) (equal L (nth (- N 3) *goal-states*)))

#|*****************************************************************************
Author: 		Dr. John Weiss

Function:		equal-states

Description: 		

This function is provided at: 
http://www.mcs.sdsmt.edu/csc447/Assignments/PA2/search.lsp

This function tests if two nodes have identical state fields in
the node structure. As parameters is takes two nodes structures, then
it compares the "state" field in each.

Parameters: 
n1 - a struct of type node
n2 - a struct of type node 
*****************************************************************************|#
(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))

#|*****************************************************************************
Author: 		Dr. John Weiss

Function:		build-solution

Description: 		

This function is provided at: 
http://www.mcs.sdsmt.edu/csc447/Assignments/PA2/search.lsp

Build-solution takes a state and a list of (state parent) pairs
and constructs the list of states that led to the current state
by tracing back through the parents to the start node (nil parent).

Parameters: 
node - a struct of type node
node-list - a list of structs of type node 
*****************************************************************************|#
(defun build-solution (node node-list)
	(do
		((path (list (node-state node))))        ; local loop var
		((null (node-parent node)) path)         ; termination condition
		
		; find the parent of the current node
		(setf node (member-state (node-parent node) node-list))
		
		; add it to the path
		(setf path (cons (node-state node) path))
	)
)

#|*****************************************************************************
Author: 		Dr. John Weiss

Function:		member-state

Description: 		

This function is provided at: 
http://www.mcs.sdsmt.edu/csc447/Assignments/PA2/search.lsp

Member-state looks for a node on the node-list with the same state.

Parameters: 
state - a struct of type node
node-list - a list of structs of type node 
*****************************************************************************|#
(defun member-state (state node-list)
	(dolist (node node-list)
		(when (equal state (node-state node)) (return node))
	)
)

#|*****************************************************************************
Authors: Hannah Aker, Derek Lane, Savoy Schuler

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
Authors: Hannah Aker, Derek Lane, Savoy Schuler

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

#|*****************************************************************************
Author: 		Dr. John Weiss

Function:		solvable, disorder

The SOLVABLE function returns T if a given 8-puzzle position is solvable,
NIL otherwise.

Usage:    (solvable L)
where L is a 9-element list such as (1 2 3 8 0 4 7 6 5)

Reference:  "Mathematical Games and Pastimes", p.79-85,
A.P.Domoryad, Macmillan, 1964.
*****************************************************************************|#
(defvar *flag*)

(defun solvable (L)
	(setf *flag* nil)                               ; global *flag*
	(mapcar #'(lambda (elem) (disorder elem L)) L)
	(eq *flag* (evenp (position 0 L)))
)

(defun disorder (elem L)
	(cond
		((eq (car L) elem))
		((> (car L) elem)
			(setf *flag* (not *flag*))
			(disorder elem (cdr L))
		)
		(t (disorder elem (cdr L)))
	)
)

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

