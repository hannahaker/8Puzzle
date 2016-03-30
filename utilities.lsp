#|*****************************************************************************
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
(defun goal-state? (L) (equal L '(1 2 3 8 0 4 7 6 5)))

(defvar *goal-states* (list '(1 2 3 8 0 4 7 6 5) 						 
						  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0))) 

;globals for search statistics
(defvar *nodes-distinct* 0)	
(defvar *nodes-generated* 0)
(defvar *nodes-expanded* 0)

; Node structure: stores state, parent, depth, and f'(n).
(defstruct node state parent depth fn)

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
