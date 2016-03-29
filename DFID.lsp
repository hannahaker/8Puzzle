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

(defvar goal-states (list '(1 2 3 8 0 4 7 6 5) 						 
						  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0))) 

;globals for search statistics
(defvar nodes-distinct 0)	
(defvar nodes-generated 0)
(defvar nodes-expanded 0)

; Node structure: stores state, parent, and depth.
(defstruct node state parent depth)

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
  Author:		 Dr. John Weiss
  
  Function:		 dfs
  
  Description: 		
	
		This function has been slightly modified from the bfs/dfs search
	located at: http://www.mcs.sdsmt.edu/csc447/Assignments/PA2/search.lsp,
	provided by Dr. John Weiss.
	
		This dfs function performs a depth-first search to find the goal
	state of an N*N tile puzzle using an OPEN and CLOSED list to keep track 
	of states of the puzzle that were expanded (checked).

    Note: 
		See the dfid function which utilizes this function and adds iterative
	deepening. For the tile puzzle, this function alone will not find the path
	to the goal state in a reasonable time. By implementing a bound on this
	dfs search we can utilize the speed of the DFS with the exhaustiveness of
	the BFS.
		
  Parameters: 
	start 	   - A puzzle state, in list form
	depthBound - The depth bound in which to search to.
	N   - The size of the puzzle.
*****************************************************************************|#
(defun dfs (start depthBound N)

	;initialize path variables
	(setf nodes-distinct 0)	 
	(setf nodes-generated 0)
	(setf nodes-expanded 0)
	
	(do*                                                    		 ; note use of sequential DO*
		(                                                   		 ; initialize local loop vars
			(curNode (make-node :state start :parent nil :depth 0))  ; current node: (start nil)
			(OPEN (list curNode))                            		 ; OPEN list:    ((start nil))
			(CLOSED nil)                                    		 ; CLOSED list:  ( )
		)
		
		; termination condition - return solution path when goal is found
		((goal-state? (node-state curNode)) (build-solution curNode CLOSED))
		
		; loop body
		(when (null OPEN) (return nil))             ; no solution
		
		
		; get current node from OPEN, update OPEN and CLOSED
		(setf curNode (car OPEN))
		
		;increment the number of expanded nodes
		(setf nodes-expanded (+ 1 nodes-expanded)) 
		(setf OPEN (cdr OPEN))
		(setf CLOSED (cons curNode CLOSED))
		
		; add successors of current node to OPEN
		(dolist (child (generate-successors (node-state curNode) N))
		
			; for each child node
			(setf child (make-node :state child 
								   :parent (node-state curNode) 
								   :depth (1+ (node-depth curNode))))
								   
			;increment the number of generated nodes
			(setf nodes-generated (+ 1 nodes-generated)) 
			
			; if the node is not on OPEN or CLOSED, and still within bound
			(if (and (<= (node-depth child) depthBound)
				(and (not (member child OPEN   :test #'equal-states))
				(not (member child CLOSED :test #'equal-states))))
				
				(progn
					;increment the number of distinct nodes
					(setf nodes-distinct (+ 1 nodes-distinct)) 
					;add to start of OPEN list
					(setf OPEN (cons child OPEN))
				)
			)
		)
	)
)

#|*****************************************************************************  
  Function:		 dfid
  
  Description: 		
	
		This function utilizes the dfs function contained in this document.
	By adding a bound to the dfs. Utilizing the speed of DFS with the
	completeness of BFS.
	
  Parameters: 
	start 	   - A puzzle state, in list form
	N   - The size of the puzzle.
*****************************************************************************|#
(defun dfid (start N)
	(do 					;initialize local loop variables
		(
			(depthBound 3)	;the static bound to search until.
			(solution '() ) ;an list to hold the solution.
		)
		
		;if the solution list is not empty, we have a solution
		((not (null solution )) solution ) 
		
		;increment the depthBound with each iteration.
		(setf depthBound(+ 1 depthBound) ) 
		
		;the solution will be the list returned from the dfs function.
		(setf solution (dfs start depthBound N))		
	)
)