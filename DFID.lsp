#|
                    ***** SEARCH.LSP *****

General-purpose exhaustive search routine includes both breadth-first
search and depth-first search. Uses graph search with OPEN and CLOSED
lists rather than tree search, to avoid cycles. Does not use heuristics
to limit or guide search.

To solve a specific problem, the functions "generate-successors" and
"goal-state" must be defined. "Generate-successors" takes a state as its
argument and returns a list of child states. "Goal-state?" returns T if
its argument is a goal state, NIL otherwise.

In order to retrace a solution path, nodes are stored as (state parent)
pairs, where "state" is the current state and "parent" is the parent
state. Given a goal node, a solution path is generated by simply tracing
backwards through the parent states.

Author: Dr. John M. Weiss, Ph.D.
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#

;--------------------------------------------------------------------------
(defun goal-state? (L) (equal L '(1 2 3 8 0 4 7 6 5))) ;ASK ABOUT N GOAL STATES

; Node structure: stores state and parent.
(defstruct node state parent depth)

; Test if two nodes have the same state.
(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))

;--------------------------------------------------------------------------

; Given a start state and a search type (BFS or DFS), return a path from the start to the goal.
(defun dfs (start depthBound puzzsize)
    (do*                                                    ; note use of sequential DO*
        (                                                   ; initialize local loop vars
            (curNode (make-node :state start :parent nil :depth 0))  ; current node: (start nil)
            (OPEN (list curNode))                           ; OPEN list:    ((start nil))
            (CLOSED nil)                                    ; CLOSED list:  ( )
        )

        ; termination condition - return solution path when goal is found
        ((goal-state? (node-state curNode)) (build-solution curNode CLOSED))
		
        ; loop body
        (when (null OPEN) (return nil))             ; no solution

		
        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))

        ; add successors of current node to OPEN
        (dolist (child (generate-successors (node-state curNode) puzzsize))
            ; for each child node
            (setf child (make-node :state child :parent (node-state curNode) :depth (1+ (node-depth curNode))))
			
			
			
            ; if the node is not on OPEN or CLOSED
            (if (and (<= (node-depth child) depthBound)
						(and (not (member child OPEN   :test #'equal-states))
						(not (member child CLOSED :test #'equal-states))))
					 
                    ; DFS - add to start of OPEN list (stack)
                (setf OPEN (cons child OPEN))
				
				;BREAdth first search
				;(setf OPEN (append OPEN (list child)))
			)
        )
    )
)

(defun dfid (start puzzsize)
	(do 
		(
			(depthBound 3)
			(solution '() ) 
		)
		
		((not (NULL solution )) solution )
		
		(setf depthBound(+ depthBound 1) )
		(setf solution (dfs start depthBound puzzsize))
	
	)
)
		
;--------------------------------------------------------------------------

; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
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

; Member-state looks for a node on the node-list with the same state.
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
