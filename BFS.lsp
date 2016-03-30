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

Author: John M. Weiss, Ph.D.
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#

(defun bfs (start N)
	(let ((solution))
			;the solution will be the list returned from the dfs function.
		(setf solution (bfs start N))	
		
		(format t " BFS graph search~% --------------~% Solution found in ~d moves~% ~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" 
		(- (length solution) 1) *nodes-generated* *nodes-distinct* *nodes-expanded* )
		(printstates solution N)
	)
)

; Breadth-first-search implements the OPEN list as a QUEUE of (state parent) nodes.
; Given a start state and a search type (BFS or DFS), return a path from the start to the goal.
(defun search_bfs (start N)
	;initialize path variables
	(setf *nodes-distinct* 0)	 
	(setf *nodes-generated* 0)
	(setf *nodes-expanded* 0)
	(do*                                                    ; note use of sequential DO*
		(                                                   ; initialize local loop vars
			(curNode (make-node :state start :parent nil))  ; current node: (start nil)
			(OPEN (list curNode))                           ; OPEN list:    ((start nil))
			(CLOSED nil)                                    ; CLOSED list:  ( )
		)
		
		; termination condition - return solution path when goal is found
		((goal-state? (node-state curNode)) (build-solution curNode CLOSED))
		
		; loop body
		(when (null OPEN) (return nil))             ; no solution
		
		;increment the number of expanded nodes
		(setf *nodes-expanded* (+ 1 *nodes-expanded*))
		; get current node from OPEN, update OPEN and CLOSED
		(setf curNode (car OPEN))
		
		(setf OPEN (cdr OPEN))
		(setf CLOSED (cons curNode CLOSED))
		
		; add successors of current node to OPEN
		(dolist (child (generate-successors (node-state curNode) N))
			
			; for each child node
			(setf child (make-node :state child :parent (node-state curNode)))
			;increment the number of generated nodes
			(setf *nodes-generated* (+ 1 *nodes-generated*)) 
			
			; if the node is not on OPEN or CLOSED
			(if (and (not (member child OPEN   :test #'equal-states))
			(not (member child CLOSED :test #'equal-states)))
			(progn
				;increment the number of distinct nodes
				(setf *nodes-distinct* (+ 1 *nodes-distinct*)) 
				; add it to the OPEN list
				; BFS - add to end of OPEN list (queue)
				(setf OPEN (append OPEN (list child)))   
			)
			)
		)
	)
)

