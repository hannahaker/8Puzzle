#|                    ***** Astar.LSP *****

General-purpose exhaustive search routine includes heuristics. Uses graph 
search with OPEN and CLOSED lists rather than tree search, to avoid cycles. 
Uses heuristics to guide search.

To solve a specific problem, the functions "generate-successors" and
"goal-state" must be defined. "Generate-successors" takes a state as its
argument and returns a list of child states. "Goal-state?" returns T if
its argument is a goal state, NIL otherwise.

In order to retrace a solution path, nodes are stored as (state parent)
pairs, where "state" is the current state and "parent" is the parent
state. Given a goal node, a solution path is generated by simply tracing
backwards through the parent states.

Author: Hannah Aker
Written Spring 2016 for CSC447/547 AI class.
|#

;--------------------------------------------------------------------------

;Admissible Heuristic #1: Misplaced Tiles - sum number of tiles not in correct place
(defun misplacedTiles (state N) 
	(let ((sum 0) (goal (nth (- N 3) goal-states)))
		;dotimes N*N (8 puzzle is 3 by 3, N=3)
		(dotimes (i (* N N) ())
			;compare nth number in goal and current state
			(if (= (nth i state) (nth i goal)) 
				;if not equal set sum to sum +1
				() (setf sum (1+ sum))
			)
		)
		sum
	)
)
;Admissible Heuristic #2: Manhattan Distance - sum of number of rows and columns each tile is away from its correct place
(defun manhattanDistance (state N) 
	(let ((sum 0) (goal (nth (- N 3) goal-states)) (j))
		;dotimes N*N (8 puzzle is 3 by 3, N=3)
		(dotimes (i (* N N) ())
			;get position of (nth i state) in goal 
			(setf j (position (nth i state) goal))
			;get diff in row and column position- (floor ( / larger-smaller N)) + (mod larger-smaller N), add to sum
			(if (> i j) (setf sum (+ sum (floor (/ (- i j) N)) (mod (- i j) N)))
				(setf sum (+ sum (floor (/ (- j i) N)) (mod (- j i) N)))
			)
		)
		sum
	)
)
;Inadmissible Heuristic: Nilsson's Sequence - sum of Manhattan distance and sequence score ( +2 for each tile not followed by correct successor and +1 for the zero in the wrong place)
(defun nilssonsSequence (state N) 
	;run manhattanDistance heuristic
	(let ((sum 0) (goal (nth (- N 3) goal-states)) (i) (j) (order '(0 1 2 5 8 7 6 3)) )
		(setf sum (manhattanDistance state N) )
		;dotimes N*N (8 puzzle is 3 by 3, N=3)
		(if (= N 3)
			(prodepth 
				(dotimes (j 7 ())
					(if (= (+ 1 (nth (nth j order) state)) (nth (nth (+ j 1) order) state))
					() (setf sum (+ sum 2)))
				)
				(if (= (nth 4 state) 0 ) 
				() (setf sum (+ sum 1)))
			)
			(prodepth
				(dotimes (i (- (* N N) 2) ())
					(if (= (+ 1 (nth i state)) (nth (+ i 1) state))
					()  (setf sum (+ sum 2)))
					;check for goal state order, if n+1th doesn't match n+1th of goal state
				)
				(if (= (nth (- (* N N) 1) state) 0 ) 
				() (setf sum (+ sum 1)))
			)
		)
		sum
	)
)
;--------------------------------------------------------------------------

; A* star implements an OPEN list and uses a heuristic to select the next node to explore
(defun astar (start N)
	(let ((solution))
		(setf solution (search_astar start 'misplacedTiles N))
		(format t " A* graph search (admissible heuristic: misplaced tiles)~% --------------~% Solution found in ~d moves~% ~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" 
		(- (length solution) 1) nodes-generated nodes-distinct nodes-expanded )
		(printstates solution N)
		
		(setf solution (search_astar start 'manhattanDistance N))
		(format t " A* graph search (admissible heuristic: Manhattan distance)~% --------------~% Solution found in ~d moves~% ~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" 
		(- (length solution) 1) nodes-generated nodes-distinct nodes-expanded )
		(printstates solution N)
		
		(setf solution (search_astar start 'nilssonsSequence N))
		(format t " A* graph search (inadmissible heuristic: Nilsson's Sequence)~% --------------~% Solution found in ~d moves~% ~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" 
		(- (length solution) 1) nodes-generated nodes-distinct nodes-expanded )
		(printstates solution N)
		
	)
)



; Given a start state and a search type (BFS or DFS), return a path from the start to the goal.
(defun search_astar (start heuristic N)
	(setf nodes-distinct 0) 
	(setf nodes-generated 0)
	(setf nodes-expanded 0)
	
	(do*                                                   
		(                                                   ; initialize local loop vars
			(curNode (make-node :state start :parent nil :depth 0 :fn 0))  ; current node: (start nil)
			(OPEN (list curNode))                           ; OPEN list:    ((start nil))
			(CLOSED nil)                                    ; CLOSED list:  ( )
			(i)
		)
		
		; termination condition - return solution path when goal is found
		((goal-state? (node-state curNode)) (build-solution curNode CLOSED))
		
		; loop body
		(when (null OPEN) (return nil))             ; no solution
		
		; get current node by selecting best heuristic, update OPEN and CLOSED
		(setf curNode (car OPEN))
		;loop through OPEN list find state with best fn
		(dotimes (i (length OPEN)) 
			(if (< (node-fn (nth i OPEN)) (node-fn curNode)) 
				(setf curNode (nth i OPEN))
				() 
			)
		)
		(setf nodes-expanded (+ 1 nodes-expanded) )
		(setf OPEN (remove curNode OPEN))
		(setf CLOSED (cons curNode CLOSED))
		
		; add successors of current node to OPEN
		(dolist (child (generate-successors (node-state curNode) N))
			
			; for each child node
			(setf child (make-node :state child :parent (node-state curNode) ) )
			(setf nodes-generated (+ 1 nodes-generated))
			
			; if the node is not on OPEN or CLOSED
			(if (and (not (member child OPEN   :test #'equal-states))
			(not (member child CLOSED :test #'equal-states)))
			
			; add it to the OPEN list
			(prodepth
				(setf nodes-distinct (+ 1 nodes-distinct ))
				(setf (node-depth child) (+ 1 (node-depth curNode)))
				(setf (node-fn child) (+ 1 (node-depth curNode) (funcall heuristic (node-state child) N)))
				(setf OPEN (append OPEN (list child)))
			)
			
			)
		)
		
		
	)
)	
