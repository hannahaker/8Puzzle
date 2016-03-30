#|********************************* 8puzzle.lsp *******************************
	Program 2 - 8 Puzzle: A*, BFS, and DFID Solution Search

	Authors: Hannah Aker, Derek Lane, Savoy Schuler

	Date: March 29, 2016

	Professor: Dr. John Weiss

	Course: CSC 447 - M001

	Usage Instructions: clisp 8puzzle <optional - puzzleFile>

	Example Input:	clisp 8puzzle easy.puz

	Bugs: None
	
	Todo: None

	Program Details:

	This program is designed to produce the optimal solution to the 8 Puzzle
	tile game. A puzzle may be entered in interpreter mode or as a file 
	(shown in "Usage"). The only bound on puzzle size is that it must be NxN
	and comprised of integers 0 to N where 0 represents the blank puzzle
	space. 

	Either run option will read and store the puzzle as a row major list.
	The list will be copied and passed to three different search algorithms:
	A*, BFS, and DFID. Each puzzle will iteratively solve for the most 
	optimal path. A parent-child node structure is used to store the path
	from the solution state back to the goal state. Each search will count
	the number of generations of nodes produced and at the search's 
	conclusion will output the search results in addition to the number of 
	moves needed to reach the optimal solution. 

*****************************************************************************|#

(load (merge-pathnames "utilities.lsp" *load-truename*))
(load (merge-pathnames "astar.lsp" *load-truename*))
(load (merge-pathnames "DFID.lsp" *load-truename*))
(load (merge-pathnames "input.lsp" *load-truename*))
(load (merge-pathnames "BFS.lsp" *load-truename*))

#|*****************************************************************************  
  Authors: Hannah Aker, Derek Lane, Savoy Schuler
  
  Function: 8puzzle 
  
  Description: 		
	
	This function checks if a puzzle state is a valid puzzle. The function
	Also checks if the puzzle state is solvable by using the function 
	provided at www.mcs.sdsmt.edu/csc447/Assignments/PA2/solvable.lsp. 
	Lastly, the function calls the three searches BFS, DFS, and A*.
	
  Parameters: 
	puzzle - a puzzle state

  Returns: None
*****************************************************************************|#
(defun 8puzzle (puzzle)
	(if (validinput puzzle 3)
		
		(if (solvable puzzle)
			(progn
				;call search functions here with puzzle
				(BFS puzzle 3)
				(astar puzzle 3)				
				(dfid puzzle 3)
			)
			(format t "Puzzle not solvable.")
		)
		(format t "Invalid input.")
	)
	(values)
)

#|*****************************************************************************  
  Authors: Hannah Aker, Derek Lane, Savoy Schuler  

  Function: Npuzzle 
  
  Description: 		
	
	This function calls validinput to check check that puzzle is a valid NxN
	puzzle. If the the puzzle is valid, this funciton will be returned to to
	call all three searches: BFS, DFS, A*.
	
  Parameters: 
	puzzle - a puzzle state

  Returns: None
*****************************************************************************|#
(defun Npuzzle (puzzle N)
	(if (validinput puzzle N)
	
			(progn
				;call search functions here with puzzle
				(BFS puzzle N)				
				(astar puzzle N)				
				(dfid puzzle N)
			)
			
		(format t "Invalid input.")
	)
	(values)
)

#|*****************************************************************************  
  Authors: Hannah Aker, Derek Lane, Savoy Schuler

  Function: validinput 
  
  Description: 		
	
	This function checks if the puzzle provided is valid for either
	3x3 or NxN. Exits if not valid. Valid true is returned.
	
  Parameters: 
	puzsize - size of the puzzle

  	Returns: T   - if a valid puzzle
		 nil - if not a valid puzzle
*****************************************************************************|#
(defun validinput (puzzle puzsize)
	(let ((listvalid) (i))
		(setf listvalid T)
		;check for an actual list and correct length
		(if (and (listp puzzle) (= (length puzzle) (* puzsize puzsize)))
			(dotimes (i (* puzsize puzsize) ())
				;check for an actual number, correct range of numbers, and no repeated numbers
				(if (numberp (nth i puzzle)) 
					(if (and 
						(>= (nth i puzzle) 0) 
						(< (nth i puzzle) (* puzsize puzsize)) 
					(not (member (nth i puzzle) (cdr (nthcdr i puzzle)))) )
					
					() (setf listvalid NIL)
					)
					(setf listvalid NIL)
				)
			)
			(setf listvalid NIL)
		)
		listvalid
	)
)

(8puzzle (input *args*))
	
