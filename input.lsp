;********************************* input.lsp **********************************

  Authors: Hannah Aker, Derek Lane, Savoy Schuler
  
  Function: input 
  
  Description: 		
	
	This function reads the number of command line arguments and determines
	handling for input. 

	If an excess of 1 argument is entered, usage is 
	printed and the program halts. Else if 1 argument is entered, it is 
	required to be a puzzle file and will be passed to fileio.lsp for 
	opening, reading, and storing. An error will be printed if the argument
	given is not a proper file. 

	If no arguments are given, the program enters interpreter mode and waits
	for an NxN puzzle to be entered into the terminal. 

	If more than two arguments are entered, the program prints usage and 
	halts. 

	
  Parameters: 
	args - command lines arguments

 	Returns: puzList - a list holding the puzzle in row major order
*****************************************************************************|#

(defun input(args)
	(let ((filename) (puzList))
		(cond
			((= (length args) 1)    
				(setf filename (merge-pathnames (car args) *load-truename*))
				
				(setf puzList (fileio filename))

			)
			
			
			((= (length args) 0)    
				(format t "You have entered interpreter mode. Please provide a list of numbers equivalent to an NxN puzzle in row major order. 0 is considered to be the puzzle's blank tile.")
				(setf puzList (read))
			)
			; else statement prints usage statement
			(t "Usage: clisp 8puzzle.lsp <optional - puzzleFile>")
			
		)	
		puzList
	)

)

