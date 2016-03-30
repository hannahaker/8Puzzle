(defun read_start(args)
    (let (filename puzList)
        (cond
            ((= (length args) 1)    
                (setf filename (caddr args))
                (setf puzList (readPuzzleFile filename))
            )
                
                
            ((= (length args) 0)    
                (format t "You have entered interactive mode. Provide a list of numbers 0-8 in row major order where 0 is the blank for the puzzle.")
                (setf puzList (read))
                (if ( not ( = (length puzList) 9))
                    (fromat t "Please ensure all 9 unique numbers 0- 8 are entered.")
                    (readStart args)
                )
                    (parseList puzList)
            )
                ; else statement prints usage statement
            (t   ‘defaults’ )

)))
