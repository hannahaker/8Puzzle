(defun read_start(args)
    (let (filename puzList)
        (cond
            ((= (length args) 3)    
                (setf filename (caddr args))
                (setf puzList (readPuzzleFile filename))
            )
                
            ((= (length args) 2)
                (setf puzList (cadr args))
            )
                
            ((= (length args) 1)    
                (format t "Enter a list of numbers 0-8 in row major order. 0 is the blank for the puzzle")
                (setf puzList (read))
                (if ( not ( = (length puzList) 9))
                    (fromat t "Error not enough numbers")
                    (readStart args)
                )
                    (parseList puzList)
                
            )
                ; else statement prints usage statement
            (t   ‘defaults’ )
)))
