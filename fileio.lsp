#|
***** fileio.lsp *****

Illustrates file I/O in Lisp.

Author: John M. Weiss, Ph.D.
Posted Spring 2016 for SDSM&T CSC447/547 Artificial Intelligence.
|#

; fileio function
(defun fileio ( filename )
	"(fileio filename): open an input file and read the data"
	
	; check for correct usage
	(when (null filename) (return-from fileio "Usage: fileio.lsp filename"))
	
	; read through file using open
	;(format t "~%Opening file ~a using open~%" filename)
	(setf fin (open filename :if-does-not-exist nil))   ; open file, returning NIL on error
	(when (null fin) (return-from fileio (format nil "Error: cannot open file ~a" filename)))
	
	(do (
		(data (read fin nil) (read fin nil))
			(puz_data)
	)          ; read entire file, returning NIL at EOF
	((null data) (close fin) puz_data)                       ; exit when file is read
	;(format t "~a~%" data)  
	; print what we read
	(setf puz_data (append puz_data (list data)))
	)
)

; call the fileio function, passing the first command-line argument as an input filename
;(format t "~a~%" (fileio (car *args*)))

