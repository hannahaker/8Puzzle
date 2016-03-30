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
	(setf fin (open filename :if-does-not-exist nil))   ; open file, returning NIL on error
	(when (null fin) (return-from fileio (format nil "Error: cannot open file ~a" filename)))
	
	(do (
		; read entire file, returning NIL at EOF
		(data (read fin nil) (read fin nil))
		(puz_data)
	)          
	
	((null data) (close fin) puz_data)                       ; exit when file is read
	; print what we read
	(setf puz_data (append puz_data (list data)))
	)
)


