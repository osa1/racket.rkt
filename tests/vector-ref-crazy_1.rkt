(if (let ((vector-1 (vector (vector #t))))
	  (and (vector-ref (vector-ref vector-1 0) 0) #t)) 42 123)
