(require "asdf")

(defmacro with-safe-io-syntax (&body body)
  "From Practical Common Lisp"
  `(with-standard-io-syntax
     (let ((*read-eval* nil))
       ,@body)))

(defun parse (file)
  "Converts the source input into coordinates for the asteroids."
  (loop for line in file
	for y from 0 to 99
	append (loop for char across line
		 for x from 0 to 99
		 if (equal char #\#)
		 collect `(,x ,y))))

(defun carte-to-angle (coord)
  (apply 'atan (reverse coord)))

(defun offset-carte-to-angle (self other)
  "Same as carte-to-angle, but with self as origin."
  (carte-to-angle (list
		   (- (car other) (car self))
		   (- (cadr other) (cadr self)))))

(defun tally-lines-of-sight (coords)
  (loop for coord in coords
	collect (length
		 (remove-duplicates
		  (loop for other in (remove coord coords :test 'equal)
			collect (offset-carte-to-angle coord other))))))

(defun find-best-point (coords)
  (let
      ((tally (tally-lines-of-sight coords)))
    (elt coords (position (apply 'max tally) tally))))

(defun part-one ()
  (let
      ((coords (parse (with-safe-io-syntax (uiop:read-file-lines "input")))))
    (print
     (loop for coord in coords
	   maximizing (length
		       (remove-duplicates
			(loop for other in (remove coord coords :test 'equal)
			      collect (offset-carte-to-angle coord other))))))))

(part-one)