(require "asdf")

(defun parse-input (input x y)
  (let
      ((integer-list (loop for c across input
			collecting (parse-integer (string c))))
       (s (* x y)))
    (loop for i from 0 below (length integer-list) by s
       collect (subseq integer-list i (+ s i)))))

(defun part-one ()
  (let*
      ((image (parse-input (uiop:read-file-string "input") 25 6))
       (minimal-0 999)
       (minimal-v 0))
    (print
     (loop for layer in image
	if (> minimal-0 (count 0 layer))
	do (setf minimal-0 (count 0 layer))
	  (setf minimal-v (* (count 1 layer) (count 2 layer)))
	finally (return minimal-v)))))

(defun part-two ()
  (let*
      ((image (parse-input (uiop:read-file-string "input") 25 6))
       (result
	(loop for pixel from 0 below (* 25 6)
	   for colours = (loop for layer in image collecting (elt layer pixel))
	   collecting (loop for c in colours if (> 2 c) return c)))
       (output
	(loop for x from 0 below 150 by 25
	   collecting (subseq result x (+ 25 x)))))
    (loop for row in output
       do
	 (loop for n in row
	     if (equal n 1)
	    do (format t "~C~C" (code-char #x2591) (code-char #x2591))
	     else
	    do (format t "~C~C" (code-char #x2593) (code-char #x2593)))
	 (terpri))))