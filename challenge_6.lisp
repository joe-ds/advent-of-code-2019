(require "asdf")

(defun add-node (table parent node)
  (setf (gethash node table) (1+ (gethash parent table))))

(defun find-orbiters (table bodies center)
  (loop for pair in bodies
	  if (equal (car pair) center)
	  do (add-node table (car pair) (cadr pair))
	  (find-orbiters table bodies (cadr pair))
	  finally (return table)))

(defun trace-orbits (bodies target)
  (loop for pair in bodies
	if (equal (cadr pair) target)
	return (cons (car pair) (trace-orbits bodies (car pair)))))

(defun part-one ()
  (let
      ((orbit-table (make-hash-table :test 'equal))
       (bodies (loop for line in (uiop:read-file-lines "input")
		     for pairs = (uiop:split-string line :separator ")")
		     collect pairs)))
    (setf (gethash "COM" orbit-table) 0)
    (find-orbiters orbit-table bodies "COM")
    (print
     (loop for orbits being the hash-values of orbit-table
	   sum orbits))))

(defun part-two ()
  (let*
      ((bodies (loop for line in (uiop:read-file-lines "input")
		     for pairs = (uiop:split-string line :separator ")")
		     collect pairs))
       (path-from-san (trace-orbits bodies "SAN"))
       (path-from-you (trace-orbits bodies "YOU")))
    (print (length (set-exclusive-or path-from-you path-from-san :test #'equal)))))