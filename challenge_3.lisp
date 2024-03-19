(require "asdf")

(defun parse-line (line)
  (uiop:split-string line :separator ","))

(defun add-points (direction curr-x curr-y set-of-points)
  (let
      ((d (char direction 0))
       (steps (parse-integer (subseq direction 1 (length direction)))))
    (cond
      ((eql d #\U)
       (loop for step from 1 to steps
	  do
	    (push (list curr-x (+ curr-y step)) set-of-points)
	  finally (return set-of-points)))
      ((eql d #\D)
       (loop for step from 1 to steps
	  do
	    (push (list curr-x (- curr-y step)) set-of-points)
	  finally (return set-of-points)))
      ((eql d #\R)
       (loop for step from 1 to steps
	  do
	    (push (list (+ curr-x step) curr-y) set-of-points)
	  finally (return set-of-points)))
      ((eql d #\L)
       (loop for step from 1 to steps
	  do
	    (push (list (- curr-x step) curr-y) set-of-points)
	  finally (return set-of-points)))
      (t nil))))

(defun loco (directions)
  (let
      ((set-of-points ())
       (curr-x 0)
       (curr-y 0))
    (loop for direction in directions
       do
	 (setq set-of-points
	       (add-points direction curr-x curr-y set-of-points))
	 (setq curr-x (caar set-of-points))
	 (setq curr-y (cadar set-of-points))
       finally (return set-of-points))))

(defun part-one ()
  (let*
      ((files (uiop:read-file-lines "input"))
       (wire-1 (loco (parse-line (car files))))
       (wire-2 (loco (parse-line (cadr files))))
       (intersects (intersection wire-1 wire-2 :test 'equal)))
    (loop for stuff in intersects
       minimizing (+ (abs (car stuff)) (abs (cadr stuff))))))

(defun pathfinder (wire intersects)
  (let
      ((points (make-hash-table :test 'equal)))
    (loop for intersect in intersects
       do (setf (gethash intersect points) nil))
    (loop for step in (reverse wire)
       for current-distance from 1
       do
	 (if (and
	      (nth-value 1 (gethash step points))
	      (not (gethash step points)))
	     (setf (gethash step points) current-distance)))
    (loop for intersect in intersects
       collecting (gethash intersect points))))

(defun part-two ()
  (let*
      ((files (uiop:read-file-lines "input"))
       (wire-1 (loco (parse-line (car files))))
       (wire-2 (loco (parse-line (cadr files))))
       (intersects (intersection wire-1 wire-2 :test 'equal)))
    (loop for a in (pathfinder wire-1 intersects)
       for b in (pathfinder wire-2 intersects)
       minimizing (+ a b))))