(require "asdf")

(defun calculate-rocket-fuel (mass)
  (- (floor (/ mass 3)) 2))

(defun calculate-rocket-fuel-r (mass)
  (let
      ((m (calculate-rocket-fuel mass)))
    (loop while (> m 0)
	  sum m
	  do (setq m (calculate-rocket-fuel m))))) 

(defun task-one ()
  (print
   (loop for line in (uiop:read-file-lines "input")
	 sum (calculate-rocket-fuel
	      (parse-integer
	       (string-trim " " line))))))

(defun task-two ()
  (print
   (loop for line in (uiop:read-file-lines "input")
	 sum (calculate-rocket-fuel-r
	      (parse-integer
	       (string-trim " " line))))))