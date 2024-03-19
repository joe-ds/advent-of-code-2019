(require "asdf")

(defun parse-line (line)
  (map 'list 'parse-integer
       (uiop:split-string line :separator ",")))

(defun run_program (p &optional (ip 0))
  (cond
    ((eql (nth ip p) 99) (car p))
    ((or (eql (nth ip p) 1) (eql (nth ip p) 2))
     (let
	 ((a (nth (nth (+ ip 1) p) p))
	  (b (nth (nth (+ ip 2) p) p))
	  (r (nth (+ ip 3) p)))
       (if (eql (nth ip p) 1)
	   (setf (nth r p) (+ a b))
	   (setf (nth r p) (* a b)))
       (run_program p (+ ip 4))))
    (t nil)))

(defun part-one ()
  (let
      ((prgm (parse-line (uiop:read-file-string "input"))))
    (print
     (run_program
      (concatenate 'list `(,(car prgm) 12 2) (cdddr prgm))))))

(defun part-two ()
  (let
      ((prgm (parse-line (uiop:read-file-string "input"))))
    (loop named outer
       for x from 0 to 99
       append (loop for y from 0 to 99
		   do
		   (let
		       ((output
			 (run_program
			  (concatenate 'list
				       (list (car prgm) x y)
				       (cdddr prgm))))) 
		     (if (eql output 19690720)
			 (progn
			   (print (list x y))
			   (return-from outer (list x y)))
			 ()))))))