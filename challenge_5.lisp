(require "asdf")

(defun parse-line (line)
  (map 'list 'parse-integer
       (uiop:split-string line :separator ",")))

(defun integer-to-digits (n)
  (loop for i from (ceiling (log n 10)) downto 1
	for a = (expt 10 i)
	for b = (expt 10 (- i 1))
	collect (truncate (mod n a) b)))

(defun parse-opcode (opcode)
  (let*
      ((instruction (mod opcode 100))
       (parsed (cddr (reverse (integer-to-digits opcode))))
       (pmode (concatenate 'list parsed
			   (make-list (- 4 (length parsed)) :initial-element 0))))
    (list instruction pmode)))

(defun fetch-params (p ip pmode number-of-params)
  (loop for i from 1 to number-of-params
	for mode in pmode
	collect (if (eq mode 0)
		    (nth (nth (+ ip i) p) p)
		  (nth (+ ip i) p))))

(defun i-1 (p ip pmode)
  "Addition"
  (let*
      ((params (fetch-params p ip pmode 2))
       (result (apply '+ params))
       (output (nth (+ ip 3) p)))
    (setf (nth output p) result)
    (list p (+ ip 4))))

(defun i-2 (p ip pmode)
  "Multiplication"
  (let*
      ((params (fetch-params p ip pmode 2))
       (result (apply '* params))
       (output (nth (+ ip 3) p)))
    (setf (nth output p) result)
    (list p (+ ip 4))))

(defun i-3 (p ip pmode)
  "Input"
  (let*
      ((result (parse-integer
		(progn
		  (format *query-io* "~a: " "INPUT")
		  (force-output *query-io*)
		  (read-line *query-io*))))
       (output (nth (1+ ip)  p)))
    (setf (nth output p) result)
    (list p (+ ip 2))))

(defun i-4 (p ip pmode)
  "Output"
  (let*
      ((params (fetch-params p ip pmode 1)))
    (print (first params))
    (list p (+ ip 2)))) 

(defun i-5 (p ip pmode)
  "Jump If True"
  (let*
      ((params (fetch-params p ip pmode 2)))
    (if (zerop (first params))
	(list p (+ ip 3))
      (list p (cadr params)))))

(defun i-6 (p ip pmode)
  "Jump If False"
  (let*
      ((params (fetch-params p ip pmode 2)))
    (if (zerop (first params))
	(list p (cadr params))
      (list p (+ ip 3)))))

(defun i-7 (p ip pmode)
  "Less Than"
  (let*
      ((params (fetch-params p ip pmode 2))
       (result (apply '< params))
       (output (nth (+ ip 3) p)))
    (if result
	(setf (nth output p) 1)
      (setf (nth output p) 0))
    (list p (+ ip 4))))

(defun i-8 (p ip pmode)
  "Equal"
  (let*
      ((params (fetch-params p ip pmode 2))
       (result (apply 'equal params))
       (output (nth (+ ip 3) p)))
    (if result
	(setf (nth output p) 1)
      (setf (nth output p) 0))
    (list p (+ ip 4))))

(defun run-intcode (p &optional (ip 0))
  (let*
      ((callback (list nil 'i-1 'i-2 'i-3 'i-4 'i-5 'i-6 'i-7 'i-8))
       (parsed-opcode (parse-opcode (nth ip p)))
       (instruction (first parsed-opcode))
       (pmode (cadr parsed-opcode)))
    (if (eql instruction 99)
	(car p)
      (apply 'run-intcode (funcall (nth instruction callback) p ip pmode)))))

(defun part-one ()
  (let
      ((prgm (parse-line (uiop:read-file-string "input"))))
    (run-intcode prgm)))

(defun part-two ()
  (part-one))