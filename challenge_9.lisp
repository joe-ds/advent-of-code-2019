(require "asdf")
(defparameter *base-offset* 0)
(defparameter *debug* 0)

;;;; NOTES ON RUNNING THIS PROGRAM
;;;; -----------------------------
;;;; This worked fine for part one (INPUT 1) on GNU CLISP. However, doe to a
;;;; lack of TCO, it caused a stack overflow error for part two (INPUT 2).
;;;; SBCL is a better bet, but you can enable TCO in most REPLs by changing the
;;;; debug level.
;;;;
;;;; The actual code used was:
;;;; sbcl --load challenge_9.lisp --eval "(sb-ext:save-lisp-and-die \"challenge_9\" :toplevel #'part-one :executable t)"

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

(defun fetch-params (p ip pmode number-of-params &key (is-output nil))
  (loop for i from 1 to number-of-params
	for mode in pmode
	;; If one of the params is output, it will be the last and we should
	;; only return the index. Also, we don't use mode 1.
	if (and (= i number-of-params) is-output)
	collect (if (eq mode 0)
		    (nth (+ ip i) p)
		  (+ *base-offset* (nth (+ ip i) p)))
	else
	collect (if (eq mode 0)
		    (nth (nth (+ ip i) p) p)
		  (if (eq mode 1)
		      (nth (+ ip i) p)
		    (nth (+ *base-offset* (nth (+ ip i) p)) p)))))

(defun i-1 (p ip pmode)
  "Addition"
  (let*
      ((params (fetch-params p ip pmode 3 :is-output t))
       (result (apply '+ (butlast params)))
       (output (car (last params))))
    (setf (nth output p) result)
    (list p (+ ip 4))))

(defun i-2 (p ip pmode)
  "Multiplication"
  (let*
      ((params (fetch-params p ip pmode 3 :is-output t))
       (result (apply '* (butlast params)))
       (output (car (last params))))
    (setf (nth output p) result)
    (list p (+ ip 4))))

(defun i-3 (p ip pmode)
  "Input"
  (let*
      ((params (fetch-params p ip pmode 1 :is-output t))
       (result (parse-integer
		(progn
		  (format *query-io* "~a: " "INPUT")
		  (force-output *query-io*)
		  (read-line *query-io*))))
       (output (first params)))
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
      ((params (fetch-params p ip pmode 3 :is-output t))
       (result (apply '< (butlast params)))
       (output (car (last params))))
    (if result
	(setf (nth output p) 1)
      (setf (nth output p) 0))
    (list p (+ ip 4))))

(defun i-8 (p ip pmode)
  "Equal"
  (let*
      ((params (fetch-params p ip pmode 3 :is-output t))
       (result (apply 'equal (butlast params)))
       (output (car (last params))))
    (if result
	(setf (nth output p) 1)
      (setf (nth output p) 0))
    (list p (+ ip 4))))

(defun i-9 (p ip pmode)
  "Adjust Base Offset"
  (let*
      ((params (fetch-params p ip pmode 1)))
    (setf *base-offset* (+ *base-offset* (first params)))
    (list p (+ ip 2))))

(defun run-intcode (p &optional (ip 0))
  (let*
      ((callback (list nil 'i-1 'i-2 'i-3 'i-4 'i-5 'i-6 'i-7 'i-8 'i-9))
       (parsed-opcode (parse-opcode (nth ip p)))
       (instruction (first parsed-opcode))
       (pmode (cadr parsed-opcode)))
    (if (< 0 *debug*)
	(format *error-output* "OPCODE: ~a~%" parsed-opcode))
    (if (eql instruction 99)
	(car p)
      (apply 'run-intcode (funcall (nth instruction callback) p ip pmode)))))

(defun part-one ()
  (let*
      ((prgm (parse-line (uiop:read-file-string "input")))
       (prgmwm (append prgm
		       (make-list (- 2000 (length prgm)) :initial-element 0))))
    (run-intcode prgmwm)))