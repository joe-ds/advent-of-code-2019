(require "asdf")

;;;; Part One Globals
(defparameter *i-stream* NIL)
(defparameter *o-stream* (make-string-output-stream))

;;;; Part Two Globals
(defparameter *io-list-stream* NIL)
(defparameter *contexts* (make-hash-table :test #'equal))
(defparameter *current-context* NIL)
(defparameter *prgm* NIL)

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
      ((result (read *i-stream*))
       (output (nth (1+ ip)  p)))
    (setf (nth output p) result)
    (list p (+ ip 2))))

(defun i-4 (p ip pmode)
  "Output"
  (let*
      ((params (fetch-params p ip pmode 1)))
    (print (first params) *o-stream*)
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

(defun permutations (l)
  (if l
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (y) (cons x y))
			  (permutations (remove x l))))
	      l)
      '(())))

(defun part-one ()
  (let
      ((prgm (parse-line (uiop:read-file-string "input"))))
    (print
     (loop for perm in (permutations '(0 1 2 3 4))
	do (print 0 *o-stream*)
	maximizing
	  (loop for phase in perm
	     do
	       (setf *i-stream*
		     (make-string-input-stream
		      (format nil "~{~A~^ ~}"
			      (list phase
				    (parse-integer
				     (get-output-stream-string *o-stream*)
				     :junk-allowed t)))))
	       (run-intcode prgm)
	     finally (return (parse-integer
			      (get-output-stream-string *o-stream*)
			      :junk-allowed t)))))))

;;;;===========================================================================
;;;;                               Part Two
;;;;
;;;; This was really all hacked together to continue using as much of the
;;;; existing code as possible. On the plus side, there is only one IO stream
;;;; for all the amplifiers.
;;;;
;;;;==========================================================================

(defun create-contexts (perm)
  "Initiate contexts (which are really just saved memory)
for all four amplifiers."
  (loop for phase in perm
       for name in '("A" "B" "C" "D" "E")
     do (setf (gethash name *contexts*) (list phase))))

(defun switch-context (&optional output)
  "Switch execution to the next amplifier."
  (let*
      ((new-context
	(loop for x in '("A" "B" "C" "D" "E")
	   for y in '("B" "C" "D" "E" "A")
	   if (equal x *current-context*)
	   return y))
       (context
	(gethash new-context *contexts*)))
    (setf *current-context* new-context)
    ;; We first check if the amplifier was ever run. If it never was, then
    ;; there will be a single list which has the amplifier's phase. We need to
    ;; make sure this is passed in first.
    ;; In both cases, we also check if the context switching was caused by an
    ;; amplifier having output to pass.
    (if (< 1 (length context))
	(progn
	  (if output
	      (setf *io-list-stream*
		    (append (list output) *io-list-stream*)))
	  (apply 'run-intcode-l context))
	(progn
	  (if output
	      (setf *io-list-stream*
		    (append (list (car context) output) *io-list-stream*))
	      (setf *io-list-stream*
		    (append (list (car context)) *io-list-stream*)))
	  (run-intcode-l *prgm*)))))

(defun i-3-l (p ip pmode)
  "Modified input function"
  (let*
      ((result (car *io-list-stream*))
       (output (nth (1+ ip) p)))
    (if result
	(progn
	  (setf (nth output p) result)
	  (setf *io-list-stream* (cdr *io-list-stream*))
	  (list p (+ ip 2)))
	;; If we have no inputs ready, we pause execution and switch to the
	;; next context.
	(progn
	  (setf (gethash *current-context* *contexts*) (list p ip))
	  (switch-context)))))

(defun i-4-l (p ip pmode)
  "Modified output function"
  (let*
      ((params (fetch-params p ip pmode 1)))
    (setf (gethash *current-context* *contexts*) (list p (+ ip 2)))
    (switch-context (first params))))

(defun run-intcode-l (p &optional (ip 0))
  (let*
      ((callback (list nil 'i-1 'i-2 'i-3-l 'i-4-l 'i-5 'i-6 'i-7 'i-8))
       (parsed-opcode (parse-opcode (nth ip p)))
       (instruction (first parsed-opcode))
       (pmode (cadr parsed-opcode)))
    (if (eql instruction 99)
	;; The final result is the output of the 'E' amplifier. Hence we check
	;; and return this output only if it is 'E' that is halting. If it
	;; isn't, we need to switch contexts.
	(if (equal *current-context* "E")
	    (first *io-list-stream*)
	    (switch-context))
	(if (equal instruction 4)
	    ;; The hacked forms of i-3 and i-4 won't always return. In i-3's
	    ;; case we don't have worry about when it doesn't return.
	    ;; But i-4 will never return, and will always pause execution. So
	    ;; We'll have to call it specially without any of the recursive
	    ;; apply calls.
	    (i-4-l p ip pmode)
	    (apply 'run-intcode-l
		   (funcall (nth instruction callback) p ip pmode))))))

(defun part-two ()
  (let
      ((prgm (parse-line (uiop:read-file-string "input"))))
    (print
     (loop for perm in (permutations '(9 8 7 6 5))
	do
	  (create-contexts perm)
	  (setf *prgm* prgm)
	  (setf *current-context* "A")
	  (setf *io-list-stream* (list (car perm) 0))
	maximizing (run-intcode-l prgm)))))