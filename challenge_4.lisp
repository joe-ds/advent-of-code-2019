(defun split-number (n)
  (let ((n (write-to-string n)))
    (loop for c across n
	 collect (digit-char-p c))))

(defun find-numbers (low high)
  (loop for n from low to high
     for digits = (split-number n)
     if (and
	 (apply '<= digits)
	 (loop for d in digits
	    for e in (cdr digits)
	    thereis (eq d e)))
     count digits))

(defun part-one ()
  (print (find-numbers 147981 691423)))

(defun find-numbers-2 (low high)
  (loop for n from low to high
     for digits = (split-number n)
     if (and
	 (apply '<= digits)
	 (loop for d in digits
	    for e in (cdr digits)
	    thereis (and
		     (eq d e)
		     (eq 2 (count d digits)))))
     count digits))

(defun part-two ()
  (print (find-numbers-2 147981 691423)))