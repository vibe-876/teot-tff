;; Units:
;; - Distance: meters
;; - Time: Seconds
;; - Mass: Kilograms
(defconstant gravitational-constant 6.6674e-11)

(defconstant earths-mass 5.972e24)
(defconstant earth-surface-radius 6.371e6)

(defconstant moon-mass 7.346e22)
(defconstant moon-surface-radius 1.737e6)

(defconstant dt 0.1)


(defun gravitational-force (ship-mass radius)
  (/ (* gravitational-constant earths-mass ship-mass)
     (expt radius 2)))

(defun normalise-vector (a)
  (let ((a-magnitude (sqrt (apply '+
				  (mapcar (lambda (an)
					    (expt an 2))
					  a)))))
    (mapcar (lambda (an)
	      (/ an a-magnitude))
	    a)))

(defun multiply-vector (alpha a)
  "Multiply a vector a by a (real) scalar alpha."
  (mapcar (lambda (an) (* alpha an))
	  a))

(defun add-vectors (a b)
  "Add two (real) vectors a and b together."
  (mapcar (lambda (an bn) (+ an bn))
	  a b))

(defun ds (s u)
  (add-vectors s (multiply-vector dt u)))

;(defun )

(defun calculate-shot (velocity boundary-conditions)
  nil)
