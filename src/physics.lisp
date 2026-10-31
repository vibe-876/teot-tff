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


;; "Vector-ise" this later.
;; https://en.wikipedia.org/wiki/Newton%27s_law_of_universal_gravitation#Vector_form
;; (defun gravitational-force (ship-mass radius)
;;   (/ (* gravitational-constant earths-mass ship-mass)
;;      (expt radius 2)))

(defun gravitational-force (object-mass ship-mass radius)
  "RADIUS should be the position vector of the ship."
  (/ (* (* -1 gravitational-constant)
	object-mass ship-mass
	(normalise-vector radius))
     (expt (magnitude radius) 2)))

(defun magnitude (a)
  (sqrt (apply '+
	       (mapcar (lambda (an)
			 (expt an 2))
		       a))))

(defun normalise-vector (a)
  (let ((a-magnitude (magnitude a)))
    (mapcar (lambda (an)
	      (/ an a-magnitude))
	    a)))

(defun displacement-vector (a b)
  "Calculates the vector between a and b."
  (add-vectors (mapcar (lambda (an)
			 (* -1 an))
		       a)
	       b))

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
