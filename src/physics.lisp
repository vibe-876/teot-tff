;; Units:
;; - Distance: meters
;; - Time: Seconds
;; - Mass: Kilograms
(defconstant gravitational-constant 6.6674e-11)

(defconstant earth-mass 5.972e24)
(defconstant earth-surface-radius 6.371e6)

(defconstant moon-mass 7.346e22)
(defconstant moon-surface-radius 1.737e6)
(defconstant moon-distance-from-earth 3.84784e8)

(defconstant dt 0.1)


(defclass ship ()
  ((acceleration :accessor a
	     :initarg :a
	     :type list)
   (mass :accessor m
	 :initarg :m
	 :type list)
   (position :accessor s
	     :initarg :s
	     :initform (list earth-mass 0)
	     :type list)))


;; Probably need to make add-vectors take an arbitrary
;; number of arguments, but that's neither here nor now.
(defun force-on-ship (ship)
  (let ((ship-mass (m ship))
	(ship-position (s ship)))
    (add-vectors
     (add-vectors (gravity-from-earth ship-mass ship-position)
		  (gravity-from-moon ship-mass ship-position (list moon-distance-from-earth 0)))
     (get-velocity-force ship))))

(defun get-velocity-force (ship)
  (multiply-vector (m ship)
		   (a ship)))

(defun gravity-from-earth (ship-mass position)
  (gravitational-force earth-mass ship-mass
		       (displacement-vector '(0 0) position)))

(defun gravity-from-moon (ship-mass ship-position moon-position)
  (gravitational-force moon-mass ship-mass
		       (displacement-vector moon-position ship-position)))

(defun gravitational-force (object-mass ship-mass radius)
  "RADIUS should be the position vector of the ship."
  (divide-through-vector (multiply-vector
			  (* -1 gravitational-constant
			     object-mass ship-mass)
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

(defun divide-through-vector (a alpha)
  "Divide through a by alpha."
  (mapcar (lambda (an)
	    (/ an alpha))
	  a))

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
