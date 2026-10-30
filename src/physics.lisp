(defconstant gravitational-constant 6.6674e-11)
(defconstant earths-mass 5.972e18)
(defconstant earth-surface-radius 6371)
(defconstant delta-t 0.1)


(defun gravitational-force (ship-mass radius)
  (/ (* gravitational-constant earths-mass ship-mass)
     (expt radius 2)))

(defun du (u)
  nil)

(defun calculate-shot (velocity boundary-conditions)
  nil)
