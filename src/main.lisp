(load "src/physics.lisp")


(defun main ()
  (let ((answer (gravitational-force
		 1 earth-mass
		 (list earth-surface-radius 0))))
    (print answer)))

(main)
