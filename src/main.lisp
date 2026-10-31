(load "src/physics.lisp")


(defun main ()
  (let ((answer (gravitational-force
		 earth-mass 1
		 (list earth-surface-radius 0))))
    (print answer)))

(main)
