(load "src/physics.lisp")


(defun main ()
  (let ((answer (gravitational-force 1 earth-surface-radius)))
    (print answer)))

(main)
