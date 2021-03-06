(in-package :cl-mesh)

(use-package :let-plus)

(defun v3 (a b c)
  (vector a b c))

(defun v+ (a b)
  (declare (type (simple-vector 3) a b))
  (map 'vector #'+ a b))

(defun v- (a b)
  (declare (type (simple-vector 3) a b))
  (map 'vector #'- a b))

(defun v-norm2 (a)
  (declare (type (simple-vector 3) a))
  (v-dot a a))

(defun v-norm (a)
  (declare (type (simple-vector 3) a))
  (sqrt (v-norm2 a)))

(defun v-cos-angle (a b)
  (declare (type (simple-vector 3) a b))
  (/ (v-dot a b) (v-norm a) (v-norm b)))

(defun v-dist2 (a b)
  (declare (type (simple-vector 3) a b))
  (v-norm2 (v- a b)))

(defun v*f (a f)
  (declare (type (simple-vector 3) a))
  (map 'vector #'(lambda (x) (* x f)) a))

(defun v-normal (a)
  (declare (type (simple-vector 3) a))
  (v*f a (/ (v-norm a))))

(defun v-dot (a b)
  (declare (type (simple-vector 3) a b))
  (reduce #'+ (map 'vector #'* a b)))

(defun v-cross (a b)
  (declare (type (simple-vector 3) a b))
  (let-plus::let+ ((#(x1 x2 x3) a)
         (#(y1 y2 y3) b))
    (v3 (- (* x2 y3) (* x3 y2))
        (- (* x3 y1) (* x1 y3))
        (- (* x1 y2) (* x2 y1)))))
