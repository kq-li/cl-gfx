(in-package :graphics)

(defvar *EPSILON* 0.0001)

(defun square (x)
  (if (< x *EPSILON*)
      0
      (* x x)))

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))

(defun bound (x bound1 bound2)
  (if (< bound1 bound2)
      (max (min x bound2) bound1)
      (max (min x bound1) bound2)))

(defun num-digits (n)
  (+ (floor (log n 10)) 1))

(defun split-string (string delimiter)
  (loop
     for beg = 0 then (+ end 1)
     for end = (position delimiter string :start beg)
     then (position delimiter string :start (+ end 1))
     while beg
     collect (subseq string beg end)
     while end))

(defun vector-clone (v)
  (map 'vector #'identity v))

(defun vector-add (&rest vectors)
  (print vectors)
  (let ((ret (vector-clone (car vectors))))
    (loop
       for v in (subseq vectors 1)
       do (loop
             for i from 0 to 2
             do (incf (aref ret i) (aref v i))))
    ret))

(defun vector-subtract (&rest vectors)
  (let ((ret (vector-clone (car vectors))))
    (loop
       for v in (subseq vectors 1)
       do (loop
             for i from 0 to 2
             do (decf (aref ret i) (aref v i))))
    ret))

(defun vector-scale (c v)
  (map 'vector #'(lambda (n) (* n c)) v))

(defun vector-dot (v1 v2)
  (loop
     for i1 across v1
     for i2 across v2
     sum (* i1 i2)))

(defun vector-norm (v)
  (sqrt (loop
           for i across v
           sum (square i))))

(defun vector-normalize (v)
  (let ((v-norm (vector-norm v)))
    (if (= v-norm 0)
        v
        (vector-scale (/ 1 v-norm) v))))

(defun vector-cross-product (v1 v2)
  (vector (- (* (aref v1 1) (aref v2 2))
             (* (aref v1 2) (aref v2 1)))
          (- (* (aref v1 2) (aref v2 0))
             (* (aref v1 0) (aref v2 2)))
          (- (* (aref v1 0) (aref v2 1))
             (* (aref v1 1) (aref v2 0)))))

(defun triangle-normal (p1 p2 p3)
  (let* ((v1 (vector-subtract p1 p2))
         (v2 (vector-subtract p2 p3))
         (normal (vector-cross-product v1 v2)))
    (vector-normalize normal)))

(defun random-color ()
  (vector (random 256) (random 256) (random 256)))
