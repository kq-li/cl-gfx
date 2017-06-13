(in-package :graphics)

(defun generate-parametric-curve (x-function y-function z-function
                                  &key (max-step *max-s-step*))
  (let ((matrix (make-matrix :dimensions '(4 0))))
    (loop
       for s from 0 to max-step
       do
         (add-point `#(,@(mapcar (lambda (f)
                                   (funcall f (/ s max-step)))
                                 (list x-function y-function z-function)))
                    :matrix matrix))
    matrix))

(defun generate-circle (x y z r)
  (generate-parametric-curve (lambda (s) (+ x (* r (cos (* 2 PI s)))))
                             (lambda (s) (+ y (* r (sin (* 2 PI s)))))
                             (lambda (s) 0)))

(defun add-circle (x y z r)
  (let ((circle (generate-circle x y z r)))
    (loop
       for i from 0 below (- (matrix-dimension circle 1) 1)
       do (add-edge (matrix-get-column circle i)
                    (matrix-get-column circle (+ i 1))))))

(defun generate-hermite-curve (x0 y0 x1 y1 rx0 ry0 rx1 ry1)
  (generate-parametric-curve
   (lambda (s)
     (let* ((H-inverse (make-matrix :initial-contents
                                    '((  2 -2  1  1  )
                                      ( -3  3 -2 -1  )
                                      (  0  0  1  0  )
                                      (  1  0  0  0  ))))
            (G (make-matrix :initial-contents `((,x0) (,x1) (,rx0) (,rx1))))
            (C (matrix-matrix-mult h-inverse G)))
       (loop
          for i from 0 to 3
          sum (* (matrix-get-element C i 0) (expt s (- 3 i))))))
   (lambda (s)
     (let* ((H-inverse (make-matrix :initial-contents
                                    '((  2 -2  1  1  )
                                      ( -3  3 -2 -1  )
                                      (  0  0  1  0  )
                                      (  1  0  0  0  ))))
            (G (make-matrix :initial-contents `((,y0) (,y1) (,ry0) (,ry1))))
            (C (matrix-matrix-mult h-inverse G)))
       (loop
          for i from 0 to 3
          sum (* (matrix-get-element C i 0) (expt s (- 3 i))))))
   (lambda (s) 0)))

(defun add-hermite-curve (x0 y0 x1 y1 rx0 ry0 rx1 ry1)
  (let ((hermite (generate-hermite-curve x0 y0 x1 y1 rx0 ry0 rx1 ry1)))
    (loop
       for i from 0 below (- (matrix-dimension hermite 1) 1)
       do (add-edge (matrix-get-column hermite i)
                    (matrix-get-column hermite (+ i 1))))))

(defun generate-bezier-curve (x0 y0 x1 y1 x2 y2 x3 y3)
  (generate-parametric-curve
   (lambda (s)
     (let* ((H-inverse (make-matrix :initial-contents
                                    '(( -1  3 -3  1  )
                                      (  3 -6  3  0  )
                                      ( -3  3  0  0  )
                                      (  1  0  0  0  ))))
            (G (make-matrix :initial-contents `((,x0) (,x1) (,x2) (,x3))))
            (C (matrix-matrix-mult h-inverse G)))
       (loop
          for i from 0 to 3
          sum (* (matrix-get-element C i 0) (expt s (- 3 i))))))
   (lambda (s)
     (let* ((H-inverse (make-matrix :initial-contents
                                    '(( -1  3 -3  1  )
                                      (  3 -6  3  0  )
                                      ( -3  3  0  0  )
                                      (  1  0  0  0  ))))
            (G (make-matrix :initial-contents `((,y0) (,y1) (,y2) (,y3))))
            (C (matrix-matrix-mult h-inverse G)))
       (loop
          for i from 0 to 3
          sum (* (matrix-get-element C i 0) (expt s (- 3 i))))))
   (lambda (s) 0)))     

(defun add-bezier-curve (x0 y0 x1 y1 x2 y2 x3 y3)
  (let ((bezier (generate-bezier-curve x0 y0 x1 y1 x2 y2 x3 y3)))
    (loop
       for i from 0 below (- (matrix-dimension bezier 1) 1)
       do (add-edge (matrix-get-column bezier i)
                    (matrix-get-column bezier (+ i 1))))))

(defun add-triangle (point1 point2 point3 &key (matrix *triangle-matrix*))
  (add-point point1 :matrix matrix)
  (add-point point2 :matrix matrix)
  (add-point point3 :matrix matrix))

(defun add-box (x y z width height depth &key (matrix *triangle-matrix*))
  (let ((vertices (make-matrix :dimensions '(4 0))))
    (add-xyz x y (- z depth) :matrix vertices)
    (add-xyz x y z :matrix vertices)
    (add-xyz x (- y height) z :matrix vertices)
    (add-xyz x (- y height) (- z depth) :matrix vertices)
    (add-xyz (+ x width) (- y height) (- z depth) :matrix vertices)
    (add-xyz (+ x width) (- y height) z :matrix vertices)
    (add-xyz (+ x width) y z :matrix vertices)
    (add-xyz (+ x width) y (- z depth) :matrix vertices)
    (loop
       for (first second third) in '(
                                     (0 3 2) (2 1 0) ;; left
                                     (3 4 5) (5 2 3) ;; top
                                     (4 7 6) (6 5 4) ;; right
                                     (7 0 1) (1 6 7) ;; bottom
                                     (1 2 5) (5 6 1) ;; front
                                     (0 7 4) (4 3 0) ;; back
                                     )
       do
         (add-triangle (matrix-get-column vertices first)
                       (matrix-get-column vertices second)
                       (matrix-get-column vertices third) :matrix matrix))))

(defun generate-nested-parametric-curve (x-function y-function z-function
                                         &key
                                           (max-u-step *max-u-step*)
                                           (max-v-step *max-v-step*))
  (let ((matrix (make-matrix :dimensions '(4 0))))
    (loop
       for u from 0 to max-u-step
       do
         (loop
            for v from 0 to max-v-step
            do
              (let ((point `#(,@(mapcar (lambda (f)
                                          (funcall f
                                                   (/ u max-u-step)
                                                   (/ v max-v-step)))
                                        (list x-function y-function z-function)))))
                (add-point point :matrix matrix))))
    matrix))

(defun generate-sphere (x y z r)
  (generate-nested-parametric-curve
   (lambda (u v)
     (+ x (* r (cos (* PI u)))))
   (lambda (u v)
     (+ y (* r (sin (* PI u)) (sin (* 2 PI v)))))
   (lambda (u v)
     (+ z (* r (sin (* PI u)) (cos (* 2 PI v)))))))

(defun add-sphere (x y z r &key (matrix *triangle-matrix*))
  (let* ((n *max-u-step*)
         (sphere (generate-sphere x y z r)))
    (loop
       for i from 0 below (- (matrix-dimension sphere 1) n 1)
       do
         (let ((triangle1 (list i (+ i 1) (+ i n 1)))
               (triangle2 (list i (+ i n 1) (+ i n))))
           (apply (lambda (v1 v2 v3) (add-triangle v1 v2 v3 :matrix matrix))
                  (loop
                     for vertex in triangle1
                     collect (matrix-get-column sphere vertex)))
           (apply (lambda (v1 v2 v3) (add-triangle v1 v2 v3 :matrix matrix))
                  (loop
                     for vertex in triangle2
                     collect (matrix-get-column sphere vertex)))))))

(defun generate-torus (x y z cr tr)
  (generate-nested-parametric-curve
   (lambda (u v)
     (let ((A (* 2 PI u))
           (B (* 2 PI v)))
       (+ x (* cr (cos A) (cos B)) (* tr (cos B)))))
   (lambda (u v)
     (let ((A (* 2 PI u))
           (B (* 2 PI v)))
       (+ y (* cr (sin A)))))
   (lambda (u v)
     (let ((A (* 2 PI u))
           (B (* 2 PI v)))
       (+ z (* -1 cr (cos A) (sin B)) (* -1 tr (sin B)))))))

(defun add-torus (x y z cr tr &key (matrix *triangle-matrix*))
  (let ((n *max-u-step*)
        (torus (generate-torus x y z cr tr)))
    (loop
       for i from 0 below (- (matrix-dimension torus 1) n 1)
       do
         (let ((triangle1 (list i (+ i 1) (+ i n 1)))
               (triangle2 (list i (+ i n 1) (+ i n))))
           (apply (lambda (v1 v2 v3) (add-triangle v1 v2 v3 :matrix matrix))
                  (loop
                     for vertex in triangle1
                     collect (matrix-get-column torus vertex)))
           (apply (lambda (v1 v2 v3) (add-triangle v1 v2 v3 :matrix matrix))
                  (loop
                     for vertex in triangle2
                     collect (matrix-get-column torus vertex)))))))
