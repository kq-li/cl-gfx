(in-package :graphics)

(defvar *max-s-step* 100)
(defvar *max-u-step* 10)
(defvar *max-v-step* 10)

(defun add-point (point &key matrix)
  (when (= (length point) 3)
    (setf point (apply 'vector
                       (append (loop for i across point collect i) '(1.0)))))
  (matrix-add-column matrix point))

(defun add-xyz (x y z &key matrix)
  (matrix-add-column matrix (vector x y z 1.0)))

(defun add-edge (point1 point2 &key (matrix *edge-matrix*))
  (add-point point1 :matrix matrix)
  (add-point point2 :matrix matrix))

(defun add-edges (matrix)
  (matrix-append *edge-matrix* matrix))

(defun clear-edge-matrix ()
  (setf *edge-matrix* (make-matrix :dimensions '(4 0))))

(defun clear-triangle-matrix ()
  (setf *triangle-matrix* (make-matrix :dimensions '(4 0))))

(defun clear-coordinate-systems ()
  (setf *coordinate-systems* (list (make-identity-matrix))))

(defun clear-screen ()
  (setf *screen* (make-array (list *x-resolution* *y-resolution*)
                             :initial-element (cons *background-color* nil))))

(defun ambient-illumination (A Ka)
  (round (bound (* A Ka) 0 255)))

(defun diffuse-illumination (L N R Kd)
  (round (bound (* L Kd (vector-dot N R)) 0 255)))

(defun specular-illumination (L N R V Ks)
  (round (bound (* L Ks (vector-dot (vector-subtract
                                     (vector-scale (* 2 (vector-dot N R)) N) R) V))
                0 255)))

(defun total-illumination (N V constant-name)
  (multiple-value-bind (kar kdr ksr kag kdg ksg kab kdb ksb)
      (values-list (loop
                      for n across (cdr (gethash constant-name *symbols*))
                      collect n))
    (let ((Ir 0)
          (Ig 0)
          (Ib 0))
      (let ((Iar (ambient-illumination (aref *ambient* 0) kar))
            (Iag (ambient-illumination (aref *ambient* 1) kag))
            (Iab (ambient-illumination (aref *ambient* 2) kab))
            (Idr 0)
            (Idg 0)
            (Idb 0)
            (Isr 0)
            (Isg 0)
            (Isb 0))
        ;;(format t "Ambient: ~a ~a ~a~%" Iar Iag Iab)
        (incf Ir Iar)
        (incf Ig Iag)
        (incf Ib Iab)

        (loop
           for key being the hash-keys of *symbols* using (hash-value value)
           when (string-equal (car value) "light")
           do
             (let* ((light (cdr value))
                    (R (vector-normalize (subseq light 0 3))))
               (incf Idr (diffuse-illumination (aref light 3) N R kdr))
               (incf Idg (diffuse-illumination (aref light 4) N R kdg))
               (incf Idb (diffuse-illumination (aref light 5) N R kdb))
               (incf Isr (specular-illumination (aref light 3) N R V ksr))
               (incf Isg (specular-illumination (aref light 4) N R V ksg))
               (incf Isb (specular-illumination (aref light 5) N R V ksb))))
        
        ;;(format t "Diffuse: ~a ~a ~a~%" Idr Idg Idb)
        ;;(format t "Specular: ~a ~a ~a~%" Isr Isg Isb)
        (incf Ir (+ Idr Isr))
        (incf Ig (+ Idg Isg))
        (incf Ib (+ Idb Isb))
        ;;(format t "Total: ~a ~a ~a~%" Ir Ig Ib)
        (apply 'vector (mapcar #'(lambda (n) (round (bound n 0 255))) (list Ir Ig Ib)))))))

(defun draw-triangles (&optional (constant-name nil))
  (setf *triangle-matrix* (matrix-matrix-mult (get-coordinate-system) *triangle-matrix*))
  (loop
     for i from 0 below (matrix-dimension *triangle-matrix* 1) by 3
     do
       (let* ((point1 (matrix-get-column *triangle-matrix* i))
              (point2 (matrix-get-column *triangle-matrix* (+ i 1)))
              (point3 (matrix-get-column *triangle-matrix* (+ i 2)))
              (normal (triangle-normal point1 point2 point3))
              color)
         (when (> (aref normal 2) 0)
           (setf color (if constant-name
                           (total-illumination normal *view-vector* constant-name)
                           *foreground-color*))
           (draw-triangle point1 point2 point3 color)))))                          

(defun draw-triangle (point1 point2 point3 &optional (color *foreground-color*))
  (cond
    ((string-equal *shading-type* "wireframe")
     (draw-line point1 point2)
     (draw-line point2 point3)
     (draw-line point1 point3))
    ((string-equal *shading-type* "flat")
     (scan-line point1 point2 point3 color))))

(defun scan-line (point1 point2 point3 &optional (color *foreground-color*))
  (multiple-value-bind (bottom middle top)
      (values-list (sort (list point1 point2 point3) #'< :key #'(lambda (point)
                                                                  (aref point 1))))
    (let* ((Bx (aref bottom 0))
           (By (round (aref bottom 1)))
           (Bz (aref bottom 2))
           (Mx (aref middle 0))
           (My (round (aref middle 1)))
           (Mz (aref middle 2))
           (Tx (aref top 0))
           (Ty (round (aref top 1)))
           (Tz (aref top 2))
           (dx0 (if (= Ty By)
                    0
                    (/ (- Tx Bx) (- Ty By))))
           (dx1 (if (= My By)
                    0
                    (/ (- Mx Bx) (- My By))))
           (dz0 (if (= Ty By)
                    0
                    (/ (- Tz Bz) (- Ty By))))
           (dz1 (if (= My By)
                    0
                    (/ (- Mz Bz) (- My By))))
           (mode 0))
      (loop
         for x0 = Bx then (+ x0 dx0)
         for x1 = Bx then (+ x1 dx1)
         for y from By to Ty
         for z0 = Bz then (+ z0 dz0)
         for z1 = Bz then (+ z1 dz1)
         for p0 = (vector (round x0) y z0)
         for p1 = (vector (round x1) y z1)
           
         do
           (draw-line p0 p1 color)
           
         when (and (= mode 0) (>= y My))
         do
           (setf mode 1)
           (setf x1 Mx)
           (setf z1 Mz)
           (setf dx1 (if (= Ty My)
                         0
                         (/ (- Tx Mx) (- Ty My))))
           (setf dz1 (if (= Ty My)
                         0
                         (/ (- Tz Mz) (- Ty My))))))))

(defun draw-edges (&key (matrix *edge-matrix*) color-function)
  (setf matrix (matrix-matrix-mult (get-coordinate-system) matrix))
  (loop
     for i from 0 below (matrix-dimension matrix 1) by 2
     do
       (let ((point1 (matrix-get-column matrix i))
             (point2 (matrix-get-column matrix (+ i 1))))
         (draw-line point1 point2 :color-function color-function))))

(defun push-coordinate-system ()
  (setf *coordinate-systems* (cons (matrix-copy (car *coordinate-systems*)) *coordinate-systems*)))

(defun pop-coordinate-system ()
  (setf *coordinate-systems* (cdr *coordinate-systems*)))

(defun get-coordinate-system ()
  (car *coordinate-systems*))

(defun set-coordinate-system (system)
  (setf (car *coordinate-systems*) system))

(defun draw-line (point1 point2 &optional (color *foreground-color*))
  (let ((x1 (aref point1 0))
        (y1 (aref point1 1))
        (z1 (aref point1 2))
        (x2 (aref point2 0))
        (y2 (aref point2 1))
        (z2 (aref point2 2)))
    (if (< x2 x1)
        (draw-line point2 point1 color)
        (let* ((dx (- x2 x1))
               (dy (- y2 y1))
               (dz (- z2 z1))
               (A dy)
               (B (- dx)))
          (cond
            ((= dx dy 0)
             (plot-pixel x1 y1 z1 color)
             (plot-pixel x2 y2 z2 color))
            ((= dx 0)
             (loop
                with x = x1
                for y from y1 to y2
                for z from z1 by (/ dz dy)
                do (plot-pixel x y z color)))
            ((= dy 0)
             (loop
                for x from x1 to x2
                with y = y1
                for z from z1 by (/ dz dx)
                do (plot-pixel x y z color)))
            ((and (>= dy 0) (< dy dx)) 
             (loop
                for x from x1 to x2
                with y = y1
                for z from z1 by (/ dz dx)
                for d = (+ (* A 2) B) then (+ d (* A 2))

                do (plot-pixel x y z color)
                  
                when (> d 0)
                do
                  (incf y)
                  (incf d (* B 2))))
            ((>= dy dx)
             (loop
                with x = x1
                for y from y1 to y2
                for z from z1 by (/ dz dy)
                for d = (+ A (* B 2)) then (+ d (* B 2))

                do (plot-pixel x y z color)

                when (< d 0)
                do
                  (incf x)
                  (incf d (* A 2))))
            ((and (< dy 0) (<= (- dy) dx))
             (loop
                for x from x1 to x2
                with y = y1
                for z from z1 by (/ dz dx)
                for d = (- (* A 2) B) then (+ d (* A 2))

                do (plot-pixel x y z color)

                when (< d 0)
                do
                  (decf y)
                  (decf d (* B 2))))
            ((> (- dy) dx)
             (loop
                with x = x1
                for y from y1 downto y2
                for z from z1 by (/ dz dy)
                for d = (- A (* B 2)) then (- d (* B 2))

                do (plot-pixel x y z color)

                when (> d 0)
                do
                  (incf x)
                  (incf d (* A 2)))))))))
