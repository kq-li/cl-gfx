(in-package :graphics)

(defclass matrix ()
  ((array :accessor matrix-array
          :initform (make-array '(0 0) :adjustable t :element-type 'number))))

(defun make-matrix (&key initial-contents (dimensions '(4 0)))
  (declare (type list initial-contents))
  (declare (type list dimensions))
  (let ((matrix (make-instance 'matrix)))
    (cond
      (initial-contents
       (adjust-array (matrix-array matrix)
                     (list (length initial-contents)
                           (length (first initial-contents)))
                     :initial-contents initial-contents))
      (dimensions
       (adjust-array (matrix-array matrix)
                     dimensions
                     :initial-element 0)))
    matrix))

(defun make-identity-matrix (&optional (size 4))
  (declare (type integer size))
  (make-matrix :initial-contents
               (loop
                  for i from 0 to (- size 1)
                  collect (loop for j from 0 to (- size 1) collect (if (= i j) 1 0)))))

(defmethod matrix-dimensions ((matrix matrix))
  (array-dimensions (matrix-array matrix)))

(defmethod matrix-dimension ((matrix matrix) (n integer))
  (array-dimension (matrix-array matrix) n))

(defmethod matrix-get-element ((matrix matrix) (row integer) (col integer))
  (aref (matrix-array matrix) row col))

(defmethod matrix-get-row ((matrix matrix) (row integer))
  (let ((cols (matrix-dimension matrix 1)))
    (make-array cols
                :initial-contents (loop
                                     for col from 0 to (- cols 1)
                                     collect (matrix-get-element matrix row col)))))

(defmethod matrix-get-column ((matrix matrix) (col integer))
  (let ((rows (matrix-dimension matrix 0)))
    (make-array rows
                :initial-contents (loop
                                     for row from 0 to (- rows 1)
                                     collect (matrix-get-element matrix row col)))))
  
(defmethod matrix-set-element ((matrix matrix) (row integer) (col integer) (value number))
  (setf (aref (matrix-array matrix) row col) value))

(defmethod matrix-set-row ((matrix matrix) (row integer) (new-row list))
  (loop
     for num in new-row
     for col from 0
     do (matrix-set-element matrix row col num)))

(defmethod matrix-set-row ((matrix matrix) (row integer) (new-row array))
  (loop
     for col from 0 to (- (matrix-dimension matrix 1) 1)
     do (matrix-set-element matrix row col (aref new-row col))))

(defmethod matrix-set-column ((matrix matrix) (col integer) (new-col list))
  (loop
     for num in new-col
     for row from 0
     do (matrix-set-element matrix row col num)))

(defmethod matrix-set-column ((matrix matrix) (col integer) (new-column array))
  (loop
     for row from 0 to (- (matrix-dimension matrix 0) 1)
     do (matrix-set-element matrix row col (aref new-column row))))

(defmethod matrix-add-row ((matrix matrix) (new-row list))
  (adjust-array (matrix-array matrix)
                (list (+ (matrix-dimension matrix 0) 1)
                      (matrix-dimension matrix 1)))
  (loop
     for num in new-row
     for col from 0 to (- (matrix-dimension matrix 1) 1)
     do (matrix-set-element matrix (- (matrix-dimension matrix 0) 1) col num)))

(defmethod matrix-add-row ((matrix matrix) (new-row array))
  (adjust-array (matrix-array matrix)
                (list (+ (matrix-dimension matrix 0) 1)
                      (matrix-dimension matrix 1)))
  (loop
     for col from 0 to (- (matrix-dimension matrix 1) 1)
     do (matrix-set-element matrix
                            (- (matrix-dimension matrix 0) 1)
                            col
                            (aref new-row col))))

(defmethod matrix-add-column ((matrix matrix) (new-col list))
  (adjust-array (matrix-array matrix)
                (list (matrix-dimension matrix 0)
                      (+ (matrix-dimension matrix 1) 1)))
  (loop
     for num in new-col
     for row from 0 to (- (matrix-dimension matrix 0) 1)
     do (matrix-set-element matrix row (- (matrix-dimension matrix 1) 1) num)))

(defmethod matrix-add-column ((matrix matrix) (new-col array))
  (adjust-array (matrix-array matrix)
                (list (matrix-dimension matrix 0)
                      (+ (matrix-dimension matrix 1) 1)))
  (loop
     for row from 0 to (- (matrix-dimension matrix 0) 1)
     do (matrix-set-element matrix
                            row
                            (- (matrix-dimension matrix 1) 1)
                            (aref new-col row))))

(defmethod matrix-append ((matrix1 matrix) (matrix2 matrix))
  (unless (= (matrix-dimension matrix1 0) (matrix-dimension matrix2 0))
    (error
     (format nil
             "Cannot append matrix ~{~a~&x~} matrix to ~{~a~^x~} matrix"
             (matrix-dimensions matrix2)
             (matrix-dimensions matrix1))))
  (loop
     for col from 0 to (- (matrix-dimension matrix2 1) 1)
     do (matrix-add-column matrix1 (matrix-get-column matrix2 col))))
    
(defmacro matrix-funcall (matrix element-function &key before-row-function after-row-function)
  `(loop
      for row from 0 to (- (matrix-dimension ,matrix 0) 1)
      do
        (when ,before-row-function
          (,before-row-function row))
        (loop
           for col from 0 to (- (matrix-dimension ,matrix 1) 1)
           do (,element-function row col))
        (when ,after-row-function
          (,after-row-function row))))

(defmethod matrix-copy ((matrix matrix))
  (let ((new-matrix (make-matrix :dimensions (matrix-dimensions matrix))))
    (matrix-funcall new-matrix
                    (lambda (row col)
                      (matrix-set-element new-matrix row col
                                          (matrix-get-element matrix row col))))
    new-matrix))
       
(defmethod matrix-scalar-mult ((matrix matrix) (scalar number))
  (let ((new-matrix (make-matrix :dimensions (matrix-dimensions matrix))))
    (matrix-funcall new-matrix
                    (lambda (row col)
                      (matrix-set-element new-matrix row col
                                          (* scalar
                                             (matrix-get-element matrix row col)))))
    new-matrix))

(defmethod matrix-matrix-mult ((matrix1 matrix) (matrix2 matrix))
  (unless (= (matrix-dimension matrix1 1) (matrix-dimension matrix2 0))
    (error
     (format nil
             "Cannot multiply matrices of dimensions ~{~a~^x~} and ~{~a~^x~}"
             (matrix-dimensions matrix1)
             (matrix-dimensions matrix2))))
  (let ((new-matrix (make-matrix :dimensions (list (matrix-dimension matrix1 0)
                                                   (matrix-dimension matrix2 1)))))
    (loop for i from 0 to (- (matrix-dimension matrix1 0) 1)
       do (loop for j from 0 to (- (matrix-dimension matrix2 1) 1)
             do (loop for k from 0 to (- (matrix-dimension matrix1 1) 1)
                   do (matrix-set-element new-matrix i j
                                          (+ (matrix-get-element new-matrix i j)
                                             (* (matrix-get-element matrix1 i k)
                                                (matrix-get-element matrix2 k j)))))))
    new-matrix))
    ;;(matrix-funcall new-matrix
                    ;;(lambda (row col)
                      ;;(loop
                         ;;for k from 0 to (- (matrix-dimension matrix1 1) 1)
                         ;;do
                           ;;(matrix-set-element new-matrix row col
                                               ;;(+ (matrix-get-element new-matrix row col)
                                                  ;;(* (matrix-get-element matrix1 row k)
                                                     ;;(matrix-get-element matrix2 k col)))))))
    ;;new-matrix))

(defmethod matrix-string ((matrix matrix))
  (with-output-to-string (stream)
    (matrix-funcall matrix
                    (lambda (row col)
                      (format stream "~a " (matrix-get-element matrix row col)))
                    :before-row-function
                    (lambda (row)
                      (format stream "[ "))
                    :after-row-function
                    (lambda (row)
                      (format stream "]~%")))))
    

(defmethod matrix-print ((matrix matrix))
  (format t "~a" (matrix-string matrix)))

(defmethod print-object ((matrix matrix) stream)
  (format stream "~%~a" (matrix-string matrix)))

(defmacro matrix-test (operation &optional description)
  `(progn
     (when ,description
       (format t ,description))
     (terpri)
     (matrix-print ,operation)
     (terpri)))

(defun make-translation-matrix (disp-x disp-y disp-z)
  (let ((matrix (make-identity-matrix)))
    (matrix-set-element matrix 0 3 disp-x)
    (matrix-set-element matrix 1 3 disp-y)
    (matrix-set-element matrix 2 3 disp-z)
    matrix))

(defun make-scale-matrix (scale-x scale-y scale-z)
  (let ((matrix (make-identity-matrix)))
    (matrix-set-element matrix 0 0 scale-x)
    (matrix-set-element matrix 1 1 scale-y)
    (matrix-set-element matrix 2 2 scale-z)
    matrix))

(defun make-rotation-matrix (axis angle)
  (let ((matrix (make-identity-matrix))
        (angle-rad (/ (* angle PI) 180)))
    (cond
      ((string-equal axis "x")
       (matrix-set-element matrix 1 1 (cos angle-rad))
       (matrix-set-element matrix 1 2 (- (sin angle-rad)))
       (matrix-set-element matrix 2 1 (sin angle-rad))
       (matrix-set-element matrix 2 2 (cos angle-rad)))
      ((string-equal axis "y")
       (matrix-set-element matrix 0 0 (cos angle-rad))
       (matrix-set-element matrix 0 2 (sin angle-rad))
       (matrix-set-element matrix 2 0 (- (sin angle-rad)))
       (matrix-set-element matrix 2 2 (cos angle-rad)))
      ((string-equal axis "z")
       (matrix-set-element matrix 0 0 (cos angle-rad))
       (matrix-set-element matrix 0 1 (- (sin angle-rad)))
       (matrix-set-element matrix 1 0 (sin angle-rad))
       (matrix-set-element matrix 1 1 (cos angle-rad))))
    matrix))
