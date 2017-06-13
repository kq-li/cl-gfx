(in-package :graphics)

(defun plot-pixel (x y z &optional (color *foreground-color*) (debug nil))
  (let ((xround (round x))
        (yround (round y)))
    (when (and (and (>= xround 0)
                    (< xround *x-resolution*))
               (and (>= yround 0)
                    (< yround *y-resolution*)))
      (let ((pixel (aref *screen* (- *y-resolution* yround 1) xround)))
        (when debug
          (format t "~a ~a ~a ~a ~a~%" x y xround yround z))
        (when (or (not (cdr pixel))
                  (> z (cdr pixel)))
          (setf (aref *screen* (- *y-resolution* yround 1) xround)
                (cons (map 'vector #'(lambda (n) (min n 255)) color) z)))))))

(defun create-ppm (filename)
  (declare (type fixnum *x-resolution* *y-resolution*))
  (declare (type string filename))
  (declare (type (simple-array (simple-array fixnum (*)) (* *)) *screen*))
           
  (with-open-file (stream (concatenate 'string filename ".ppm")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((header (format nil "P3 ~d ~d 255 ~%" *x-resolution* *y-resolution*)))
      (loop
         for char across header
         do (write-char char stream))

      (loop
         for x from 0 below *x-resolution*
         do
           (loop
              for y from 0 below *y-resolution*
              do
                (let ((pixel (car (aref *screen* x y))))
                  (princ (aref pixel 0) stream)
                  (write-char #\space stream)
                  (princ (aref pixel 1) stream)
                  (write-char #\space stream)
                  (princ (aref pixel 2) stream)
                  (write-char #\space stream)))
           (terpri stream)))))

(defun create-png (filename)
  (create-ppm filename)
  (run-shell-command (format nil "convert ~a.ppm ~a.png" filename filename)))

(defun create (filename)
  (let* ((position (position #\. filename :from-end t))
         (prefix (if position (subseq filename 0 position) filename))
         (extension (if position (subseq filename position) nil)))
    (create-ppm prefix)
    (when (and extension
               (not (string= extension ".ppm")))
      (run-shell-command (format nil "convert ~a.ppm ~a" prefix filename)))))

(defun display ()
  (create "tmp.ppm")
  (run-shell-command "display tmp.ppm")
  (run-shell-command "rm tmp.ppm"))

(defun create-gif (filename)
  (run-shell-command (format nil "convert anim/~a* ~a.gif" filename filename)))

