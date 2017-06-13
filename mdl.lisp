(in-package :graphics)

(defvar *reserved-words* (make-hash-table :test 'equal))

(loop
   for (reserved . value) in '(("x" . "XYZ")
                               ("y" . "XYZ")
                               ("z" . "XYZ")
                               ("screen" . "SCREEN")
                               ("light" . "LIGHT")
                               ("constants" . "CONSTANTS")
                               ("save_coord_system" . "SAVE_COORDS")  
                               ("camera" . "CAMERA")
                               ("ambient" . "AMBIENT")  
                               ("torus" . "TORUS")
                               ("sphere" . "SPHERE")
                               ("box" . "BOX")  
                               ("line" . "LINE")
                               ("bezier" . "BEZIER")
                               ("hermite" . "HERMITE")  
                               ("circle" . "CIRCLE") 
                               ("mesh" . "MESH")
                               ("texture" . "TEXTURE")  
                               ("set" . "SET") 
                               ("move" . "MOVE")
                               ("scale" . "SCALE")
                               ("rotate" . "ROTATE")  
                               ("basename" . "BASENAME")
                               ("save_knobs" . "SAVE_KNOBS")  
                               ("tween" . "TWEEN")
                               ("frames" . "FRAMES")
                               ("vary" . "VARY")  
                               ("push" . "PUSH")
                               ("pop" . "POP")  
                               ("save" . "SAVE")
                               ("display" . "DISPLAY")  
                               ("generate_rayfiles" . "GENERATE_RAYFILES")  
                               ("shading" . "SHADING")  
                               ("phong" . "SHADING_TYPE")
                               ("flat" . "SHADING_TYPE")
                               ("goroud" . "SHADING_TYPE")
                               ("raytrace" . "SHADING_TYPE")
                               ("wireframe" . "SHADING_TYPE")  
                               ("set_knobs" . "SET_KNOBS")
                               ("focal" . "FOCAL")
                               ("web" . "WEB"))
   do (setf (gethash reserved *reserved-words*) value))

(define-string-lexer mdl-lexer
  ("[a-zA-Z_][.a-zA-Z0-9_]*"
   (let ((value (gethash $@ *reserved-words*)))
     (if value
         (return (values (intern value) $@))
         (return (values 'string $@)))))
  ("-?[0-9]*\\.[0-9]+" (return (values 'number (read-from-string $@))))
  ("-?[0-9]+\\.?" (return (values 'number (read-from-string $@))))
  ("//.*"))

(defvar *commands* (make-array 0 :fill-pointer 0 :adjustable t))
(defvar *symbols* (make-hash-table :test #'equal))

(defun sethash (hash value table)
  (setf (gethash hash table) value))

(defun add-command (&rest command)
  (vector-push-extend command *commands*))

(define-parser mdl-parser
  (:start-symbol mdl)
  (:terminals (frames basename vary constants ambient light shading shading_type push pop save display set set_knobs box sphere torus line move rotate scale xyz number string nil))

  (mdl
   (frames
    number
    #'add-command)

   (basename
    string
    #'add-command)

   (vary
    string number number number number
    #'(lambda (vary knob startframe endframe startvalue endvalue)
        (sethash knob '("knob" . 0) *symbols*)
        (add-command vary knob startframe endframe startvalue endvalue)))

   (constants
    string number number number number number number number number number
    #'add-command)

   (ambient
    number number number
    #'add-command)

   (light
    string number number number number number number
    #'(lambda (light name &rest args)
        (sethash name (cons "light" (apply 'vector args)) *symbols*)))

   (shading
    shading_type
    #'add-command)
   
   (push
    #'add-command)

   (pop
    #'add-command)

   (save
    #'(lambda (save)
        (add-command save nil)))
   
   (save
    string
    #'add-command)

   (display
    #'add-command)

   (set
    string number
    #'add-command)

   (set_knobs
    number
    #'add-command)
   
   (box
    number number number number number number
    #'add-command)

   (box
    string number number number number number number
    #'(lambda (box constants x y z l w h)
        (sethash constants '("constants" . nil) *symbols*)
        (add-command box x y z l w h constants)))

   (sphere
    number number number number
    #'add-command)

   (sphere
    string number number number number
    #'(lambda (sphere constants x y z r)
        (sethash constants '("constants" . nil) *symbols*)
        (add-command sphere x y z r constants)))

   (torus
    number number number number number
    #'add-command)

   (torus
    string number number number number number
    #'(lambda (torus constants x y z tr cr)
        (sethash constants '("constants" . nil) *symbols*)
        (add-command torus x y z tr cr constants)))

   (line
    number number number number number number
    #'add-command)
   
   (circle
    number number number
    #'add-command)
   
   (hermite
    number number number number number number number number
    #'add-command)
   
   (bezier
    number number number number number number number number
    #'add-command)

   (move
    number number number
    #'(lambda (move x y z)
        (add-command move x y z nil)))

   (move
    number number number string
    #'(lambda (move x y z knob)
        (sethash knob '("knob" . 0) *symbols*)
        (add-command move x y z knob)))
   
   (rotate
    xyz number
    #'(lambda (rotate axis angle)
        (add-command rotate axis angle nil)))

   (rotate
    xyz number string
    #'(lambda (rotate axis angle knob)
        (sethash knob '("knob" . 0) *symbols*)
        (add-command rotate axis angle knob)))
   
   (scale
    number number number
    #'(lambda (scale x y z)
        (add-command scale x y z nil)))

   (scale
    number number number string
    #'(lambda (scale x y z knob)
        (sethash knob '("knob" . 0) *symbols*)
        (add-command scale x y z knob)))

   nil)

  (arguments (alphanum #'list)
             (alphanum arguments #'(lambda (alphanum arguments)
                                     (cons alphanum arguments))))
  
  (alphanum number
            string)

  (nil))

(defvar *frame-count* 1)
(defvar *basename* "pic")
(defvar *frames* nil)

(defun animation-pass (&optional (commands *commands*) (symbols *symbols*))
  (format t "Animation pass...~%")
  (let (vary
        frames
        basename)
    (loop
       for line across commands
       do
         (let ((command (car line))
               (arguments (cdr line)))
           (cond
             ((string-equal command "vary")
              (setf vary t))
             ((string-equal command "frames")
              (setf frames t)
              (setf *frame-count* (car arguments))
              (setf *frames*
                    (make-array *frame-count*
                                :initial-contents
                                (loop
                                   for i from 0 below *frame-count*
                                   collect (make-hash-table :test 'equal)))))
             ((string-equal command "basename")
              (setf basename t)
              (setf *basename* (car arguments))))))
    (when (and vary (not frames))
      (error "Vary used without specifying frames!"))))

(defun vary-pass (&optional (commands *commands*) (symbols *symbols*))
  (format t "Vary pass...~%")
  (loop
     for line across commands
     do
       (let ((command (car line))
             (arguments (cdr line)))
         (when (string-equal command "vary")
           (multiple-value-bind (knob startframe endframe startvalue endvalue)
               (values-list arguments)
             (loop
                for frame from 0 below *frame-count*
                for knob-table across *frames*
                with value = startvalue
                with interval = (/ (- endvalue startvalue)
                                   (+ (- endframe startframe) 1))
                when (and (>= frame startframe)
                          (<= frame endframe))
                do
                  (incf value interval)
                  (sethash knob value knob-table)))))))

(defun lighting-pass (&optional (commands *commands*) (symbols *symbols*))
  (format t "Lighting pass...~%")
  (loop
     for line across commands
     do
       (let ((command (car line))
             (arguments (cdr line)))
         (cond
           ((string-equal command "shading")
            (setf *shading-type* (car arguments)))
           ((string-equal command "constants")
            (let ((name (car arguments))
                  (constants (cdr arguments)))
              (sethash name (cons "constants" (apply 'vector constants)) symbols)))
           ((string-equal command "ambient")
            (setf *ambient* (apply 'vector arguments)))))))

(defun parse-commands (&optional (commands *commands*) (symbols *symbols*))
  (format t "Parsing...~%")
  (loop
     for line across commands
     do
       (let ((command (car line))
             (arguments (cdr line)))
         (cond
           ((string-equal command "push")
            (push-coordinate-system))
           ((string-equal command "pop")
            (pop-coordinate-system))
           ((string-equal command "save")
            (let ((filename (if (= (length arguments) 1)
                                (string-trim '(#\Space #\Return) (car arguments))
                                (concatenate 'string *basename* ".png"))))
              (format t "Saving ~a...~%" filename)
              (create filename)
              (format t "Saved ~a~%" filename)))
           ((string-equal command "display")
            (display))
           ((string-equal command "set")
            (let ((knob (first arguments))
                  (value (second arguments)))
              (sethash knob `("knob" . ,value) *symbols*)))
           ((string-equal command "set_knobs")
            (let ((value (first arguments)))
              (loop
                 for knob being the hash-keys of *symbols* using (hash-value v)
                 when (string-equal (car v) "knob")
                 do (sethash knob `("knob" . ,value) *symbols*))))
           ((string-equal command "box")
            (apply 'add-box (subseq arguments 0 6))
            (draw-triangles (nth 6 arguments))
            (clear-triangle-matrix))
           ((string-equal command "sphere")
            (apply 'add-sphere (subseq arguments 0 4))
            (draw-triangles (nth 4 arguments))
            (clear-triangle-matrix))
           ((string-equal command "torus")
            (apply 'add-torus (subseq arguments 0 5))
            (draw-triangles (nth 5 arguments))
            (clear-triangle-matrix))
           ((string-equal command "line")
            (let* ((point1 (apply 'vector (subseq arguments 0 3)))
                   (point2 (apply 'vector (subseq arguments 3 6))))
              (add-edge point1 point2))
            (draw-edges)
            (clear-edge-matrix))
           ((string-equal command "circle")
            (apply 'add-circle (subseq arguments 0 3))
            (draw-edges)
            (clear-edge-matrix))
           ((string-equal command "hermite")
            (apply 'add-hermite-curve (subseq arguments 0 8))
            (draw-edges)
            (clear-edge-matrix))
           ((string-equal command "bezier")
            (apply 'add-bezier-curve (subseq arguments 0 8))
            (draw-edges)
            (clear-edge-matrix))
           ((string-equal command "move")
            (multiple-value-bind (x y z knob)
                (values-list arguments)
              (if knob
                  (let* ((value (cdr (gethash knob *symbols*)))
                         (matrix (apply
                                  'make-translation-matrix
                                  (mapcar #'(lambda (n) (* value n))
                                          (list x y z)))))
                    (set-coordinate-system
                     (matrix-matrix-mult (get-coordinate-system) matrix)))
                  (let ((matrix (make-translation-matrix x y z)))
                    (set-coordinate-system
                     (matrix-matrix-mult (get-coordinate-system) matrix))))))
           ((string-equal command "rotate")
            (multiple-value-bind (axis angle knob)
                (values-list arguments)
              (if knob
                  (let* ((value (cdr (gethash knob *symbols*)))
                         (matrix (make-rotation-matrix axis (* angle value))))
                    (set-coordinate-system
                     (matrix-matrix-mult (get-coordinate-system) matrix)))
                  (let ((matrix (make-rotation-matrix axis angle)))
                    (set-coordinate-system
                     (matrix-matrix-mult (get-coordinate-system) matrix))))))
           ((string-equal command "scale")
            (multiple-value-bind (x y z knob)
                (values-list arguments)
              (if knob
                  (let* ((value (cdr (gethash knob *symbols*)))
                         (matrix (apply
                                  'make-scale-matrix
                                  (mapcar #'(lambda (n) (* value n))
                                          (list x y z)))))
                    (set-coordinate-system
                     (matrix-matrix-mult (get-coordinate-system) matrix)))
                  (let ((matrix (make-scale-matrix x y z)))
                    (set-coordinate-system
                     (matrix-matrix-mult (get-coordinate-system) matrix))))))))))

(defun parse-file (filename)
  (with-open-file (stream filename)
    (loop
       for line = (read-line stream nil)
       while line
       do
         (parse-with-lexer (mdl-lexer line) mdl-parser)))
  
  (animation-pass)
  (vary-pass)
  (lighting-pass)
  
  (without-floating-point-underflow
      (cond
        ((= *frame-count* 1) (parse-commands))
        (t (ensure-directories-exist "anim/")
           (run-shell-command (format nil "rm -rf anim/~a*" *basename*))
           (loop
              for frame from 0 below *frame-count*
              do
                (loop
                   with knob-table = (aref *frames* frame)
                   for knob being the hash-keys of knob-table using (hash-value value)
                   do
                     (sethash knob `("knob" . ,value) *symbols*))
                (parse-commands)
                (let ((filename (format nil (format nil "anim/~a~~~d,'0d.ppm"
                                                    *basename*
                                                    (num-digits (- *frame-count* 1)))
                                        frame)))
                  (format t "Saving ~a...~%" filename)
                  (create filename)
                  (format t "Saved ~a~%" filename))
                (clear-edge-matrix)
                (clear-triangle-matrix)
                (clear-coordinate-systems)
                (clear-screen))
           (create-gif *basename*)
           (format t "Saved ~a.gif~%" *basename*)))))
  
