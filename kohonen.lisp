(defun draw-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

(defclass kohonen-window (glut:window)
  ((km :accessor km :initarg :km :initform (error "No Kohonen Map privided")))
  (:default-initargs
   :width 400 :height 400 :pos-x 100 :pos-y 100
   :mode '(:single :rgb) :title "Kohonen Example"))

(defmethod glut:display-window :before ((w kohonen-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defmethod glut:display ((w kohonen-window))
  (gl:clear :color-buffer-bit)
   ;; Select green for all lines.
  (gl:color 0 1 0)
  (let* ((kohonen-map (km w))
         (nodes (nodes kohonen-map)))
    (do ((i 1 (+ i 1)))
        ((= i (number-of-nodes kohonen-map)))
      (let ((current (aref nodes i))
            (last (aref nodes (- i 1))))
            (draw-line (x last) (y last) (x current) (y current))))
  ;; Select red for all nodes
    (gl:color 1 0 0)
    (do ((i 0 (+ i 1)))
        ((= i (number-of-nodes kohonen-map)))
      (let ((current (aref nodes i)))
        (gl:rect (- (x current) 0.005)
                 (- (y current) 0.005)
                 (+ (x current) 0.005)
                 (+ (y current) 0.005)))))
 
  (glut:swap-buffers))


(defmethod glut:idle ((w kohonen-window))
  (dotimes (i 1000)
    (update (km w)))
  (glut:post-redisplay))


(defmethod glut:reshape ((w kohonen-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 1 0 1))

(defmethod glut:keyboard ((w kohonen-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defun kohonen ()
  (glut:display-window (make-instance 'kohonen-window 
                                      :km (make-instance '2d-kohonen-map 
                                                         :number-of-nodes 300))))
;; Kohonen Stuff

(defclass 2d-kohonen-map ()
  ((nodes :accessor nodes)
   (number-of-nodes :initarg :number-of-nodes :accessor number-of-nodes)
   (gamma :initarg :gamma :accessor gamma :initform 0.1)
   (alpha :initarg :alpha :accessor alpha :initform 0.5)))

(defclass kohonen-node ()
  ((x :accessor x)
   (y :accessor y)))

(defun random-0-1-float ()
  (/ (random 100000) 100000.0))


(defparameter *random-init* nil)

(defmethod initialize-instance ((node kohonen-node) &key)
  (setf (x node) (if *random-init* (random-0-1-float) 0.5))
  (setf (y node) (if *random-init* (random-0-1-float) 0.5)))

(defmethod initialize-instance :after ((km 2d-kohonen-map) &key)
   (setf (nodes km)
         (make-array (list (number-of-nodes km))
                     :element-type 'kohonen-node))
   (dotimes (i (number-of-nodes km))
     (setf (aref (nodes km) i)
           (make-instance 'kohonen-node))))

(defun square (x) (* x x))
  
(defmethod distance ((node kohonen-node) x y)
  (sqrt (+ (square (- x (x node)))
           (square (- y (y node))))))

(defmethod update ((km 2d-kohonen-map))
  ;; Create a query
  (let ((query-x (random-0-1-float))
        (query-y (random-0-1-float))
        (active 0)
        (min-distance 2))
    ;; Find the most arroused node
    (dotimes (i (number-of-nodes km))
      (let ((distance (distance (aref (nodes km) i) query-x query-y)))
        (when (< distance min-distance)
          (setf active i
                min-distance distance))))
    ;; Push all nodes in the neighborhood towards the query
    ;; -- move to the left
    (let ((current-gamma (gamma km)))
      (loop :for i :from active :upto (- (number-of-nodes km))
         :do (let ((x-dist (- query-x (x (aref (nodes km) i))))
                   (y-dist (- query-y (y (aref (nodes km) i)))))
               (incf (x (aref (nodes km) i)) (* x-dist current-gamma))
               (incf (y (aref (nodes km) i)) (* y-dist current-gamma)))
         (setf current-gamma (* (alpha km) current-gamma))))

    ;; -- move to the right
    (let ((current-gamma (gamma km)))
      (loop :for i :from active :downto 0
         :do (let ((x-dist (- query-x (x (aref (nodes km) i))))
                   (y-dist (- query-y (y (aref (nodes km) i)))))
               (incf (x (aref (nodes km) i)) (* x-dist current-gamma))
               (incf (y (aref (nodes km) i)) (* y-dist current-gamma)))
         (setf current-gamma (* (alpha km) current-gamma))))
  ))
