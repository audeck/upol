(defclass point ()
  ((x :initform 0)
   (y :initform 0)))

;;; EOL

; Triangles
(defclass triangle ()
  ((vertex-a :initform (make-instance 'point))
   (vertex-b :initform (make-instance 'point))
   (vertex-c :initform (make-instance 'point))))

(defmethod vertices ((tri triangle))
  (list (slot-value tri 'vertex-a)
        (slot-value tri 'vertex-b)
        (slot-value tri 'vertex-c)))

(defun square (x)
  (* x x))

(defun point-distance (pt-a pt-b)
  (sqrt (+ (square (- (slot-value pt-a 'x)
                      (slot-value pt-b 'x)))
           (square (- (slot-value pt-a 'y)
                      (slot-value pt-b 'y))))))

(defmethod perimeter ((triangle triangle))
  (let ((a (slot-value triangle 'vertex-a))
        (b (slot-value triangle 'vertex-b))
        (c (slot-value triangle 'vertex-c)))
    (+ (point-distance a b)
       (point-distance b c)
       (point-distance c a))))

(defmethod right-triangle-p ((triangle triangle))
  (let* ((a (slot-value triangle 'vertex-a))
         (b (slot-value triangle 'vertex-b))
         (c (slot-value triangle 'vertex-c))
         (sides (sort (mapcar #'square
                              (list (point-distance a b)
                                    (point-distance b c)
                                    (point-distance c a)))
                   #'>)))
    (eql (first sides) (+ (second sides) (third sides)))))

; Ellipses
(defclass ellipse ()
  ((focal-point-1  :initform (make-instance 'point))
   (focal-point-2  :initform (make-instance 'point))
   (major-semiaxis :initform 0)))

(defmethod eccentricity ((ellipse ellipse))
  (/ (/ (point-distance (slot-value ellipse 'focal-point-1)
                        (slot-value ellipse 'focal-point-2))
        2)
     (slot-value ellipse 'major-semiaxis)))

(defmethod major-semiaxis ((ellipse ellipse))
  (slot-value ellipse 'major-semiaxis))

(defmethod minor-semiaxis ((ellipse ellipse))
  (* (major-semiaxis ellipse)
     (sqrt (- 1 (square (eccentricity ellipse))))))

(defmethod to-ellipse ((circle circle))
  (let ((ellipse (make-instance 'ellipse)))
    (setf (slot-value ellipse 'focal-point-1)  (slot-value circle 'center)
          (slot-value ellipse 'focal-point-2)  (slot-value circle 'center)
          (slot-value ellipse 'major-semiaxis) (slot-value circle 'radius))))
