;;; Same as 01.lisp, but follows the encapsulation principle

; Triangle class
(defclass triangle ()
  ((vertex-a :initform (make-instance 'point))
   (vertex-b :initform (make-instance 'point))
   (vertex-c :initform (make-instance 'point))))

(defmethod vertex-a ((tri triangle))
  (slot-value tri 'vertex-a))

(defmethod set-vertex-a ((tri triangle) value)
  (unless (typep value 'point)
    (error "The vertex of a triangle should be a point!"))
  (setf (slot-value tri 'vertex-a) value))

(defmethod vertex-b ((tri triangle))
  (slot-value tri 'vertex-b))

(defmethod set-vertex-b ((tri triangle) value)
  (unless (typep value 'point)
    (error "The vertex of a triangle should be a point!"))
  (setf (slot-value tri 'vertex-b) value))

(defmethod vertex-c ((tri triangle))
  (slot-value tri 'vertex-c))

(defmethod set-vertex-c ((tri triangle) value)
  (unless (typep value 'point)
    (error "The vertex of a triangle should be a point!"))
  (setf (slot-value tri 'vertex-c) value))

(defmethod vertices ((tri triangle))
  (list (vertex-a tri)
        (vertex-b tri)
        (vertex-c tri)))

(defmethod perimeter ((tri triangle))
  (let ((a (vertex-a tri))
        (b (vertex-b tri))
        (c (vertex-c tri)))
    (+ (point-distance a b)
       (point-distance b c)
       (point-distance c a))))

(defmethod right-triangle-p ((tri triangle))
  (let* ((a (vertex-a tri))
         (b (vertex-b tri))
         (c (vertex-c tri))
         (sides (sort (mapcar #'square
                              (list (point-distance a b)
                                    (point-distance b c)
                                    (point-distance c a)))
                   #'>)))
    (eql (first sides) (+ (second sides) (third sides)))))

(defmethod to-polygon ((tri triangle))
  (let ((polygon (make-instance 'polygon)))
    (set-items polygon (vertices tri))))



; Picture class
(defclass picture ()
  (items :initform '()))

(defmethod items ((pic picture))
  (copy-list (slot-value pic 'items)))

(defmethod check-item ((pic picture) item)
  (unless (or (typep item 'pic)
              (typep item 'point)
              (typep item 'triangle)
              (typep item 'polygon)
              (typep item 'ellipse)
              (typep item 'circle))
    (error "Invalid picture item!"))
  pic)

(defmethod check-items ((pic picture) items)
  (dolist (item items)
    (check-item pic item))
  pic)

(defmethod set-items ((pic picture) value)
  (check-items pic value)
  (setf (slot-value pic 'items) (copy-list value)))



; Ellipse class
(defclass ellipse ()
  ((focal-point-1  :initform (make-instance 'point))
   (focal-point-2  :initform (make-instance 'point))
   (major-semiaxis :initform 0)))

(defmethod focal-point-1 ((ellipse ellipse))
  (slot-value ellipse 'focal-point-1))

(defmethod set-focal-point-1 ((ellipse ellipse) value)
  (unless (typep value 'point)
    (error "The focal point of an ellipse should be a point!"))
  (setf (slot-value ellipse 'focal-point-1) value))

(defmethod focal-point-2 ((ellipse ellipse))
  (slot-value ellipse 'focal-point-2))

(defmethod set-focal-point-2 ((ellipse ellipse) value)
  (unless (typep value 'point)
    (error "The focal point of an ellipse should be a point!"))
  (setf (slot-value ellipse 'focal-point-2) value))

(defmethod eccentricity ((ellipse ellipse))
  (/ (/ (point-distance (focal-point-1 ellipse)
                        (focal-point-2 ellipse))
        2)
     (major-semiaxis ellipse)))

(defmethod major-semiaxis ((ellipse ellipse))
  (slot-value ellipse 'major-semiaxis))

(defmethod set-major-semiaxis ((ellipse ellipse) value)
  (unless (> value 0)
    (error "The semiaxis of an ellipse should be a non-negative integer!"))
  (setf (slot-value ellipse 'major-semiaxis) value))

(defmethod minor-semiaxis ((ellipse ellipse))
  (* (major-semiaxis ellipse)
     (sqrt (- 1 (square (eccentricity ellipse))))))

(defmethod to-ellipse ((circle circle))
  (let ((ellipse (make-instance 'ellipse)))
    (set-focal-point-1 ellipse (center circle))
    (set-focal-point-2 ellipse (center circle))
    (set-major-semiaxis ellipse (radius circle))))
