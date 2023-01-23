
;;; Triangle class
(defclass triangle ()
  ((vertex-a :initform (make-instance 'point))
   (vertex-b :initform (make-instance 'point))
   (vertex-c :initform (make-instance 'point))
   (color :initform :black)
   (thickness :initform 1)
   (filledp :initform nil)
   (closedp :initform nil)))

; Attributes

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



; Draw attributes

(defmethod color ((tri triangle))
  (slot-value tri 'color))

(defmethod set-color ((tri triangle) value)
  (setf (slot-value tri 'color) value)
  tri)

(defmethod thickness ((tri triangle))
  (slot-value tri 'thickness))

(defmethod set-thickness ((tri triangle) value)
  (setf (slot-value tri 'thickness) value)
  tri)

(defmethod filledp ((tri triangle))
  (slot-value tri 'filledp))

(defmethod set-filledp ((tri triangle) value)
  (setf (slot-value tri 'filledp) value)
  tri)

(defmethod closedp ((tri triangle))
  (slot-value tri 'closedp))

(defmethod set-closedp ((tri triangle) value)
  (setf (slot-value tri 'closedp) value)
  tri)



; Geometric transformations

(defmethod move ((tri triangle) dx dy)
  (dolist (vertex (vertices tri))
    (move vertex dx dy))
  tri)

(defmethod rotate ((tri triangle) angle center)
  (dolist (vertex (vertices tri))
    (rotate vertex angle center))
  tri)

(defmethod scale ((tri triangle) coeff center)
  (dolist (vertex (vertices tri))
    (scale vertex coeff center))
  tri)





;;;;; Perimeter methods ;;;;;

(defmethod perimeter ((pt point)) 0)

(defmethod perimeter ((c circle))
  (* 2 pi (radius c)))

(defmethod perimeter ((pic picture))
  (reduce #'+ (mapcar (lambda (x) (perimeter x))
                      (items pic))))

(defmethod distance-to ((pt1 point) pt2)
  (point-distance pt1 pt2))

(defun next (list item)
  (nth (mod (find item list) (length list)) list))

(defmethod perimeter ((poly polygon))
  (let ((result 0)
        (items (items poly)))
    (dolist (item items)
      (incf result (distance-to item (next items item))))
    result))





;;;;; Ellipse class

; Ellipse class
(defclass ellipse ()
  ((focal-point-1  :initform (make-instance 'point))
   (focal-point-2  :initform (make-instance 'point))
   (major-semiaxis :initform 0)
   (color :initform :black)
   (filledp :initform nil)
   (thickness :initform 1)))

(defmethod focal-point-1 ((ellipse ellipse))
  (slot-value ellipse 'focal-point-1))

(defmethod set-focal-point-1 ((ellipse ellipse) value)
  (unless (typep value 'point)
    (error "The focal point of an ellipse should be a point!"))
  (setf (slot-value ellipse 'focal-point-1) value)
  ellipse)

(defmethod focal-point-2 ((ellipse ellipse))
  (slot-value ellipse 'focal-point-2))

(defmethod set-focal-point-2 ((ellipse ellipse) value)
  (unless (typep value 'point)
    (error "The focal point of an ellipse should be a point!"))
  (setf (slot-value ellipse 'focal-point-2) value)
  ellipse)

(defmethod center ((e ellipse))
  (move (make-instance 'point)
        (+ (x (focal-point-2 e))
           (/ (- (x (focal-point-1 e)) (x (focal-point-2 e))) 2))
        (+ (y (focal-point-2 e))
           (/ (- (y (focal-point-1 e)) (y (focal-point-2 e))) 2))))

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
  (setf (slot-value ellipse 'major-semiaxis) value)
  ellipse)

(defmethod minor-semiaxis ((ellipse ellipse))
  (* (major-semiaxis ellipse)
     (sqrt (- 1 (square (eccentricity ellipse))))))



;;; Draw parameters

(defmethod color ((e ellipse))
  (slot-value e 'color))

(defmethod set-color ((e ellipse) value)
  (setf (slot-value e 'color) value)
  e)

(defmethod filledp ((e ellipse))
  (slot-value e 'filledp))

(defmethod set-filledp ((e ellipse) value)
  (setf (slot-value e 'filledp) value)
  e)

(defmethod thickness ((e ellipse))
  (slot-value e 'thickness))

(defmethod set-thickness ((e ellipse) value)
  (setf (slot-value e 'thickness) value)
  e)



;;; Draw methods

(defmethod set-mg-params ((e ellipse) mgw)
  (mg:set-param mgw :foreground (color e))
  (mg:set-param mgw :thickness (thickness e))
  (mg:set-param mgw :filledp (filledp e))
  e)

(defmethod do-draw ((e ellipse) mgw)
  (let ((center (center e)))
    (mg:draw-ellipse mgw 
                     (x center) 
                     (y center) 
                     (major-semiaxis e)
                     (minor-semiaxis e)
                     0)
    e))

(defmethod draw ((e ellipse) mgw)
  (set-mg-params e mgw)
  (do-draw e mgw))

(defun point-distance (pt-a pt-b)
  (sqrt (+ (square (- (slot-value pt-a 'x)
                      (slot-value pt-b 'x)))
           (square (- (slot-value pt-a 'y)
                      (slot-value pt-b 'y))))))

(defun square (x)
  (* x x))
