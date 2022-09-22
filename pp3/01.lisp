(defclass triangle ()
  ((vertex-a :initform (make-instance 'point))
   (vertex-b :initform (make-instance 'point))
   (vertex-c :initform (make-instance 'point))))

(defmethod vertices ((tri triangle))
  (list (slot-value tri 'vertex-a)
        (slot-value tri 'vertex-b)
        (slot-value tri 'vertex-c)))

(defun point-distance (pt-a pt-b)
  (sqrt (+ (square (- (slot-value pt-a x)
                      (slot-value pt-b x)))
           (square (- (slot-value pt-a y)
                      (slot-value pt-b y))))))

(defmethod perimeter ((triangle triangle))
  (
