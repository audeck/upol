(defmacro my-incf (a)
  `(setf ,a (+ ,a 1)))

(defmacro swap (a b)
  `(psetf a ,b b ,a))

(defmacro swap-2 (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (setf ,a ,b ,b ,tmp))))
 
#|
(swap-2 a b)
|#

(defun make-point (x y)
  (lambda (what &optional val)
    (case what
      ('x x)
      ('y y)
      ('set-x (setf x val))
      ('set-y (setf y val))
      ('pointp T))))

(defun x (point)
  (funcall point 'x))

(defun y (point)
  (funcall point 'y))

(defun set-x (point val)
  (funcall point 'set-x val))

(defun set-y (point val)
  (funcall point 'set-y val))

(defun pointp (obj)
  (and (functionp obj)
       (funcall obj 'pointp)))

(defun point-distance (A B)
  (sqrt (+ (expt (- (x A) (x B)) 2)
           (expt (- (y A) (y B)) 2))))

(defun make-circle (radius center)
  (lambda (what &optional val)
    (case what
      ('radius radius)
      ('center center)
      ('set-radius (setf radius val))
      ('circlep T))))  ; Could return it's type ("circle" for example)

(defun radius (circle)
  (funcall circle 'radius))

(defun center (circle)
  (funcall circle 'center))

(defun set-radius (circle val)
  (funcall circle 'set-radius val))

(defun circlep (obj)
  (and (functionp obj)
       (funcall obj 'circlep)))

(defun circle-area (circle)
  (* pi (expt (radius circle) 2)))

(defun move (obj vec-x vec-y)
  (cond ((pointp obj) (set-x obj (+ (x obj) vec-x))
                      (set-y obj (+ (y obj) vec-y)))
        ((circlep obj) (let ((c (center obj)))
                         (set-x c (+ (x c) vec-x))
                         (set-y c (+ (y c) vec-y))))))
