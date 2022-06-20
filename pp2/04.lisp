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

(defun move (object vec)
  (cond ((pointp object) (move-point object vec))
        ((circlep object) (move-point (center object vec)))
        (t (error "Object not compatible!"))))

(defun move-point (point vec)
  (set-x point (+ (first vec)  (x point)))
  (set-y point (+ (second vec) (y point))))

; Let to create a variable only accessible to val function
(let ((store nil))
  (defun val (&optional (value nil givenp))
    (if givenp
      (setf store value)
      store)))

(defmacro defvalf (name &optional (default nil))
  (let ((store-sym (gensym "STORE")))
    `(let ((,store-sym ,default))
       (defun ,name (&optional (value nil givenp))
         (if givenp
             (setf ,store-sym value)
           ,store-sym)))))

(defmacro my-time (expression)
  (let ((start-sym (gensym "START")))
    `(let ((,start-sym 0))
       (progn
         (setf ,start-sym (get-internal-run-time))
         ,expression
         (format t "Vyhodnocení výrazu ~S trvalo ~as." ',expression (/ (- (get-internal-run-time) start)
                                                                       internal-time-units-per-second))))))
