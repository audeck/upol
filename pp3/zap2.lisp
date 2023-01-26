#|

Dependencies:
 - micro-graphics library
 - 04_light.lisp
 - 05.lisp

|#



;;; ---------------------------------- Semaphore class ----------------------------------

(defclass semaphore (abstract-picture)
  ((semaphore-type  :initform :vehicle)
   (semaphore-phase :initform 0)))

(defmethod initialize-instance ((s semaphore) &key)
  (call-next-method)
  (update-items s)
  (update-lights s))

;; ----- Attributes -----

(defmethod semaphore-type ((s semaphore))
  (slot-value s 'semaphore-type))

(defmethod set-semaphore-type ((s semaphore) type)
  (check-semaphore-type s type)
  (setf (slot-value s 'semaphore-type) type)
  (set-semaphore-phase s 0)
  (update-items s)
  s)

(defmethod check-semaphore-type ((s semaphore) type)
  (unless (or (eql type :vehicle)
              (eql type :pedestrian))
    (error "Trying to set an invalid semaphore type"))
  s)


(defmethod semaphore-phase ((s semaphore))
  (slot-value s 'semaphore-phase))

; Sets current phase to (phase mod phase-count) and(!) implicitly updates the semaphore's lights
; NOTE: it's worth debating whether this method should be part of the user interface
;       (along with next-phase) or protected and leave just next-phase
(defmethod set-semaphore-phase ((s semaphore) phase)
  (unless (and (numberp phase)
               (>= phase 0))
    (error "semaphore-phase of semaphor should be a non-negative integer"))
  (setf (slot-value s 'semaphore-phase) (mod phase (phase-count s)))
  (update-lights s)
  s)

;; ----- Methods -----

; Advances the semaphore to it's next phase
(defmethod next-phase ((s semaphore))
  (set-semaphore-phase s (1+ (semaphore-phase s)))
  s)

(defmethod phase-count ((s semaphore))
  (length (lights-data s)))

; Maps the corresponding list of lights-data to the semaphore's lights to turn them on or off
(defmethod update-lights ((s semaphore))
  (let ((current-lights-data (nth (semaphore-phase s) (lights-data s)))
        (lights (lights s)))
    (mapcar (lambda (state light) (if state (turn-on light) (turn-off light))) 
            current-lights-data
            lights)
    s))

; Returns the semaphore's lights-data, which is a list of lists:
; e.g. ((light0-phase0 light1-phase0) (light0-phase1 light1-phase1)),
;      where lighti-phasej is T if light #i is on during phase #j, otherwise nil
(defmethod lights-data ((s semaphore))
  (case (semaphore-type s) (:vehicle '((t nil nil) (t t nil) (nil nil t) (nil t nil)))
                           (:pedestrian '((t nil) (nil t)))))

; Returns a list of semaphore lights
(defmethod lights ((s semaphore))
  (items (first (items s))))

; Creates and sets the semaphore's items (lights background)
(defmethod update-items ((s semaphore))
  (let ((background (create-background s))
        (lights (create-lights s)))
    (do-set-items s (list lights background))
    s))

;; ----- Type-dependant drawing data -----

; Creates and returns the semaphore's background
(defmethod create-background ((s semaphore))
  (case (semaphore-type s) (:vehicle (create-vehicle-background s))
                           (:pedestrian (create-pedestrian-background s))))

(defmethod create-vehicle-background ((s semaphore))
  (let ((bg (make-instance 'abstract-polygon))
        (a (make-point -30 -80))
        (b (make-point 30 -80))
        (c (make-point 30 80))
        (d (make-point -30 80)))
    (set-color bg :black)
    (set-thickness bg 0)
    (set-filledp bg t)
    (set-closedp bg t)
    (setf (slot-value bg 'items) (list a b c d))
    bg))

(defmethod create-pedestrian-background ((s semaphore))
  (let ((bg (make-instance 'abstract-polygon))
        (a (make-point -30 -55))
        (b (make-point 30 -55))
        (c (make-point 30 55))
        (d (make-point -30 55)))
    (set-color bg :black)
    (set-thickness bg 0)
    (set-filledp bg t)
    (set-closedp bg t)
    (setf (slot-value bg 'items) (list a b c d))
    bg))

; Creates and returns the semaphore's lights
(defmethod create-lights ((s semaphore))
  (case (semaphore-type s) (:vehicle (create-vehicle-lights s))
                           (:pedestrian (create-pedestrian-lights s))))

(defmethod create-vehicle-lights ((s semaphore))
  (let ((lights (make-instance 'abstract-picture))
        (light1 (make-instance 'light))
        (light2 (make-instance 'light))
        (light3 (make-instance 'light)))
    (set-radius light1 20)
    (set-radius light2 20)
    (set-radius light3 20)
    (move light1 0 -50)
    (move light2 0 0)
    (move light3 0 50)
    (set-on-color light1 :red)
    (set-on-color light2 :orange)
    (set-on-color light3 :green)
    ; off-color is default :gray
    (do-set-items lights (list light1 light2 light3))
    lights))

(defmethod create-pedestrian-lights ((s semaphore))
  (let ((lights (make-instance 'abstract-picture))
        (light1 (make-instance 'light))
        (light2 (make-instance 'light)))
    (set-radius light1 20)
    (set-radius light2 20)
    (move light1 0 -25)
    (move light2 0 25)
    (set-on-color light1 :red)
    (set-on-color light2 :green)
    ; off-color is default :gray
    (do-set-items lights (list light1 light2))
    lights))



;;; ---------------------------------- Crossroads class ----------------------------------

(defclass crossroads (abstract-picture)
  ((program          :initform '())
   (crossroads-phase :initform 0)))

;; ----- Attributes -----

(defmethod program ((cr crossroads))
  (copy-list (slot-value cr 'program)))

; Updates program and implicitly updates phase-count, semaphore lights and sets crossroads-phase to 0
; If the program assigns a phase to a semaphore that's higher than it's phase-count, the semaphore
; calculates (phase mod phase-count) and assigns that.
(defmethod set-program ((cr crossroads) program)
  (check-program cr program)
  (setf (slot-value cr 'program) (copy-list program))
  (set-crossroads-phase cr 0)
  (update-semaphore-lights cr)
  cr)

(defmethod check-program ((cr crossroads) program)
  (unless (and (typep program 'list)
               (> (length program) 0))
    (error "Program of a crossroads should be a list that's not empty")))

(defmethod crossroads-phase ((cr crossroads))
  (slot-value cr 'crossroads-phase))

(defmethod set-crossroads-phase ((cr crossroads) phase)
  (setf (slot-value cr 'crossroads-phase) phase)
  cr)

;; ----- Methods -----

; Advances to the next phase and updates all semaphore lights
(defmethod next-phase ((cr crossroads))
  (set-crossroads-phase cr (mod (1+ (crossroads-phase cr)) (phase-count cr)))
  (update-semaphore-lights cr)
  cr)

; Updates the phase all semaphores according to the current program and crossroads phase
; (which also updates all semaphores' lights)
(defmethod update-semaphore-lights ((cr crossroads))
  (let ((semaphore-phases (nth (crossroads-phase cr) (program cr))))
    (mapcar (lambda (semaphore phase) (set-semaphore-phase semaphore phase)) (semaphores cr) semaphore-phases)
    cr))

(defmethod phase-count ((cr crossroads))
  (length (program cr)))

; Finds and returns all semaphores in items of crossroads - now even those pesky ones hiding in 
; the items of (abstract) pictures!
; NOTE: User method calls with 2 arguments result in undefined behaviour
(defmethod semaphores ((cr crossroads) &optional (items (items cr)))
  (if (null items)
      '()
    (let ((item (car items)))
      (cond ((typep item 'semaphore) (cons item (semaphores cr (cdr items))))
            ((typep item 'abstract-picture) (append (semaphores cr (items item)) 
                                                    (semaphores cr (cdr items))))
            (t (semaphores cr (cdr items)))))))

;; ----- Method overrides for set-items -----

(defmethod check-items ((cr crossroads) items)
  (dolist (item items)
    (check-item cr item))
  cr)

(defmethod check-item ((cr crossroads) item)
  (unless (typep item 'shape)
    (error "Crossroads item should be of (or inherit) type shape"))
  cr)



;;; ---------------------------------- Functions ----------------------------------

(defun make-point (x y)
  (let ((pt (make-instance 'point)))
    (move pt x y)
    pt))

(defun make-rectangle (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((rect (make-instance 'abstract-polygon))
        (pt1 (make-point x1 y1))
        (pt2 (make-point x2 y2))
        (pt3 (make-point x3 y3))
        (pt4 (make-point x4 y4)))
    (setf (slot-value rect 'items) (list pt1 pt2 pt3 pt4))
    rect))



#|

;;; ---------------------------------- EXAMPLE ---------------------------------- 
;;; (evaluate first progn to show window, then evaluate the second one repeatedly to "animate"):

--- First progn ---
(progn
  (setf w (make-instance 'window))
  (set-background w :green)

  (setf road1 (set-thickness (set-closedp (set-filledp (set-color (make-rectangle 84 0 211 0 211 210 84 210) :gray) T) T) 0))
  (setf road2 (set-thickness (set-closedp (set-filledp (set-color (make-rectangle 0 60 298 60 298 150 0 150) :gray) T) T) 0))

  (setf zero (make-point 0 0))

  (setf ps0 (move (scale (set-semaphore-type (make-instance 'semaphore) :pedestrian) 0.25 zero) 130 40))
  (setf ps1 (move (scale (set-semaphore-type (make-instance 'semaphore) :pedestrian) 0.25 zero) 170 170))
  (setf ps2 (move (scale (set-semaphore-type (make-instance 'semaphore) :pedestrian) 0.25 zero) 230 105))
  (setf ps3 (move (scale (set-semaphore-type (make-instance 'semaphore) :pedestrian) 0.25 zero) 60 105))

  (setf vs0 (move (scale (make-instance 'semaphore) 0.25 zero) 149 60))
  (setf vs1 (move (scale (make-instance 'semaphore) 0.25 zero) 149 150))
  (setf vs2 (move (scale (make-instance 'semaphore) 0.25 zero) 211 105))
  (setf vs3 (move (scale (make-instance 'semaphore) 0.25 zero) 84 105))

  (setf vehicle-semaphores (set-items (make-instance 'picture) (list vs0 vs1 vs2 vs3)))

  (setf cr (make-instance 'crossroads))
  (set-items cr (list ps0 ps1 ps2 ps3 vehicle-semaphores road1 road2))
  (set-program cr '((0 0 0 0 0 0 0 0)
                    (0 0 0 0 0 0 1 1)
                    (1 1 0 0 0 0 2 2)
                    (1 1 0 0 0 0 3 3)
                    (0 0 0 0 0 0 0 0)
                    (0 0 0 0 1 1 0 0)
                    (0 0 1 1 2 2 0 0)
                    (0 0 1 1 3 3 0 0)))

  (set-shape w cr)
  (redraw w))

--- Second progn ---
(progn (next-phase cr) (redraw w))

|#