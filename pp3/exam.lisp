


#|

 --- PP3: zadání 26/01/2023 (zkouška 27/01/2023) ---
                   Suchomel Luboš

"Rozšíření" knihovny OMG (Object Micro-Graphics); potřebuje:
 - micro-graphics (verze z 8. přednášky) @ 08_load.lisp

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin masked-shape-mixin
;;;

#|
Mixin přidává vlastnost pro masku (abstract-polygon) a chování pro 
nastavení masky při vykreslování, geometrické transformace objektu
s maskou, a hit-scanning pro mouse-down eventy.

NOTE: Prázdné polygony mají stejnou funkčnost jako maska pokrývající
      celé okno, monogony a digony mají stejnou funkčnost jako maska
      s obsahem 0 (chování odvozeno z micro-graphics).
|#

(defclass masked-shape-mixin ()
  ((mask :initform (make-instance 'abstract-polygon))))

;;;
;;; Vlastnosti
;;;

(defmethod mask ((ms masked-shape-mixin))
  (slot-value ms 'mask))

(defmethod do-set-mask ((ms masked-shape-mixin) value)
  (if (eql value t)
      (setf value (make-instance 'abstract-polygon))  ; Default mask
    (unless (typep value 'abstract-polygon)
       (error "The mask of a shape must be a polygon")))

  (set-delegate value ms)
  (setf (slot-value ms 'mask) value))

(defmethod set-mask ((ms masked-shape-mixin) value)
  (send-with-change ms #'do-set-mask value)
  ms)

;;;
;;; Vykreslování
;;;

(defmethod set-mg-params ((ms masked-shape-mixin) mgw)
  (mg:set-param mgw :mask (polygon-coordinates (mask ms)))
  (call-next-method))

(defmethod draw :after ((ms masked-shape-mixin) mgw)
  (mg:set-param mgw :mask '()))

;;;
;;; Geometrické transformace
;;;

(defmethod move ((ms masked-shape-mixin) dx dy)
  (move (mask ms) dx dy)
  (call-next-method))

(defmethod rotate ((ms masked-shape-mixin) angle center)
  (rotate (mask ms) angle center)
  (call-next-method))

(defmethod scale ((ms masked-shape-mixin) coeff center)
  (scale (mask ms) coeff center)
  (call-next-method))

;;;
;;; Hit-scanning
;;;

(defmethod mask-hit-p ((ms masked-shape-mixin) point)
  (if (null (items (mask ms)))
      t
    (contains-point-p (set-filledp (mask ms) t) point)))

(defmethod contains-point-p ((ms masked-shape-mixin) point)
  (and (mask-hit-p ms point)
       (call-next-method)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třídy pro tvary s maskou
;;;

(defclass masked-point (masked-shape-mixin point)
  ())

(defclass masked-circle (masked-shape-mixin circle)
  ())

(defclass masked-polygon (masked-shape-mixin polygon)
  ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testy
;;;

#|

;;;
;;; Pomocné funkce a metody k testům
;;;

(defun make-point (x y)
  (set-x (set-y (make-instance 'point)
                y)
         x))

(defmethod mouse-down ((ms masked-shape-mixin) button position)
  (print "Clicked!")
  (call-next-method))

;;;
;;; Testy
;;;

(progn
  ; Shapes
  (setf w (make-instance 'window))
  (setf c (set-filledp (set-radius (move (make-instance 'masked-circle) 50 50)
                                   40)
                       t))
  (setf pt (set-filledp (set-thickness (move (make-instance 'masked-point) 150 50)
                                       10)
                        t))
  (setf poly (set-filledp (set-items (make-instance 'masked-polygon)
                                     (list (make-point 140 70)
                                           (make-point 210 84)
                                           (make-point 199 121)
                                           (make-point 184 99)
                                           (make-point 166 81)))
                          t))
  (setf p (set-items (make-instance 'picture)
                     (list c pt poly)))
  (set-shape w p)

  ; Masks
  (setf c-mask (set-items (make-instance 'polygon)
                          (list (make-point 10 10)
                                (make-point 90 10)
                                (make-point 90 50)
                                (make-point 10 50))))
  (setf pt-mask (set-items (make-instance 'polygon)
                           (list (make-point 150 40)
                                 (make-point 180 40)
                                 (make-point 180 70)
                                 (make-point 150 70))))
  (setf poly-mask (set-items (make-instance 'polygon)
                             (list (make-point 180 80)
                                   (make-point 200 120)
                                   (make-point 160 120))))
)  ;;; End of progn ;;;

(set-mask c c-mask)
(set-mask pt pt-mask)
(set-mask poly poly-mask)
(set-mask poly t)

(mg:get-param (slot-value w 'mg-window) :mask)

;;; Geometrické transformace

(move c 0 10)
(rotate c 1 (center c))
(scale c 0.5 (center c))

(move pt 0 -10)
(rotate pt -1 pt)
(scale pt 2 pt)  ; Doesn't affect thickness

(move poly 0 10)
(rotate poly 1 (first (items poly)))
(scale poly 2 (first (items poly)))

;;; (!)Hit-scanning test poklikáním

|#

