;
; DEPENDENCIES:
;  - OMG library (8th lecture version): 08_load.lisp
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; ---------- INSPECTOR-ATTRIBUTES DOCS ---------- ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; On extendibility:
;
; In order for the INSPECTOR-WINDOW to be able to inspect a given
; class, the class first needs to implement or, at least inherit,
; the INSPECTOR-ATTRIBUTES method, which should return a list of 
; symbols corresponding to it's attributes which are able to be 
; inspected.
;
; The class also needs to follow the encapsulation principle
; (or at least the inspectable attributes need to), i.e.:
;    a) Implement "setters" of same names as it's attributes.
;          e.g.: name -> name
;    b) Implement "getters" of names comprised of it's attributes'
;       names prefixed with set-.
;          e.g.: name -> set-name
;
; The class should also "interface" well with the OMG library
; (i.e. handle micro-graphics callbacks, send and handle
; corresponding events, etc.).
;
; You can find example implementations below.

;;; ----- Inspector attribute definitions for the OMG library -----

(defmethod inspector-attributes ((obj omg-object))
  '())

(defmethod inspector-attributes ((shape shape))
  (append (call-next-method)
          '(color thickness filledp)))

(defmethod inspector-attributes ((point point))
  (append (call-next-method)
          '(x y r phi)))

; center-x and center-y to spice things up
(defmethod inspector-attributes ((circle circle))
  (append (call-next-method)
          '(center-x center-y radius)))

(defmethod center-x ((circle circle))
  (x (center circle)))

(defmethod set-center-x ((circle circle) x)
  (set-x (center circle) x)  ; Handles change
  circle)

(defmethod center-y ((circle circle))
  (y (center circle)))

(defmethod set-center-y ((circle circle) y)
  (set-y (center circle) y)
  circle)

(defmethod inspector-attributes ((ap abstract-polygon))
  (append (call-next-method)
          '(closedp)))

(defmethod inspector-attributes ((aw abstract-window))
  (append (call-next-method)
          '(background)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; ---------- INSPECTED-WINDOW CLASS ---------- ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass inspected-window (window) ())

(defmethod mouse-down-no-shape ((w inspected-window) button position)
  (send-event w 'ev-mouse-down w button position))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; ---------- INSPECTOR-WINDOW CLASS ---------- ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass inspector-window (window)
  ((inspected-window :initform nil)
   (inspected-object :initform nil)))

(defmethod initialize-instance ((iw inspector-window) &key)
  (call-next-method)
  (set-shape iw (add-event (make-instance 'abstract-picture)
                           'ev-double-click  ; Workaround
                           'ev-double-click))
  (do-set-items (shape iw) 
                (list (make-text "Waiting for a window.")))
  (move-info-text iw))

;;; ----- ATTRIBUTES -----

(defmethod inspected-window ((iw inspector-window))
  (slot-value iw 'inspected-window))

(defmethod do-set-inspected-window ((iw inspector-window) window)
  ; Check type
  (unless (typep window 'inspected-window)
    (error "The window being inspected has to be of type inspected-window"))

  ; Update delegates
  (let ((old-window (inspected-window iw)))
    (when old-window (set-delegate old-window nil)))
  (set-delegate window iw)

  ; Update slots (inspected-window and initial inspected-object)
  (setf (slot-value iw 'inspected-window) window)
  (set-inspected-object iw window)
  iw)

(defmethod set-inspected-window ((iw inspector-window) window)
  (send-with-change iw 'do-set-inspected-window window))

(defmethod inspected-object ((iw inspector-window))
  (slot-value iw 'inspected-object))

; Sets the inspected object and updates info text accordingly
(defmethod do-set-inspected-object ((iw inspector-window) object)
  (unless (typep object 'omg-object)
    (error "The inspected-object of an inspector should be of, 
            or at least inherit, type omg-object"))
  (setf (slot-value iw 'inspected-object) object)
  (update-info-text iw)
  iw)

(defmethod set-inspected-object ((iw inspector-window) object)
  (send-with-change iw 'do-set-inspected-object object))

;;; ----- METHODS (and two functions) -----

(defun make-text (text)
  (set-text (make-instance 'text-shape) text))

; Recreates the info-text (= inspector window's shape's items)
(defmethod update-info-text ((iw inspector-window))
  (when (inspected-object iw)
    (let* ((object (inspected-object iw))
           (attributes (inspector-attributes object)))
      (do-set-items 
       (shape iw) (cons (make-text (format nil "Inspecting: ~a" (type-of object)))
                        (attribute-text-list iw object attributes))))
    (move-info-text iw)))

; Returns a list of text-shapes; one for each valid inspector attribute
(defmethod attribute-text-list ((iw inspector-window) object attributes)
  (if (null attributes)
      '()
    (let ((attribute (first attributes)))
      (cons (add-event (make-text (format nil 
                                          " - ~a: ~a" 
                                          attribute 
                                          (apply attribute (list object))))
                       'ev-double-click  ; Workaround
                       'ev-double-click)
            (attribute-text-list iw object (cdr attributes))))))

; Spaces out the info text-shapes evenly
(defmethod move-info-text ((iw inspector-window))
  (let ((lines (items (shape iw))))
    (dotimes (i (length lines))
      (move (nth i lines) 6 (* (1+ i) 20)))))

; Returns the setter name for prop symbol
(defun setter-name (prop)
  (values (find-symbol (format nil "SET-~a" prop))))

; Uses the fact that the inspector-attributes and (items (shape iw)),
; that is text-shapes, lists have the same ordering and finds the
; attribute symbol corresponding to text-shape
(defmethod find-attribute-by-text ((iw inspector-window) text-shape)
  (let ((text-shapes (items (shape iw)))
        (attributes (inspector-attributes (inspected-object iw))))
    (nth (1- (position text-shape text-shapes)) attributes)))

; Prompts the user and changes the attribute corresponging to 
; text-shape to it's result
(defmethod change-attribute-by-text ((iw inspector-window) text-shape)
  (let ((attribute (find-attribute-by-text iw text-shape))
        (prompt-result (multiple-value-list
                        (capi:prompt-for-value "Enter a new value"))))
    (when (second prompt-result)
      (apply (setter-name attribute) (inspected-object iw) (list (first prompt-result))))))

;;; ----- DOUBLE CLICK CALLBACK -----

(defmethod install-callbacks ((iw inspector-window))
  (call-next-method)
  (install-double-click-callback iw))

(defmethod install-double-click-callback ((iw inspector-window))
  (mg:set-callback 
   (slot-value iw 'mg-window)
   :double-click (lambda (mgw button x y)
                   (declare (ignore mgw))
                   (window-double-click iw button
                                        (move (make-instance 'point) x y))))
  iw)

(defmethod window-double-click ((iw inspector-window) button position)
  (let ((shape (find-clicked-shape iw position)))
    (if (and shape (typep shape 'text-shape))
        (double-click shape button position)
      iw)))

(defmethod double-click ((shape text-shape) button position)
  (send-event shape 'ev-double-click shape button position))

;;; ----- EVENTS -----

(defmethod ev-mouse-down ((iw inspector-window) sender clicked object position)
  (when (typep sender 'inspected-window)
    (set-inspected-object iw clicked))
  (call-next-method))

(defmethod ev-change ((iw inspector-window) sender)
  (when (typep sender 'inspected-window)
    (update-info-text iw))
  (call-next-method))

; NOTE: Ideally I'd like to implement ev-double-click with it's translation
;       right in omg-object, but we can't rewrite original source code, hence
;       this workaround.
(defmethod ev-double-click ((ap abstract-picture) sender clicked object position)
  (send-event ap 'ev-double-click clicked object position))

(defmethod ev-double-click ((iw inspector-window) sender clicked object position)
  (unless (typep sender 'inspected-window)
    (change-attribute-by-text iw clicked)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; ---------- TESTING PLAYGROUND ---------- ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(progn 
  (setf iw (make-instance 'inspector-window))
  (setf w (make-instance 'inspected-window))
  (set-inspected-window iw w)
  (set-shape w (make-instance 'abstract-picture))  ; Not inspectable!
  (do-set-items (shape w) (list (move (set-radius (make-instance 'circle)
                                                  20)
                                      40 40))))

; Changes!
(set-filledp (first (items (shape w))) t)

(set-background w :orange)

|#