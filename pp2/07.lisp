;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Příklad k 7. přednášce z PP2 --- students.lisp
;;;

#|
Prohlížejte si zdrojový kód a vyhodnocujte zakomentované testy
|#

;; Na přednášce jsme strukturu studenta nepotřebovali.
;; Tady ji ale definujeme:

(defun make-student (pt-list)
  (list 'student pt-list))

(defun point-list (student)
  (second student))

;; A kvůli testování si vytvoříme globální proměnnou se studenty.
(defvar *students*)
(setf *students*
      (list (make-student '(3 0 NIL 3 2 3 4 3 NIL 4 3 2 4)) 
            (make-student '(0 3 1 0 3 2 2 3 2 0 0 3 NIL))
            (make-student '(NIL 2 NIL NIL 2 1 2 4 1 3 0 NIL NIL))
            (make-student '(1 0 0 2 NIL NIL 3 4 2 2 0 NIL NIL))
            (make-student '(NIL 4 1 0 1 1 2 2 3 2 3 0 0))
            (make-student '(0 4 1 NIL 1 3 0 0 2 0 NIL 1 3))
            (make-student '(2 1 0 4 1 NIL NIL NIL 2 4 1 NIL NIL))
            (make-student '(0 0 0 0 1 NIL NIL NIL NIL 1 NIL NIL NIL))
            (make-student '(3 0 4 2 NIL 2 NIL 2 4 3 3 2 1))
            (make-student '(0 0 4 3 NIL 3 2 1 3 0 NIL 0 3))
            (make-student '(3 2 4 0 2 2 4 2 0 NIL 2 4 NIL))
            (make-student '(NIL 0 3 2 NIL 3 0 3 NIL 1 1 3 2))
            (make-student '(3 2 1 4 0 2 4 4 3 1 3 3 0))))

(defun attendance (student)
  (reduce (lambda (x y)
            (+ x (if y 1 0)))
          (point-list student)
          :initial-value 0))

(defun list-average (list)
  (/ (apply #'+ list) 
     (length list)))

(defun completedp (student)
  (>= (attendance student) 9))


;; Úspěšní studentti:
(defun successful (students)
  (remove-if-not #'completedp students))

(defun successful-count (students)
  (length (successful students)))

#|
(successful *students*)
(successful-count *students*)
|#

;; Průměrný výsledek studenta
(defun student-point-avg (student)
  (list-average 
   (remove nil (point-list student))))

#|
(student-point-avg (car *students*))
|#

;; Body za danou písemku
(defun point-lists (students)
  (mapcar #'point-list students))

(defun test-points (test students)
  (mapcar (lambda (points) (nth test points))
          (mapcar #'point-list 
                  students)))

#|
(test-points 4 *students*)
|#

;; Průměrný počet bodů ze zadané písemky u úspěšných studentů
(defun test-points-avg-succ (test students)
  (labels ((nth-test (points)
             (nth test points)))
    (list-average
     (remove nil
             (mapcar #'nth-test
                     (mapcar #'point-list 
                             (remove-if-not #'completedp 
                                            students)))))))


#|
(test-points-avg-succ 4 *students*)
|#


;;; EOL

(defun total-points (student)
  (reduce (lambda (x y) (+ x y))
          (remove nil
                  (point-list student))))

(defun max-points (students)
  (reduce #'max
          (mapcar #'total-points students))) 

(defun best-student (students)
  (reduce (lambda (x y) (if (< (total-points x) (total-points y)) y x))
          students))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 7. přednáška z PP2 --- streams.lisp
;;;

;; Přísliby z minula:

(defun make-promise (fun)
  (list 'promise nil nil fun))

(defmacro delay (expr)
  `(make-promise (lambda () ,expr)))

(defun validp (promise)
  (second promise))

(defun force (promise)
  (unless (validp promise)
    (setf (third promise) (funcall (fourth promise))
          (second promise) t))
  (third promise))
 
(defun invalidate (promise)
  (setf (second promise) nil))

#|
Proud (stream) je
- buď prázdný proud, což je symbol nil
- nebo pár, jehož cdr je příslib proudu

Podobně jako u seznamů funkcí cons konstruujeme proud operátorem
cons-stream. Je to ovšem makro, protože jako druhý argument 
chceme dát výraz po jehož vyhodnocení vznikne pokračování proudu.
Do cdr páru se uloží pouze příslib, takže ke zjištění, jak proud 
pokračuje, je třeba použít force. Až v ten moment se provede výpočet.
|#

(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))

(defun stream-car (s)
  (car s))

;; stream-cdr provede pomocí force výpočet pokračování proudu.
(defun stream-cdr (s)
  (force (cdr s)))

#|

Pomocné funkce

|#

;; Převod seznamu na proud
(defun list-to-stream (list)
  (if (null list)
      '()
    (cons-stream (car list) (list-to-stream (cdr list)))))

#|
(setf s (list-to-stream '(1 2 3 4)))
(stream-car s)
(setf s (stream-cdr s))
(stream-car s)
(setf s (stream-cdr s))
(stream-car s)
;; ... atd, takhle to můžete zkoušet i u dalších příkladů
|#


;; Zakrytí symbolu stream, aby se dal použít (jinak by došlo k chybě,
;; protože je v Lispu už použit; nemusíte se tím ale trápit).
(shadow '(stream))

;; Analogie funkce list, ale napsaná jako makro, aby se zachovalo
;; líné vyhodnocování
(defun stream (&rest elements)
  (list-to-stream elements))

#|
(stream 1 2 3 4)
|#

#|
Následují funkce velmi podobné funkcím pro seznamy
|#

;; u nekonečného proudu se zacyklí
(defun stream-length (s)
  (if (null s)
      0
    (1+ (stream-length (stream-cdr s)))))

;; Jako mapcar, ale pracuje i s nekonečným proudem! 
;; (ale jen s jedním argumentem)
(defun stream-map (f s)
  (if (null s)
      '()
    (cons-stream
      (funcall f (stream-car s))
      (stream-map f (stream-cdr s)))))

#|
(stream-map #'1+ (stream 1 2 3 4 5))
(setf s
      (stream-map (lambda (x)
                    (format t "~%Výpočet pro hodnotu ~s " x)
                    (1+ x))
                  (stream 1 2 3 4 5)))

(stream-car s)
(setf s (stream-cdr s))
(stream-car s)
(setf s (stream-cdr s))
(stream-car s)
|#

;; Převod proudu na seznam. max-count lze nastavit, abychom se 
;; vyhnuli zacyklení u nekonečného proudu.
(defun stream-to-list (stream &optional max-count)
  (if (or (null stream)
          (eql max-count 0))
      '()
    (cons (stream-car stream) 
          (stream-to-list (stream-cdr stream)
                          (when max-count (- max-count 1))))))

;; Nekonečný proud jedniček
(defun ones ()
  (cons-stream 1 (ones)))

#|
(stream-to-list (ones) 20)
|#

;; Nekonečný proud přirozených čísel
(defun naturals (&optional (from 1))
  (cons-stream from (naturals (1+ from))))

#|
(stream-to-list (naturals) 20)
|#

;; nekonečný proud mocnin dvojky
(defun pow2 ()
  (labels ((iter (n)
             (cons-stream n (iter (* 2 n)))))
    (iter 1)))

#|
(stream-to-list (pow2) 20)
|#

;; Oblíbená Fibonacciho posloupnost poprvé
(defun fib1 ()
  (labels ((iter (n1 n2)
             (cons-stream n2 (iter n2 (+ n1 n2)))))
    (cons-stream 1 (iter 1 1))))

#|
(stream-to-list (fib1) 20)
|#

#|
Další příklady bez komentářů
|#

(defun stream-each-other (stream)
  (when stream
    (let ((car (stream-car stream))
          (cdr (stream-cdr stream)))
      (if cdr
          (cons-stream car (stream-each-other (stream-cdr cdr)))
        (cons-stream car nil)))))

(defun stream-squares (stream)
  (stream-map (lambda (x) (* x x))
              stream))

(defun stream-cubes (stream)
  (stream-map (lambda (x) (* x x x))
              stream))

(defun squares ()
  (stream-squares (naturals)))

#|
(stream-to-list (squares) 20)
|#

(defun stream-remove (elem stream)
  (cond ((null stream) '())
        ((eql elem (stream-car stream))
         (stream-remove elem (stream-cdr stream)))
        (t (cons-stream (stream-car stream) 
                        (stream-remove elem (stream-cdr stream))))))

(defun stream-remove-if (test stream)
  (cond ((null stream) '())
        ((funcall test (stream-car stream))
         (stream-remove-if test (stream-cdr stream)))
        (t (cons-stream (stream-car stream) 
                        (stream-remove-if test (stream-cdr stream))))))

(defun even-naturals ()
  (stream-remove-if #'oddp (naturals)))

#|

(stream-to-list (even-naturals) 50)
 
|#

(defun stream-map-2 (f s1 s2)
  (if (or (null s1) (null s2))
      '()
    (cons-stream
      (funcall f (stream-car s1) (stream-car s2))
      (stream-map-2 f (stream-cdr s1) (stream-cdr s2)))))

;; Fungovalo by toto s klasickými seznamy (kdybychom omezili délku)?
(defun fib2 ()
  (let (result)
    (setf result (cons-stream 
                  1
                  (cons-stream
                   1
                   (stream-map-2 #'+ result (stream-cdr result)))))))

;; Prvočísla:
(defun dividesp (m n)
  (= (rem n m) 0))

(defun sieve (stream)
  (let ((car (stream-car stream))
        (cdr (stream-cdr stream)))
    (cons-stream car
                 (sieve (stream-remove-if (lambda (n)
                                            (dividesp car n))
                                          cdr)))))

(defun eratosthenes ()
  (sieve (naturals 2)))

;; Duplicity

(defun stream-remove-duplicates (stream)
  (when stream
    (let ((car (stream-car stream)))
      (cons-stream car
                   (stream-remove-duplicates 
                    (stream-remove car stream))))))

;; Racionální čísla:

(defun positive-rationals ()
  (labels ((pr (numerator denominator)
             (cons-stream (/ numerator denominator)
                          (if (= numerator denominator)
                              (pr 1 (1+ denominator))
                            (pr (1+ numerator) denominator)))))
    (stream-remove-duplicates (pr 1 1))))

;;; EOL

;; Infinite 0 stream:
(defun zeros ()
  (cons-stream 0 (zeros)))

;; Infinite alternating 0 1 (or 1 0) streams:
(defun zeroones ()
  (cons-stream 0 (onezeros)))

(defun onezeros ()
  (cons-stream 1 (zeroones)))

(defun stream-ref (stream index)
  (when stream
    (if (= 0 index)
        (stream-car stream)
      (stream-ref (stream-cdr stream) (- index 1)))))

(defun stream-heads (stream &optional (sum 0))
  (when stream
    (let ((new-sum (+ (stream-car stream) sum)))
      (cons-stream new-sum (stream-heads (stream-cdr stream) new-sum)))))

(defun perfect (stream)
  (stream-heads (stream-cubes (stream-each-other stream))))

;; GENERATORS

(defun next (gen)
  (funcall gen))

(defun stream-to-gen (stream)
  (lambda ()
    (prog1 (stream-car stream)
      (setf stream (stream-cdr stream)))))

(defun zero-one-gen ()
  (let ((n 1))
    (lambda ()
      (if (= n 1)
          (decf n)
        (incf n)))))

(defun naturals-gen ()
  (let ((n 0))
    (lambda ()
      (incf n))))

(defun gen-heads (gen)
  (let ((n 0))
    (lambda ()
      (setf n (+ n (next gen))))))
