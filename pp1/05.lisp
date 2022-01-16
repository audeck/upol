(defun my-make-list (length elem)
  (if (= length 0)
      ()
      (cons elem (my-make-list (- length 1) elem))))

;;; End-Of-Lecture

(defun interval (lower upper)
  (if (< upper lower)
      nil
      (cons lower upper)))

(defun lower-bound (interval)
  (car interval))

(defun upper-bound (interval)
  (cdr interval))

(defun number-in-interval-p (n interval)
  (and (< (lower-bound interval) n) (< n (upper-bound interval))))

(defun interval-intersection (int1 int2)
  (interval (max (lower-bound int1) (lower-bound int2)) 
            (min (upper-bound int1) (upper-bound int2))))

; (let ((d (cons 3 4)))
;   (cons 1 (cons d (cons d 2))))

(defun proper-list-p (list)
  (cond ((null list) t)
        ((consp (car list)) nil)
        (t (proper-list-p (cdr list)))))

; Iterative version (of my-make-list from lecture)
(defun my-make-list (length elem)
  (my-make-list-iter length elem ()))

(defun my-make-list-iter (length elem ir)
  (if (<= length 0)
       ir
      (my-make-list-iter (- length 1) elem (cons elem ir))))

; Recursive version
(defun make-ar-seq-list (first dif len)
  (if (<= len 0)
      ()
      (cons first (make-ar-seq-list (+ first dif) dif (- len 1)))))

; Iterative version
(defun make-ar-seq-list (first dif len)
  (make-ar-seq-list-iter first dif len ()))

(defun make-ar-seq-list-iter (first dif len ir)
  (if (<= len 0)
       ir
      (make-ar-seq-list-iter first dif (- len 1) (cons (+ first (* dif (- len 1))) ir))))

; Recursive version
(defun make-geom-seq-list (first quot len)
  (if (<= len 0)
      ()
      (cons first (make-geom-seq-list (* first quot) quot (- len 1)))))

; Iterative version
(defun make-geom-seq-list (first quot len)
  (make-geom-seq-list-iter first quot len ()))

(defun make-geom-seq-list-iter (first quot len ir)
  (if (<= len 0)
       ir
      (make-geom-seq-list-iter first quot (- len 1) (cons (* first (expt quot (- len 1))) ir))))
