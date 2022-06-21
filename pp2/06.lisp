;;; Version #2 (lazy eval)

(defun make-elist (&rest elements)
  (list 'elist
        (copy-list elements)
        nil
        nil
        nil
        nil))

(defun clear-cache (elist)
  (setf (third elist) nil
        (fourth elist) nil
        (fifth elist) nil
        (sixth elist) nil))

(defun elist-list (elist)
  (second elist))

(defun (setf elist-list) (new-list elist)
  (clear-cache elist)
  (setf (second elist) new-list))

(defun list-average (list)
  (/ (apply #'+ list) 
     (length list)))

(defun elist-average (elist)
  (when (null (third elist))
    (setf (third elist)
          (list-average (elist-list elist))))
  (third elist))

(defun list-median (list)
  (let ((sorted (sort (copy-list list) #'<))
        (length (length list))
        (tail nil))
    (cond ((oddp length) (nth (/ (1- length) 2) sorted))
          (t (setf tail (nthcdr (1- (/ length 2)) sorted))
             (/ (+ (pop tail) (pop tail)) 2)))))

(defun elist-median (elist)
  (when (null (fourth elist))
    (setf (fourth elist)
          (list-median (elist-list elist))))
  (fourth elist))

(defun add-after (val after list)
  (cond ((null after) (cons val list))
        (t (setf (cdr after) 
                 (cons val (cdr after))) 
           list)))

(defun elist-add-after (elist value cons)
  (clear-cache elist)
  (setf (elist-list elist)
        (add-after value cons (elist-list elist)))
  elist)

(defun delete-after (after list)
  (cond ((eql after nil) (cdr list))
        (t (setf (cdr after) (cddr after))
           list)))

(defun elist-delete-after (elist cons)
  (clear-cache elist)
  (setf (elist-list elist)
        (delete-after cons (elist-list elist)))
  elist)

(defun elist-max (elist)
  (unless (fifth elist)
    (setf (fifth elist)
          (apply #'max (elist-list elist)))
    (print "Calculated max!"))
  (fifth elist))

(defun elist-min (elist)
  (unless (sixth elist)
    (setf (sixth elist)
          (apply #'min (elist-list elist)))
    (print "Calculated min!"))
  (sixth elist))


#|
;;; testy (vyhodnocujte v Listeneru)

(setf el (make-elist))
(elist-list el)
(setf (elist-list el) (list 1 2 4 9))
(elist-list el)
(elist-average el)
el
(elist-median el)
el

(setf cons (cddr (elist-list el)))
(elist-add-after el 5 cons)
(elist-delete-after el (cdr cons))
(elist-add-after el 3 (cdr (elist-list el)))
|#

;;; Version #3 (promises)

;; Přísliby s memoizací:

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

;; na testy příslibů se podívejte do přednášky

|#

;; elisty s přísliby

(defun make-elist (&rest elements)
  (let ((result nil))
    (setf result (list 'elist
                       (copy-list elements)
                       nil
                       nil
                       nil
                       nil)
          (third result) (delay (list-average 
                                 (elist-list result)))
          (fourth result) (delay (list-median 
                                  (elist-list result)))
          (fifth result) (delay (apply #'max (elist-list result)))
          (sixth result) (delay (apply #'min (elist-list result))))
    result))

(defun elist-list (elist)
  (second elist))

(defun elist-avg-promise (elist)
  (third elist))

(defun elist-median-promise (elist)
  (fourth elist))

(defun elist-max-promise (elist)
  (fifth elist))

(defun elist-min-promise (elist)
  (sixth elist))

(defun elist-average (elist)
  (force (elist-avg-promise elist)))

(defun elist-median (elist)
  (force (elist-median-promise elist)))

(defun elist-max (elist)
  (force (elist-max-promise elist)))

(defun elist-min (elist)
  (force (elist-min-promise elist)))

(defun elist-invalidate (elist)
  (invalidate (elist-avg-promise elist))
  (invalidate (elist-median-promise elist))
  (invalidate (elist-max-promise elist))
  (invalidate (elist-min-promise elist)))

(defun (setf elist-list) (val elist)
  (elist-invalidate elist)
  (setf (second elist) val))

(defun elist-add-after (elist value cons)
  (elist-invalidate elist)
  (setf (elist-list elist) 
        (add-after value cons (elist-list elist)))
  elist)

(defun elist-delete-after (elist cons)
  (elist-invalidate elist)
  (setf (elist-list elist)
        (delete-after cons (elist-list elist)))
  elist)


#|
;;; testy (vyhodnocujte v Listeneru)

(setf el (make-elist))
(elist-list el)
(setf (elist-list el) (list 1 2 4 9))
(elist-list el)
(elist-average el)
el
(elist-median el)
el

(setf cons (cddr (elist-list el)))
(elist-add-after el 5 cons)
(elist-delete-after el (cdr cons))
(elist-add-after el 3 (cdr (elist-list el)))
|#
