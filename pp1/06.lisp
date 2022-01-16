(defun proper-list-p (x)
  (or (null x)
      (and (consp x)
           (proper-list-p (cdr x)))))

(defun my-nth (n list) 
  (if (= n 0)
      (car list)
    (my-nth (- n 1) (cdr list))))

(defun my-nthcdr (n list) 
  (if (= n 0)
      list
    (my-nthcdr (- n 1) (cdr list))))

(defun my-ldiff (list tail)
  (if (or (null list) (eql list tail))
      ()
    (cons (car list) 
          (my-ldiff (cdr list) tail))))

(defun append-2 (list1 list2)
  (if (null list1)
      list2
    (cons (car list1) 
          (append-2 (cdr list1) list2))))

(defun my-revappend (list1 list2)
  (if (null list1)
      list2
    (my-revappend (cdr list1)
                  (cons (car list1) list2))))

(defun my-reverse (list)
  (my-revappend list ()))

(defun scale-list (list factor) 
  (if (null list)
      ()
    (cons (* factor (car list))
          (scale-list (cdr list) factor))))

(defun sum-lists-2 (list1 list2) 
  (if (null list1)
      ()
      (cons (+ (car list1) (car list2))
            (sum-lists-2 (cdr list1) (cdr list2)))))

(defun elementp (x list)
  (and (not (null list))
       (or (eql x (car list))
           (elementp x (cdr list)))))

(defun my-subsetp (list1 list2)
  (or (null list1)
      (and (elementp (car list1) list2)
           (my-subsetp (cdr list1) list2))))

(defun my-intersection (list1 list2)
  (cond ((null list1) ())
        ((elementp (car list1) list2) 
         (cons (car list1)
               (intersection (cdr list1) list2)))
        (t (intersection (cdr list1) list2))))

(defun subsets (list)
  (if (null list)
      (list ())
    (let ((cdr-subsets (subsets (cdr list))))
      (append-2 cdr-subsets
                (add-to-all (car list) cdr-subsets)))))

(defun add-to-all (elem list)
  (if (null list)
      ()
    (cons (cons elem (car list)) (add-to-all elem (cdr list)))))

(defun merge-sort (list) 
  (let* ((len (length list))
         (len/2 (floor (/ len 2))) 
         (list2 (nthcdr len/2 list))
         (list1 (ldiff list list2)))
    (if (<= len 1)
        list
      (merge-lists (merge-sort list1) (merge-sort list2)))))

(defun merge-lists (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((<= (car l1) (car l2)) 
         (cons (car l1) (merge-lists (cdr l1) l2)))
        (t (cons (car l2) (merge-lists l1 (cdr l2))))))

;;; End-Of-Lecture

(defun last-pair (list)
  (if (null (cdr list))
      list
      (last-pair (cdr list))))

(defun my-copy-list (list)
  (if (null list)
      ()
      (cons (car list) (my-copy-list (cdr list)))))

(defun my-contains-p (elem list)
  (and (not (null list))
       (or (eql elem (car list))
           (my-contains-p elem (cdr list)))))

(defun equal-lists-p (list1 list2)
  (cond ((not (= (length list1) (length list2))) nil)
        ((null list1) t)
        (t (and (eql (car list1) (car list2)) (equal-lists-p (cdr list1) (cdr list2))))))

(defun my-remove (elem list)
  (cond ((null list) ())
        ((eql elem (car list)) (my-remove elem (cdr list)))
        (t (cons (car list) (my-remove elem (cdr list))))))

(defun remove-nthcdr (n list)
  (if (= n 0)
      ()
      (cons (car list) (remove-nthcdr (- n 1) list))))

(defun each-other (list)
  (if (null list)
      ()
      (cons (car list) (each-other-help (cdr list)))))

(defun each-other-help (list)
  (if (null list)
      ()
      (each-other (cdr list))))

(defun factorials (n)
  (factorials-iter n list))

(defun factorials-iter (n i ir)
  (cond ((= i 0) (cons 1 (factorials-iter n 1 1)))
        ((= i (- n 1)) (cons (* ir i) ()))
        (t (cons (* ir i) (factorials-iter n (+ i 1) (* ir i))))))

(defun list-tails (list)
  (if (null list)
      (cons nil nil)
      (cons list (list-tails (cdr list)))))

(defun list-sum (list)
  (if (null list)
       0
      (+ (car list) (list-sum (cdr list)))))

(defun subtract-lists-2 (list1 list2)
  (if (null list1)
      ()
      (cons (- (car list1) (car list2)) (subtract-lists-2 (cdr list1) (cdr list2)))))

(defun scalar-product (vec1 vec2)
  (if (null vec1)
       0
      (+ (* (car vec1) (car vec2)) (scalar-product (cdr vec1) (cdr vec2)))))

(defun vector-length (vec)
  (sqrt (vector-length-help vec)))

(defun vector-length-help (vec)
  (if (null vec)
       0
      (+ (expt (car vec) 2) (vector-length-help (cdr vec)))))

(defun flatten (list)
    (cond ((null list) nil)
          ((consp (car list)) (append (flatten (car list)) (flatten (cdr list))))
          (t (append (list (car list)) (flatten (cdr list))))))
