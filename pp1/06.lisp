(defun last-pair (list)
    (if (null (cdr list))
        list
        (last-pair (cdr list))))

(defun my-copy-list (list)
    (if (null list)
        ()
        (cons (car list) (my-copy-list (cdr list)))))

(defun equal-lists-p (list1 list2)
    (if (or (null list1) (null list2)) 
        (and (null list1) (null list2))
        (and (eql (car list1) (car list2)) (equal-lists-p (cdr list1) (cdr list2)))))

(defun my-remove (e list)
    (cond ((null list) ())
          ((eql e (car list)) (my-remove e (cdr list)))
          (t (cons (car list) (my-remove e (cdr list))))))

(defun list-sum (list)
    (if (null list)
        0
        (+ (car list) (list-sum (cdr list)))))

(defun remove-nthcdr (n list)
    (if (= n 1)
        ()
        (cons (car list) (remove-nthcdr (- n 1) (cdr list)))))

(defun each-other (list)
    (if (null list)
        ()
        (cons (car list) (each-other-other (cdr list)))))

(defun each-other-other (list)
    (if (null list)
        ()
        (each-other (cdr list))))

(defun factorials (n)
    (factorials-iter n list))

(defun factorials-iter (n i ir)
    (cond ((= i 0) (cons 1 (factorials-iter n 1 1)))
          ((= i (- n 1)) (cons (* ir i) ()))
          (t (cons (* ir i) (factorials-iter n (+ i 1) (* ir i))))))

(defun fib-list (n)
    (fib-list-iter n 0 0 0))

(defun fib-list-iter (n i ir1 ir2)
    (cond ((= n 0) ())
          ((= i 0) (cons 1 (fib-list-iter n (+ i 1) 0 1)))
          ((<  i n) (cons (+ ir1 ir2) (fib-list-iter n (+ i 1) ir2 (+ ir1 ir2))))))

(defun flatten (list)
    (cond ((null list) nil)
          ((consp (car list)) (append (flatten (car list)) (flatten (cdr list))))
          (t (append (list (car list)) (flatten (cdr list))))))
