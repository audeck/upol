(defun my-member (x list)
    (cond ((null list) nil)
          ((eql x (car list)) (cons (car list) (cdr list)))
          (t (my-member x (cdr list)))))

(defun my-member-higher (x list test)
    (cond ((null list) nil)
          ((funcall test x (car list)) (cons (car list) (cdr list)))
          (t (my-member-higher x (cdr list) test))))

(defun my-mapcar (fun list)
    (if (null list)
        '()
        (cons (funcall fun (car list)) 
              (my-mapcar fun (cdr list)))))

(defun foldr (fun list init)
    (if (null list)
        init
        (funcall fun (car list) (foldr fun (cdr list) init))))

(defun foldl (fun list init)
    (list)) ;???

(defun my-length (list)
    (labels ((inc (elem rest) (+ 1 rest)))
        (foldr #'inc list 0)))

(defun my-mapcar-foldr (fun list)
    (labels ((HELP (elem rest)
                 (cons (funcall fun elem) rest)))
        (foldr #'HELP list '())))

(defun arithmetic-mean (&rest numbers)
    (/ (foldr #'+ numbers 0) (length numbers)))


    
