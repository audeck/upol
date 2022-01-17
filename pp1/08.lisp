(defun foldr (fun list init)
  (if (null list)
       init
      (funcall fun (car list) (foldr fun (cdr list) init))))

;;; End-Of-Lecture

(defun my-member (elem list)
  (cond ((null list) nil)
        ((eql elem (car list)) list)
        (t (my-member elem (cdr list)))))

(defun my-member-if (elem list fun)
  (cond ((null list) nil)
        ((funcall fun elem (car list)) list)
        (t (my-member-if elem (cdr list) fun))))

(defun my-mapcar (fun list)
  (labels ((help (elem rest)
              (cons (funcall fun elem) rest)))
    (foldr #'help list '())))

(defun foldl (fun list init)
  (labels ((iter (list ir) 
              (if (null list) 
                   ir 
                  (iter (cdr list) (funcall fun ir (car list))))))
    (iter list init)))

(defun arithmetic-mean (a &rest rest)
  (/ (foldr #'+ rest a) (1+ (length rest))))

(defun equal-lists-p (list &rest lists)
  (labels ((truth-list-p (list)
              (if (null list)
                   t
                  (and (car list) (truth-list-p (cdr list))))))
    (truth-list-p (apply #'mapcar #'eql list lists))))

(defun my-mapcar (fun list &rest lists)
  (labels ((help (fun list)
               (if (null list)
                   '()
                   (cons (funcall fun (car list))
                         (my-mapcar fun (cdr list))))))
    (apply #'help fun list lists)))

(defun my-length (list)
    (labels ((inc (elem rest) (+ 1 rest)))
        (foldr #'inc list 0)))
