(defun make-queue ()
  (cons nil nil))

(defun queue-front (q)
  (car q))

(defun (setf queue-front) (front q)
  (setf (car q) front))

(defun queue-rear (q)
  (cdr q))

(defun (setf queue-rear) (rear q)
  (setf (cdr q) rear))

(defun empty-queue-p (q) 
  (null (queue-front q)))

(defun dequeue (q)
  (when (empty-queue-p q)
    (error "Queue is empty"))
  (prog1 (car (queue-front q))
    (setf (queue-front q) (cdr (queue-front q)))))

(defun enqueue (q el)
  (let ((new (cons el nil)))
    (if (empty-queue-p q)
        (setf (queue-front q) new)
      (setf (cdr (queue-rear q)) new))
    (setf (queue-rear q) new)
    el))

;;; EOL

(defun test1 (a) (setf a 2))
(defun test2 (a) (test1 a)a)

(defun test3 (a) (setf (car a) 2))
(defun test4 (a) (test3 a)a)

(defun add-element-after (new el list)
  (let ((tail (member el list)))
    (setf (cdr tail) (cons new (cdr tail)))
    list))

(defun make-queue (&rest elements)
  (let ((q (cons nil nil)))
    (enqueue-list q elements)
    q))

(defun enqueue-list (q list)
  (cond ((null list) q)
        (t (progn (enqueue q (car list))
                  (enqueue-list q (cdr list))))))

(defun queue-front-element (q)
  (first (car q)))

(defun queue-rear-element (q)
  (first (cdr q)))

(defun circlist (&rest elems)
  (let ((list (copy-list elems)))
    (setf (cdr (last list)) list)))

; #1=(a . #2=((b . #1#) . (c . #2#))) - Only the "cyclic" nodes are numbered

(defun bidir-cons (val prev next)
  (list val prev next))

(defun bidir-val (bidir)
  (first bidir))

(defun (setf bidir-val) (val bidir)
  (setf (first bidir) val))

(defun bidir-prev (bidir)
  (second bidir))

(defun (setf bidir-prev) (val bidir)
  (setf (second bidir) val))

(defun bidir-next (bidir)
  (third bidir))

(defun (setf bidir-next) (val bidir)
  (setf (third bidir) val))

(defun bidir-empty-p (bidir)
  (null (car bidir)))

(defun bidir-list (&rest elems)
  (let ((bidir (cons nil nil)))
    (dolist (e elems)
      (bidir-add-end e bidir))
    bidir))

(defun bidir-add-end (e bidir)
  (let ((e-cons (bidir-cons e nil nil)))
    (cond ((bidir-empty-p bidir) (setf (car bidir) e-cons)
                                 (setf (cdr bidir) e-cons))
          (t (setf (bidir-prev e-cons) (cdr bidir))
             (setf (bidir-next (cdr bidir)) e-cons)
             (setf (cdr bidir) e-cons)))
    bidir))


(defun list-to-bidir-list (list)
  (let ((bidir (bidir-list)))
    (dolist (e list)
      (bidir-add-end e bidir))
    bidir))
