(defun mem (seq index)
  (funcall seq index))

(defun accumulate (seq from to combiner null-val)
  (if (< to from)
       null-val
      (funcall combiner (mem seq from) (accumulate seq (1+ from) to combiner null-val))))

(defun seq-to-list (seq n)
  (labels ((internal (i)
             (if (>= i n)
                 '()
                 (cons (mem seq i) (internal (1+ i))))))
    (internal 0)))

;;; End-Of-Lecture

(defun max-elem (seq lower upper)
  (accumulate seq lower upper #'max -999999))  ; null-val should probably be something like a REAL-MIN const

(defun min-elem (seq lower upper)
  (accumulate seq lower upper #'min 999999))

(defun constant-seq-p (seq k)
  (= (max-elem seq 0 (1- k)) (min-elem seq 0 (1- k))))

(defun increasing-seq-p (seq k)
  (or (<= k 1)
      (and (funcall (lambda (n1 n2) (< n1 n2)) (mem seq (- k 2)) (mem seq (- k 1)))
           (increasing-seq-p seq (- k 1)))))

(defun even-members (seq)
  (lambda (n) (funcall seq (* 2 n))))

(defun zero-mem-p (tbl row column)
  (= (funcall tbl row column) 0))

; 10x10 table
(defun zero-row-p (tbl row)
  (labels ((search (column)
             (or (> row 9)
                 (and (zero-mem-p tbl row column)
                      (search (1+ column))))))
    (search 0)))

(defun transpose-table (tbl)
  (lambda (row column) (funcall tbl column row)))
