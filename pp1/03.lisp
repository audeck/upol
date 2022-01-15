(defun power (a n)
  (if (= n 0)
      1
      (* a (power a (- n 1)))))

;;; End-Of-Lecture

(defun my-gcd (a b)
  (if (= b 0)
       a
      (my-gcd b (rem a b))))

(defun heron-sqrt (a epsilon)
  (heron-sqrt-calc a a epsilon))

(defun heron-sqrt-calc (x a epsilon)
  (if (<(abs (- (power2 x) a)) epsilon)
      x
      (heron-sqrt-calc (/ (+ x (/ a x)) 2) a epsilon)))

(defun power2 (n)
  (* n n))

(defun interval-sum (a b)
  (interval-sum-iter a b 0))

(defun interval-sum-iter (a b ir)
  (if (< b a)
      ir
      (interval-sum-iter a (- b 1) (+ ir b))))

(defun power (a n)
  (power-iter a n 1))

(defun power-iter (a n ir)
  (if (= n 0)
      ir
      (power-iter a (- n 1) (* ir a))))

(defun div-by-nine-p (n)
  (and (= (digit-sum n) 9)
       (or (< n 10)
           (div-by-nine-p (digit-sum n)))))

(defun digit-sum (n)
  (digit-sum-i n (- (digit-count n) 1)))

(defun digit-sum-i (n i)
  (if (< i 0)
      0
      (+ (digit n i) (digit-sum-i n (- i 1)))))  ; digit and digit-sum are undefined

(defun leibniz1 (epsilon)
  (* 4 (leibniz1-calc 0 epsilon)))

(defun leibniz1-calc (n epsilon)
  (let ((x (if (evenp n) (/ 1 (+ (* 2 n) 1 )) (* -1 (/ 1 (+ (* 2 n) 1 ))))))
    (if (< (abs x) epsilon)
        x
        (+ x (leibniz1-calc (+ n 1) epsilon)))))
        
