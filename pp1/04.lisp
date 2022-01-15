(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 2)) (fib (- n 1))))))

(defun count-change (amount) 
  (cc amount 6))

(defun cc (amount kinds)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds 0)) 0) 
        (t (+ (cc amount (- kinds 1))
              (cc (- amount (first-denom kinds)) kinds)))))

(defun first-denom (kinds) 
  (cond ((= kinds 1) 1) 
        ((= kinds 2) 2) 
        ((= kinds 3) 5)
        ((= kinds 4) 10)
        ((= kinds 5) 20)
        ((= kinds 6) 50)))

(defun power2 (n)
  (* n n))

(defun fast-power (a n)
  (cond ((= n 0) 1)
        ((evenp n) (power2 (fast-power a (/ n 2))))
        (t (* a (fast-power a (- n 1))))))

;;; End-Of-Lecture

(defun fast-power-i (a n)
  (fast-power-iter a n 1))

(defun fast-power-iter (a n ir)
  (cond ((= n 0) ir)
        ((evenp n) (fast-power-iter a (/ n 2) (* ir (fast-power-i a (/ n 2)))))
        (t (fast-power-iter a (- n 1) (* ir a)))))

(defun divides-p (a b)
  (if (= (rem b a) 0) t nil))

(defun my-rem (a b)
  (cond ((= b 0) nil)
        ((< a b) a)
        (t (my-rem (- a b) b))))

(defun prime-p (n)
  (prime-p-index n (- n 1)))

(defun prime-p-index (n i)
  (if (< i 2)
      t
      (and (not (divides-p i n)) (prime-p-index n (- i 1)))))

(defun perfect-p (n)
  (if (= (perfect-p-index n (- n 1)) n) t nil))

(defun perfect-p-index (n i)
  (cond ((= i 0) 0)
        ((divides-p i n) (+ i (perfect-p-index n (- i 1))))
        (t (perfect-p-index n (- i 1)))))

(defun pascal (y x)
  (if (or (<= x 0) (>= x y))
      1
      (+ (pascal (- y 1) (- x 1)) (pascal (- y 1) x))))

(defun fib (n)
  (fib-iter n 0 0 0 0))

(defun fib-iter (n i r1 r2 ir)
  (cond ((<= n i) ir)
        ((= i 0) (fib-iter n 1 0 0 0))
        ((= i 1) (fib-iter n 2 1 0 1))
        (t (fib-iter n (+ i 1) (+ r1 r2) r1 (+ ir r1)))))
