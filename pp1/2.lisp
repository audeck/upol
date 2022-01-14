(defun triangle-area (b h)
  (* 1/2 b h))

;;; End-Of-Lecture

(defun power2 (a)
  (* a a))

(defun power3 (a)
  (* a a a))

(defun power4 (a)
  (* (power2 a) (power2 a)))

(defun hypotenuse (a b)
  (sqrt (+ (power2 a) (power2 b))))

(defun absolute-value (a)
  (if (< a 0) (- a) a))

(defun my-signum (n)
  (if (= n 0) 0 (/ n (absolute-value n))))

(defun my-positive-p (n)
  (> n 0))

(defun my-negative-p (n)
  (< n 0))

(defun point-distance (A-x A-y B-x B-y)
  (let ((x-leg (- A-x B-x))
        (y-leg (- A-y B-y)))
    (hypotenuse x-leg y-leg)))

(defun triangle-p (a b c)
  (and (> (+ a b) c) (> (+ b c) a) (> (+ c a) b)))

(defun heron (a b c)
  (let ((s (/ (+ a b c) 2)))
    (sqrt (* s (- s a) (- s b) (- s c)))))

(defun heron-cart (A-x A-y B-x B-y C-x C-y)
  (let ((a (point-distance B-x B-y C-x C-y))
        (b (point-distance C-x C-y A-x A-y))
        (c (point-distance A-x A-y B-x B-y)))
    (heron a b c)))
