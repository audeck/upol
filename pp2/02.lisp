(defmacro my-when (condition &rest rest)
  `(if ,condition (progn ,@rest) NIL))

(defmacro and-2 (ex1 ex2)
  `(if ,ex1 ,ex2 nil))

#|
(or-2 a b)

(if a t b)
|#

(defmacro or-2 (ex1 ex2)
  `(if ,ex1 ,ex1 ,ex2))

#|
(if-zero condition a b)

(if (= condition 0) a b)
|#

(defmacro if-zero (condition a b)
  `(if (= ,condition 0) ,a ,b))

#|
(defmacro my-unless (condition &rest expressions)
  `(if ,condition nil (progn ,@expressions)))
|#

(defmacro my-unless (condition &rest expressions)
  `(my-when (not ,condition) ,@expressions))

(defmacro whenb (sym condition &rest expressions)
  `(let ((,sym ,condition)) (when ,sym ,@expressions)))

(defmacro reverse-progn (&rest body)
  `(progn ,@(reverse body)))

(defmacro my-let (bindings &rest body)
  `(funcall (lambda (,@(mapcar #'first bindings)) ,@body) ,@(mapcar #'second bindings)))
