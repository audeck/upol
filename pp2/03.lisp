(defmacro awhen (cond &body body)
  `(let ((it ,cond))
     (if it (progn ,@body) nil)))

(defmacro my-prog1 (first &body other)
  (let ((first-sym (gensym "FIRST")))
    `(let ((,first-sym ,first))
       (progn ,first-sym ,@other) 
       ,first-sym)))

; Works, because let doesn't actually bind form1 to result before binding fun (doesn't work with let*)
(defmacro my-prog2 (form1 &body forms)
  `(let ((result ,form1)
         (fun (lambda () ,@forms)))
     (funcall fun result))

; Same thing
(defmacro my-prog3 (first &body body)
  `(let ((result ,first)
         (bresult (progn ,@body)))
     result))

; Works!
(defun my-prog4 (first &rest rest)
  first)

(defmacro my-test (exp)
  `(let ((exp-result ,exp))
     (progn (format t "Vyhodnocovaný výraz: ~S~%" ',exp)  ; ~% prints a newline char (only in format)
            (format t "Hodnota výrazu: ~S" exp-result))
     exp-result))

(defmacro my-and (&optional (form t) &rest forms)
  (if forms
      `(when ,form (my-and ,@forms))
    form))

#|
(my-and)        ; t
(my-and a)      ; (when a (my-and))
(my-and a b)    ; (when a (my-and b))
(my-and a b c)  ; (when a (my-and b c))
|#

(defmacro my-or (&optional (form nil) &rest forms)
  (let ((form-sym (gensym "FORM")))
    (if forms
        `(let ((,form-sym ,form))
           (if ,form-sym ,form-sym (my-or ,@forms)))
      form)))

(defmacro my-let* (bindings &body body)
  (if bindings
      `(let ((,(caar bindings) ,(cadar bindings))) (my-let* ,(cdr bindings) ,@body))
    `(progn ,@body)))

(defmacro all-cond (&rest branches)
  (if branches
      `(let ((current ',(first branches)))
           (when (eval (first current)) (eval (second current)))
           (all-cond ,@(cdr branches)))
    nil))

(defmacro op-alias (op alias)
  `(defmacro ,alias (&body params)
     `(,',op ,@params)))
