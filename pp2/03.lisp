

(defmacro op-alias (op alias)
  `(defmacro ,alias (&body params)
     `(,',op ,@params)))
