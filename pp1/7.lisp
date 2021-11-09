(defun tree-node (val children)
    (cons 'tree (cons val children)))

; (setf tree (tree-node 4 (list (tree-node 1 nil) (tree-node 5 (list (tree-node 2 nil) (tree-node 3 nil) (tree-node 1 (list (tree-node 3 nil) (tree-node 6 nil))))) (tree-node 9 (list (tree-node 2 nil) (tree-node 11 nil))))))

(defun binary-tree-node (val left-child right-child)
    (list 'binary-tree val left-child right-child))

(defun left-child (node)
    (caddr node))

(defun right-child (node)
    (cadddr node))

(defun tree-type (node)
    (car node))

(defun treep (node)
    (eql (tree-type node) 'tree))

(defun binary-treep (node)
    (eql (tree-type node) 'binary-tree))

(defun node-value (node)
    (cadr node))

(defun node-children (node)
    (cond ((treep node) (cddr node))
          ((binary-treep node) (remove nil (cddr node)))
          (t (error "Unknown tree type."))))

;;; EOL

(defun plist-remove (plist prop)
    (cond ((null plist) nil)
          ((eql (car plist) prop) (plist-remove (cddr plist) prop))
          (t (cons (car plist) (cons (cadr plist) (plist-remove (cddr plist) prop))))))

(defun plist-add (plist prop val)
    (cond ((null plist) (cons prop (cons val nil)))
          ((eql (car plist) prop) (cons prop (cons val (cddr plist))))
          (t (cons (car plist) (cons (cadr plist) (plist-add (cddr plist) prop val))))))

; (tree-node 5 (list (tree-node 2 (list (tree-node 1 nil) (tree-node (tree-node 3 nil) (list (tree-node 4 nil))))) (tree-node 7 (list (tree-node 6 nil) (tree-node 8 nil)))))

(defun tree-find (val node)
    (cond ((null node) nil)
          ((eql val (node-value node)) val)
          (t (tree-find-children val (node-children node)))))

(defun tree-find-children (val nodes)
    (cond ((null nodes) nil)
          ((eql val (tree-find val (car nodes))) val) 
          (t (tree-find-children val (cdr nodes)))))
