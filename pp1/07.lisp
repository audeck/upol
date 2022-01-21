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

; (tree-node 5 (list (tree-node 2 (list (tree-node 1 nil) (tree-node 3 (list (tree-node 4 nil))))) (tree-node 7 (list (tree-node 6 nil) (tree-node 8 nil)))))

(defun tree-find (val node)
    (cond ((null node) nil)
          ((eql val (node-value node)) val)
          (t (tree-find-children val (node-children node)))))

(defun tree-find-children (val nodes)
    (cond ((null nodes) nil)
          ((eql val (tree-find val (car nodes))) val) 
          (t (tree-find-children val (cdr nodes)))))

(defun tree-sum (node)
    (cond ((null node) 0)
          (t (+ (node-value node) (tree-sum-multi (node-children node))))))

(defun tree-sum-multi (nodes)
    (cond ((null nodes) 0)
          (t (+ (tree-sum (car nodes)) (tree-sum-multi (cdr nodes))))))

(defun add-to-list (elem list)
  (append list (list elem)))

(defun tree-maximal-paths (tree)
  (if (null tree)
       nil
      (tree-maximal-paths-single tree '() '())))

(defun tree-maximal-paths-single (tree current_path result)
  (if (null (node-children tree))
      (add-to-list (add-to-list (node-value tree) current_path) result)
      (tree-maximal-paths-multi (node-children tree) (add-to-list (node-value tree) current_path) result)))

(defun tree-maximal-paths-multi (trees current_path result)
  (if (null trees)
       result
      (tree-maximal-paths-multi (cdr trees) current_path (tree-maximal-paths-single (car trees) current_path result))))
       
(defun tree-height (tree)
  (if (null tree)
       0
      (tree-height-multi (node-children tree))))

(defun tree-height-multi (trees)
  (if (null trees)
       0
      (max (+ 1 (tree-height (car trees))) (tree-height-multi (cdr trees)))))

(defun find-path (val tree)
  (if (null tree)
       nil
      (find-path-single val tree '())))

(defun find-path-single (val tree path)
  (if (eql val (node-value tree))
      (add-to-list (node-value tree) path)
      (find-path-multi val (node-children tree) (add-to-list (node-value tree) path))))

(defun find-path-multi (val trees path)
  (cond ((null trees) nil)
        ((null (find-path-single val (car trees) path)) (find-path-multi val (cdr trees) path))
        (t (find-path-single val (car trees) path))))
