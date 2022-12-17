(defun create-node (state &optional parent-node (node-cost 0) last-action)
    (list state parent-node node-cost last-action) 
)

(defun get-node-state (node)
    (car node)
)

(defun get-parent-node (node)
    (cadr node)
)

(defun get-node-cost (node)
    (caddr node)
)

(defun get-node-last-action (node)
    (last node 0)
)

(defun get-all-parents (node)
    (let ((parent-node (get-parent-node)))
        (cond ((null parent-node) nil)
              (t (cons node (get-all-parents parent-node)))
        ))
)

(defun calc-value (node &optional (h 0))
    (+ (get-node-cost node) h)
)


(defun qsort-nodes (nodes-list &optional (h 0))
    (cond ((null nodes-list) nil)
        (t (append 
                (qsort-nodes (get-sublist-by-comparator (cdr nodes-list) (calc-value (car nodes-list)) '< h))
                (cons (car nodes-list) nil)
                (qsort-nodes (get-sublist-by-comparator (cdr nodes-list) (calc-value (car nodes-list)) '>= h))
            )
        )
    )
)

(defun get-sublist-by-comparator (l num &optional (comparator '<) (h 0))
    (cond ((null l) nil)
        (((funcall comparator (calc-value (car l) h) num)) (cons (car l) (get-sublist-by-comparator (cdr l) num comparator h)))
        (t (get-sublist-by-comparator (cdr l) num comparator h))
    )
)