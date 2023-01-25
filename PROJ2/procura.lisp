(defun bfs (&optional open-list closed-list target (start-time (get-internal-real-time)))
"Breadth-first-search algorithm implementation. Starts with a list of `node OPEN-LIST which it will expand the first node from. CLOSED-LIST
represents the already explores nodes. The search will continue until TARGET is met."
    (cond ((null open-list) nil)
          (t 
            (let ((successors (remove-dupes (generate-successors (car open-list) (all-actions-list) 'bfs target) (append open-list closed-list))))
                (if (solutionp (car (last successors)) target)
                    (show-solution (car (last successors)) (total-nodes (append (cdr open-list) successors) (append closed-list (cons (car open-list) nil))) (length (append closed-list (cons (car open-list) nil))) start-time)
                    (bfs (append (cdr open-list) successors) (append closed-list (cons (car open-list) nil)) target start-time)
                )
            )
        )
    )
)

(defun dfs (&optional open-list closed-list target max-depth (start-time (get-internal-real-time)))
"Depth-first-search algorithm implementation. Starts with a list of `node OPEN-LIST which it will expand the first node from. CLOSED-LIST
represents the already explores nodes. The search will continue until TARGET is met or MAX-DEPTH is reached."
    (cond ((null open-list) nil)
          (t 
            (let ((successors (remove-dupes (generate-successors (car open-list) (all-actions-list) 'dfs target max-depth) (append open-list closed-list))))
                (if (solutionp (car (last successors)) target)
                    (show-solution (car (last successors)) (total-nodes (append (cdr open-list) successors) (append closed-list (cons (car open-list) nil))) (length (append closed-list (cons (car open-list) nil))) start-time)
                    (dfs (append successors (cdr open-list)) (append closed-list (cons (car open-list) nil)) target max-depth start-time)
                )
            )
        )
    )
)

(defun a* (&optional open-list closed-list target (h-func 'h0) (start-time (get-internal-real-time)))
"A* informed algorithm implementation. Starts with a list of `node OPEN-LIST which it will expand the first node from. CLOSED-LIST
represents the already explores nodes. The search will continue until TARGET is met. Heuristic value is calculated using a heuristic function
H-FUNC."
    (cond ((null open-list) nil)
          ((eql 'h0 h-func) (bfs open-list closed-list target))
          (t 
            (let ((successors (remove-dupes (qsort-nodes (generate-successors (car open-list) (all-actions-list) 'a* target 1 h-func)) (append open-list closed-list))))
                (if (solutionp (car successors) target)
                    (show-solution (car successors) (total-nodes (append (cdr open-list) successors) (append closed-list (cons (car open-list) nil))) (length (append closed-list (cons (car open-list) nil))) start-time)
                    (a* (append successors (cdr open-list)) (append closed-list (cons (car open-list) nil)) target h-func start-time)
                )
            )
        )
    )
)

(defun negamax (node depth player)
    (cond ((= depth 0) (eval-node node))
          (t form2))
)



(defun total-nodes (open closed)
"Returns an integer with the sum of nodes present in OPEN and CLOSED lists"
    (+ (length open) (length closed))
)

(defun penetrance (L Tn)
"Calculates a tree's penetrance given L depth and TN total nodes."
    (float (/ L Tn))
)

(defun polynom (B L Tn)
"Calculates the polynomial function for the effective branching factor: B^L - TN = 0"
    (if (= l 1)
        (- B Tn)
        (+ (expt B L) (polynom B (1- L) Tn))
    )
)

(defun effective-branching-factor (L Tn &optional upper-bound lower-bound (dev 0.1))
"Calculates the approximate value of the effective-branching-factor via bissection method."
    (let ((average-bound (/ (+ upper-bound lower-bound) 2 ))
        (diff-bound (- upper-bound lower-bound))
        (polyn (polynom (/ (+ upper-bound lower-bound) 2 ) lower-bound upper-bound)))
        (cond ((< diff-bound dev) average-bound)
              ((< polyn 0) (effective-branching-factor L Tn upper-bound average-bound dev))
              (T (effective-branching-factor L Tn average-bound lower-bound dev))
        )
    )
)