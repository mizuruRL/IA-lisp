(defun create-node (state &optional parent-node (node-level 0) (h 0))
"Function to create the node data-estructure using a state, the parent node, its node level and heuristic value."
    (list state parent-node node-level h) 
)

(defun get-node-state (node)
"Returns a given node's state"
    (car node)
)

(defun get-parent-node (node)
"Returns a given node's parent node"
    (cadr node)
)

(defun get-node-level (node)
"Returns a given node's level"
    (caddr node)
)

(defun get-node-h (node)
"Returns a given node's heuristic value."
    (car (last node))
)

(defun get-node-cost (node)
"Calculates a given node's cost by adding its level and heuristic value."
    (+ (get-node-level node) (get-node-h node))
)

(defun get-all-states (node)
"Returns every state from the current node to the root."
    (let ((parent-node (get-parent-node node)))
        (cond ((null node) nil)
              (t (cons (get-node-state node) (get-all-states parent-node)))
        ))
)

(defun h2 (node target)
"Second heuristic function. It is calculated by checking how far NODE `node is from achieving the box goal TARGET 
and divides it by the sum of adjacent arc types a given horizontal arc has."
    (/ (- target (check-all-closed-boxes (get-node-state node))) (1+ (get-adjacent-hor-arcs (get-node-state node))))
)

(defun h1 (node target)
"First heuristic function. It is calculated by checking how far NODE `node is from achieving the box goal TARGET."
    (- target (check-all-closed-boxes (get-node-state node)))
)

(defun h0 (&optional node target)
"Null heuristic, always returns 0. Really only used as a default value."
    (declare (ignore node target))
    0
)

(defun qsort-nodes (nodes-list)
"Quicksorts a given list of nodes in ascending order by node cost."
    (cond ((null nodes-list) nil)
        (t (append 
                (qsort-nodes (get-sublist-by-comparator (cdr nodes-list) (get-node-cost (car nodes-list)) '<))
                (cons (car nodes-list) nil)
                (qsort-nodes (get-sublist-by-comparator (cdr nodes-list) (get-node-cost (car nodes-list)) '>=))
            )
        )
    )
)

(defun get-sublist-by-comparator (l num &optional (comparator '<))
"Auxiliary function for qsort-nodes. Sorts a list by a given COMPARATOR and NUM value to compare each element `node to."
    (cond ((null l) nil)
        ((funcall comparator (get-node-cost (car l)) num) (cons (car l) (get-sublist-by-comparator (cdr l) num comparator)))
        (t (get-sublist-by-comparator (cdr l) num comparator))
    )
)

(defun solutionp (node solution)
"Checks if NODE `node is objective node, given a number of closed boxes SOLUTION."
    (if (null node)
        nil
        (= (check-all-closed-boxes (get-node-state node)) solution)
    )
)

(defun generate-successors (node action-list algorithm &optional (target 3) (max-depth 1) (h-func 'h0) (line 1) (column 1))
"Returns a list with all succeeding nodes from a given NODE `node. It will apply the actions in ACTION-LIST, according to the ALGORITHM, 
target of closed boxes TARGET, max tree depth DEPTH (for dfs), heuristic function H-FUNC (for a*), starting from a given LINE x COLUMN y"
    (if (and (eql algorithm 'dfs) (= (get-node-level node) max-depth))
        nil
        (let ((succ-node (generate-successor node (car action-list) line column h-func target))
            (action (car action-list))
            (next-pos-hor (increment-pos-horizontal (car (get-node-state node)) line column))
            (next-pos-ver (increment-pos-vertical (cadr (get-node-state node)) line column)))
            (cond ((solutionp succ-node target) (cons succ-node nil))
                ((null action) nil)
                ((and (null succ-node) (eql action 'insert-horizontal-arc) (null next-pos-hor)) (generate-successors node (cdr action-list) algorithm target max-depth h-func))
                ((and (null succ-node) (eql action 'insert-vertical-arc) (null next-pos-ver)) (generate-successors node (cdr action-list) algorithm target max-depth h-func))
                ((and (null succ-node) (eql action 'insert-horizontal-arc)) (generate-successors node action-list algorithm target max-depth h-func (car next-pos-hor) (cadr next-pos-hor)))
                ((and (null succ-node) (eql action 'insert-vertical-arc)) (generate-successors node action-list algorithm target max-depth h-func (car next-pos-ver) (cadr next-pos-ver)))
                ((eql action 'insert-horizontal-arc) (cons succ-node (generate-successors node action-list algorithm target max-depth h-func (car next-pos-hor) (cadr next-pos-hor))))
                ((eql action 'insert-vertical-arc) (cons succ-node (generate-successors node action-list algorithm target max-depth h-func (car next-pos-ver) (cadr next-pos-ver))))
            )
        )
    )
)

(defun successor-exists (succ-node l)
"Checks if a SUCC-NODE `node already exists in a given L `list."
    (not (null (member (get-node-state succ-node) (mapcar 'car l))))
)

(defun generate-successor (node action &optional (line 1) (column 1) (h-func 'h0) (target 3))
"Generates a succeeding node from a given NODE `node. It will perform ACTION on its state, at LINE x COLUMN y, with a given heuristic function H-FUNC
and a box target TARGET."
    (if (or (null action) (null line) (null column))
        nil
        (let ((board (funcall action (get-node-state node) line column)))
            (if (null board) nil (create-node board node (1+ (get-node-level node)) (funcall h-func (create-node board node (1+ (get-node-level node))) target)))
        )
    )
)

(defun all-actions-list ()
"Returns all possible operators."
    '(insert-horizontal-arc insert-vertical-arc)
)

(defun show-solution (objective-node &optional total-nodes total-expanded-nodes (start-time 0) (end-time (get-internal-real-time)))
"Returns a list with values needed to format the solution."
    (let ((L (get-node-level objective-node))
    )
        (list objective-node total-nodes total-expanded-nodes (penetrance L total-nodes) (float (effective-branching-factor L total-nodes total-nodes L)) (- end-time start-time))
    )
)

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

(defun remove-dupes (open-list closed-list)
"Removes duplicate nodes from OPEN-LIST that are present in CLOSED-LIST."
    (let ((node (car open-list)))
        (cond ((null open-list) nil)
              ((successor-exists node closed-list) (remove-dupes (cdr open-list) closed-list))
              (t (cons node (remove-dupes (cdr open-list) closed-list)))
        )    
    )
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