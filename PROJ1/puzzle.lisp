(defun create-node (state &optional parent-node (node-level 0) (h 0))
    (list state parent-node node-level h) 
)

(defun get-node-state (node)
    (car node)
)

(defun get-parent-node (node)
    (cadr node)
)

(defun get-node-level (node)
    (caddr node)
)

(defun get-node-h (node)
    (car (last node))
)

(defun get-node-cost (node)
    (+ (get-node-level node) (get-node-h node))
)

(defun get-all-parents (node)
    (let ((parent-node (get-parent-node)))
        (cond ((null parent-node) nil)
              (t (cons node (get-all-parents parent-node)))
        ))
)

(defun get-all-states (node)
    (let ((parent-node (get-parent-node node)))
        (cond ((null node) nil)
              (t (cons (get-node-state node) (get-all-states parent-node)))
        ))
)

(defun h1 (node target)
    (- target (check-all-closed-boxes (get-node-state node)))
)

(defun h0 (&optional node target)
    0
)

(defun qsort-nodes (nodes-list)
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
    (cond ((null l) nil)
        ((funcall comparator (get-node-cost (car l)) num) (cons (car l) (get-sublist-by-comparator (cdr l) num comparator)))
        (t (get-sublist-by-comparator (cdr l) num comparator))
    )
)

(defun solutionp (node solution)
    (if (null node)
        nil
        (= (check-all-closed-boxes (get-node-state node)) solution)
    )
)

(defun generate-successors (node action-list algorithm &optional (target 3) (max-depth 1) (h-func 'h0) (line 1) (column 1))
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
    (not (null (member (get-node-state succ-node) (mapcar 'car l))))
)

(defun generate-successor (node action &optional (line 1) (column 1) h-func (target 3))
    (if (null action)
        nil
        (let ((board (funcall action (get-node-state node) line column)))
            (if (null board) nil (create-node board node (1+ (get-node-level node)) (funcall h-func (create-node board node (1+ (get-node-level node))) target)))
        )
    )
)

(defun all-actions-list ()
    '(insert-horizontal-arc insert-vertical-arc)
)

(defun show-solution (objective-node &optional total-nodes total-expanded-nodes)
    (list objective-node total-nodes total-expanded-nodes)
)

(defun bfs (&optional open-list closed-list target)
    (cond ((null open-list) nil)
          ((successor-exists (car open-list) closed-list) (bfs (cdr open-list) closed-list target))
          (t 
            (let ((successors (generate-successors (car open-list) (all-actions-list) 'bfs target)))
                (if (solutionp (car (last successors)) target)
                    (show-solution (car (last successors)) (total-nodes open-list closed-list) (length closed-list))
                    (bfs (append (cdr open-list) successors) (append closed-list (cons (car open-list) nil)) target)
                )
            )
        )
    )
)

(defun dfs (&optional open-list closed-list target max-depth)
    (cond ((null open-list) nil)
          ((successor-exists (car open-list) closed-list) (dfs (cdr open-list) closed-list target max-depth))
          (t 
            (let ((successors (generate-successors (car open-list) (all-actions-list) 'dfs target max-depth)))
                (if (solutionp (car (last successors)) target)
                    (show-solution (car (last successors)) (total-nodes open-list closed-list) (length closed-list))
                    (dfs (append successors (cdr open-list)) (append closed-list (cons (car open-list) nil)) target max-depth)
                )
            )
        )
    )
)

(defun a* (&optional open-list closed-list target (h-func 'h0))
    (cond ((null open-list) nil)
          ((eql 'h0 h-func) (bfs open-list closed-list target))
          ((successor-exists (car open-list) closed-list) (a* (cdr open-list) closed-list target h-func))
          (t 
            (let ((successors (qsort-nodes (generate-successors (car open-list) (all-actions-list) 'a* target 1 h-func))))
                (if (solutionp (car successors) target)
                    (show-solution (car (last successors)) (total-nodes open-list closed-list) (length closed-list))
                    (a* (append (cdr open-list) successors) (append closed-list (cons (car open-list) nil)) target h-func)
                )
            )
        )
    )
)

(defun total-nodes (open closed)
    (+ (length open) (length closed))
)

(defun insert-vertical-arc (board line column)
    (cond ((null board) nil)
        ((or (not (in-bounds-vertical (cadr board) line column)) (= 1 (get-vertical-arc-at (cadr board) line column))) nil)
        (t (list (car board) (append (get-preceeding (cadr board) column) (cons (replace-n (get-n (cadr board) column) line 1) nil) (nthcdr column (cadr board)))))
    )
)

(defun insert-horizontal-arc (board line column)
    (cond ((null board) nil)
        ((or (not (in-bounds-horizontal (car board) line column)) (= 1 (get-horizontal-arc-at (car board) line column))) nil)
        (t (list (append (get-preceeding (car board) line) (cons (replace-n (get-n (car board) line) column 1) nil) (nthcdr line (car board))) (cadr board)))
    )
)

(defun check-box (board line column)
    (cond ((null board) nil)
          (t (and (= 1 (get-vertical-arc-at (cadr board) line column)) (= 1 (get-vertical-arc-at (cadr board) line (1+ column))) (= 1 (get-horizontal-arc-at (car board) line column)) (= 1 (get-horizontal-arc-at (car board) (1+ line) column))))
        )      
)

(defun check-all-closed-boxes (board &optional (line 1) (column 1))
    (cond ((not (in-bounds-horizontal (car board) (1+ line) 1)) 0)
          ((not (in-bounds-horizontal (car board) 1 column)) (check-all-closed-boxes board (1+ line) 1))
          ((check-box board line column) (1+ (check-all-closed-boxes board line (1+ column))))
          (t (check-all-closed-boxes board line (1+ column)))
    )
)

(defun increment-pos-horizontal (horizontal-list line column)
    (cond ((null horizontal-list) nil)
            ((in-bounds-horizontal horizontal-list line (1+ column)) (list line (1+ column)))
            ((in-bounds-horizontal horizontal-list (1+ line) 1) (list (1+ line) 1))
            (t nil)
    )
)

(defun increment-pos-vertical (vertical-list line column)
    (cond ((null vertical-list) nil)
            ((in-bounds-vertical vertical-list (1+ line) column) (list (1+ line) column))
            ((in-bounds-vertical vertical-list 1 (1+ column)) (list 1 (1+ column)))
            (t nil)
    )
)

(defun in-bounds-vertical (vertical-list line column)
    (and (>= (length (car vertical-list)) line) (>= (length vertical-list) column) (< 0 line) (< 0 column))
)

(defun in-bounds-horizontal (horizontal-list line column)
    (and (>= (length (car horizontal-list)) column) (>= (length horizontal-list) line) (< 0 line) (< 0 column))
)

(defun get-preceeding (l n)
    (subseq l 0 (1- n))
)

(defun replace-n (l i n)
    (cond ((null l) nil)
          ((= i 1) (cons n (cdr l)))
          (t (cons (car l) (replace-n (cdr l) (1- i) n)))
    )
)

(defun get-n (l n)
        (nth (1- n) l)
)

(defun get-vertical-arc-at (columns-list line column)
        (nth (1- line) (get-n columns-list column))
)

(defun get-horizontal-arc-at (lines-list line column)
        (nth (1- column) (get-n lines-list line))
)

(defun test ()
    (setq board '(((0 0 0)(0 0 1)(0 1 1)(0 0 1)) ((0 0 0)(0 1 0)(0 0 1)(0 1 1))))
    (insert-horizontal-arc (insert-vertical-arc board 1 4) 3 1)
)