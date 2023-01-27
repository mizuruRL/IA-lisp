(defun create-node (state &optional parent-node (node-level 0) (h 0))
"Function to create the node data-structure using a state, the parent node, its node level and heuristic value. State is a
list where the first element is the board representation, the second element represents the first player's score, and the third
element represents the second player's score."
    (list state parent-node node-level h) 
)

(defun get-node-state (node)
"Returns a given node's state"
    (car node)
)

(defun get-node-state-board (node)
"Returns a given node's board, that is stored within node's state."
    (car (get-node-state node))
)

(defun get-node-state-score-player (node player)
"Returns a given node's player score, that is stored within node's state."
    (if (= 1 player)
        (second (get-node-state node))
        (third (get-node-state node))
    )   
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

(defun h2 (board player)
"Heuristic function to order nodes for negamax. It returns the sum of types of adjacent arcs relative to horizontal arcs on a given board, for a given player."
    (get-adjacent-hor-arcs board player)
)

(defun qsort-nodes (nodes-list)
"Quicksorts a given list of nodes in descending order by node heuristic."
    (cond ((null nodes-list) nil)
        (t (append 
                (qsort-nodes (get-sublist-by-comparator (cdr nodes-list) (get-node-h (car nodes-list)) '>=))
                (cons (car nodes-list) nil)
                (qsort-nodes (get-sublist-by-comparator (cdr nodes-list) (get-node-h (car nodes-list)) '<))
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

(defun generate-successors (node action-list player &optional (h-func 'h2) (line 1) (column 1))
"Returns a list with all succeeding nodes from a given NODE `node. It will apply all possible actions from action-list for player and 
saves the heuristic function's H-FUNC result to the respective successor."
    (if (null node)
        nil
        (let ((succ-node (generate-successor node (car action-list) line column h-func player))
            (action (car action-list))
            (next-pos-hor (increment-pos-horizontal (car (get-node-state-board node)) line column))
            (next-pos-ver (increment-pos-vertical (cadr (get-node-state-board node)) line column)))
            (cond 
                ((null action) nil)
                ((and (null succ-node) (eql action 'insert-horizontal-arc) (null next-pos-hor)) (generate-successors node (cdr action-list) player h-func))
                ((and (null succ-node) (eql action 'insert-vertical-arc) (null next-pos-ver)) (generate-successors node (cdr action-list) player h-func))
                ((and (null succ-node) (eql action 'insert-horizontal-arc)) (generate-successors node action-list player h-func (car next-pos-hor) (cadr next-pos-hor)))
                ((and (null succ-node) (eql action 'insert-vertical-arc)) (generate-successors node action-list player h-func (car next-pos-ver) (cadr next-pos-ver)))
                ((eql action 'insert-horizontal-arc) (cons succ-node (generate-successors node action-list player h-func (car next-pos-hor) (cadr next-pos-hor))))
                ((eql action 'insert-vertical-arc) (cons succ-node (generate-successors node action-list player h-func (car next-pos-ver) (cadr next-pos-ver))))
            )
        )
    )
)

(defun generate-successor (node action &optional (line 1) (column 1) (h-func 'h2) (player 1))
"Generates a succeeding node from a given NODE `node. It will perform ACTION on its state, at LINE x COLUMN y, with a given heuristic function H-FUNC
for a given player."
    (if (or (null action) (null line) (null column))
        nil
        (let ((board (funcall action (get-node-state-board node) line column player)))
            (if (null board)
                nil
                (create-node (list board (check-all-closed-boxes board 1) (check-all-closed-boxes board 2)) node (1+ (get-node-level node)) (funcall h-func board player))
            )
        )
    )
)

(defun all-actions-list ()
"Returns all possible operators."
    '(insert-horizontal-arc insert-vertical-arc)
)

(defun insert-vertical-arc (board line column &optional (player 1))
"Inserts a vertical arc on BOARD at LINE COLUMN for a given player."
    (cond ((null board) nil)
        ((or (not (in-bounds-vertical (cadr board) line column)) (not (= 0 (get-vertical-arc-at (cadr board) line column)))) nil)
        (t (list (car board) (append (get-preceeding (cadr board) column) (cons (replace-n (get-n (cadr board) column) line player) nil) (nthcdr column (cadr board)))))
    )
)

(defun insert-horizontal-arc (board line column &optional (player 1))
"Inserts a horizontal arc on BOARD at LINE COLUMN for a given player."
    (cond ((null board) nil)
        ((or (not (in-bounds-horizontal (car board) line column)) (not (= 0 (get-horizontal-arc-at (car board) line column)))) nil)
        (t (list (append (get-preceeding (car board) line) (cons (replace-n (get-n (car board) line) column player) nil) (nthcdr line (car board))) (cadr board)))
    )
)

(defun check-box (board line column &optional (player 1))
"Checks if there is a box at LINE COLUMN from BOARD from a given player."
    (cond ((null board) nil)
          (t (and (= player (get-vertical-arc-at (cadr board) line column)) (= player (get-vertical-arc-at (cadr board) line (1+ column))) (= player (get-horizontal-arc-at (car board) line column)) (= player (get-horizontal-arc-at (car board) (1+ line) column))))
        )      
)

(defun check-all-closed-boxes (board player &optional (line 1) (column 1))
"Checks how many closed boxes a given BOARD has for a given player."
    (cond ((not (in-bounds-horizontal (car board) (1+ line) 1)) 0)
          ((not (in-bounds-horizontal (car board) 1 column)) (check-all-closed-boxes board player (1+ line) 1))
          ((check-box board line column player) (1+ (check-all-closed-boxes board player line (1+ column))))
          (t (check-all-closed-boxes board player line (1+ column)))
    )
)

(defun increment-pos-horizontal (horizontal-list line column)
"Increments LINE COLUMN to next coordinates available from HORIZONTAL-LIST."
    (cond ((or(null horizontal-list) (null line) (null column)) nil)
            ((in-bounds-horizontal horizontal-list line (1+ column)) (list line (1+ column)))
            ((in-bounds-horizontal horizontal-list (1+ line) 1) (list (1+ line) 1))
            (t nil)
    )
)

(defun increment-pos-vertical (vertical-list line column)
"Increments LINE COLUMN to next coordinates available from VERTICAL-LIST."
    (cond ((or(null vertical-list) (null line) (null column)) nil)
            ((in-bounds-vertical vertical-list (1+ line) column) (list (1+ line) column))
            ((in-bounds-vertical vertical-list 1 (1+ column)) (list 1 (1+ column)))
            (t nil)
    )
)

(defun in-bounds-vertical (vertical-list line column)
"Checks if LINE COLUMN coordinate is in bounds for VERTICAL-LIST."
    (if (or (null line) (null column))
        nil
        (and (>= (length (car vertical-list)) line) (>= (length vertical-list) column) (< 0 line) (< 0 column))
    )
)

(defun in-bounds-horizontal (horizontal-list line column)
"Checks if LINE COLUMN coordinate is in bounds for HORIZONTAL-LIST."
    (if (or (null line) (null column))
        nil
        (and (>= (length (car horizontal-list)) column) (>= (length horizontal-list) line) (< 0 line) (< 0 column))
    )
)

(defun get-preceeding (l n)
"Gets sublist of preceeding elements at N nth from L list."
    (subseq l 0 (1- n))
)

(defun replace-n (l i n)
"Replaces index I element with N element on L list."
    (cond ((null l) nil)
          ((= i 1) (cons n (cdr l)))
          (t (cons (car l) (replace-n (cdr l) (1- i) n)))
    )
)

(defun get-n (l n)
"Gets N nth element from L list."
        (nth (1- n) l)
)

(defun get-vertical-arc-at (columns-list line column)
"Gets vertical arc on COLUMNS-LIST, from given LINE COLUMN coordinate."
        (nth (1- line) (get-n columns-list column))
)

(defun get-horizontal-arc-at (lines-list line column)
"Gets horizontal arc on LINES-LIST, from given LINE COLUMN coordinate."
        (nth (1- column) (get-n lines-list line))
)

(defun get-adjacent-hor-arcs (board player &optional (line 1) (column 1))
"Gets the sum of all types of adjacent arcs on given BOARD for each horizontal adjacent arc."
    (if (null board)
        nil
        (cond ((or (null column) (null line)) 0)
              (t (let ((new-coords (increment-pos-horizontal (car board) line column))
                        (hor-arc (get-horizontal-arc-at (car board) line column)))
                    (cond ((and (= player hor-arc) (has-adjacent-ver board player line column) (has-adjacent-hor board player line column)) (+ 2 (get-adjacent-hor-arcs board player (car new-coords) (cadr new-coords))))
                          ((and (= player hor-arc) (has-adjacent-ver board player line column)) (1+ (get-adjacent-hor-arcs board player (car new-coords) (cadr new-coords))))
                          (t (get-adjacent-hor-arcs board player (car new-coords) (cadr new-coords))))
                    )
                )
        )
    )
)

(defun has-adjacent-ver (board player &optional (line 1) (column 1))
"Checks on a BOARD if any horizontal arc has another horizontal arc adjacent to it."
    (let ((ver-arcs (cadr board)))
        (or (and (in-bounds-vertical ver-arcs line column) (or (= player (get-vertical-arc-at ver-arcs line column)) (= player (get-vertical-arc-at ver-arcs line (1+ column))))) (and (in-bounds-vertical ver-arcs (1- line) column) (or (= player (get-vertical-arc-at ver-arcs (1- line) column)) (= player (get-vertical-arc-at ver-arcs (1- line) (1+ column))) ) ) )
    )
)

(defun has-adjacent-hor (board player &optional (line 1) (column 1))
"Checks on a BOARD if any horizontal arc has another horizontal arc adjacent to it."
    (let ((hor-arcs (car board)))
        (or (and (in-bounds-horizontal hor-arcs line (1- column)) (= player (get-horizontal-arc-at hor-arcs line (1- column)))) (and (in-bounds-horizontal hor-arcs line (1+ column)) (= player (get-horizontal-arc-at hor-arcs line (1+ column)))))
    )
)

(defun count-total-arcs-player (board player)
"Counts the total arcs that exist on the board for a given player."
    (let ((ver-arcs (cadr board))
        (hor-arcs (car board)))
        (+ (count-arcs-player ver-arcs player) (count-arcs-player hor-arcs player))
    )
)

(defun count-arcs-player (arcs player)
"Counts the arcs that a player has on the board."
    (count player (apply #'append arcs))
)

(defun starting-board ()
"Returns the default starting board."
    '(((0 0 0 0 0 0)(0 0 0 0 0 0)(0 0 0 0 0 0)(0 0 0 0 0 0)(0 0 0 0 0 0)(0 0 0 0 0 0))((0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)))
)

(defun play-cpu (state depth player &optional (max-player 2))
"Simulates a play for a bot on a given state, by running negamax on it, with a given depth. Player is the starting player value. Negamax tries to maximize
for max-player"
    (reset-vars state)
    (negamax (create-node state) depth player most-negative-fixnum most-positive-fixnum 1 max-player)
)

(defun play-human (state arc-type line column player)
"Makes a new state, based on given state, with the given changes through line column and player. This simulates a human play."
    (if (null arc-type)
        nil
        (let ((new-board (funcall arc-type (car state) line column player)))
            (if (null new-board)
                nil
                (list new-board (check-all-closed-boxes new-board player) (check-all-closed-boxes new-board (switch-player player)))
            )
        )
    )
)

(defun get-arc-fun (arc-type)
"Converts arc-type to the corresponding action in all-actions-list. 'h -> insert-horizontal-arc; 'v -> insert-vertical-arc."
    (cond ((eql arc-type 'h) (car (all-actions-list)))
          ((eql arc-type 'v) (cadr (all-actions-list)))
          (T nil)
    )
)
(defun terminal-p (node)
"Checks if given node is a terminal node (no more moves)."
    (let ((hor-arcs (car (get-node-state-board node)))
        (ver-arcs (cadr (get-node-state-board node))))
        (and (no-more-moves hor-arcs) (no-more-moves ver-arcs))
    )
)

(defun no-more-moves (arcs)
"Checks if there are no more moves for a given arc list ARCS." 
    (every #'identity (mapcar (lambda (l)
        (every #'identity (mapcar (lambda (arc)
            (if (= arc 0) nil t)) l))) arcs)
    )
)

(defun switch-player (player)
"Switches player value."
    (if (= player 1)
        2
        1
    )
)

(defun eval-node (node player)
"Evaluates a given node's score. The formula it follows is: 
(total player arcs + (total types of adjacent player arcs to horizontal arcs * 2) + (total player boxes * 3). 
The opposing player's node evaluation is subtracted from the given player's evaluation."
    (- (+ (* 3 (get-node-state-score-player node player)) (count-total-arcs-player (get-node-state-board node) player) (* 2 (get-adjacent-hor-arcs (get-node-state-board node) player))) (+ (* 3 (get-node-state-score-player node (switch-player player))) (count-total-arcs-player (get-node-state-board node) (switch-player player)) (* 2 (get-adjacent-hor-arcs (get-node-state-board node) (switch-player player)))))
)