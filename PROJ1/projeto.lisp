(load (compile-file "puzzle.lisp"))
(load (compile-file "procura.lisp"))

(defun load-problems (path-to-file)
"Loads a problem file to LISP stream given PATH-TO-FILE."
    (load-file path-to-file)
)

(defun load-file (path-to-file)
"Loads a file to LISP stream given PATH-TO-FILE."
    (with-open-file (stream path-to-file)
        (loop for line = (read-line stream nil)
            while line
            collect (read-from-string line)
        )
    )
)

(defun display-board (board)
"Returns a string with the BOARD built for the LISP CLI."
    (build-board (car board)(cadr board))
)

(defun build-board-horizontal-arc (line-list)
"Returns a string with all of the lines in LINE-LIST represented for the CLI."
    (cond ((null line-list) ".")
          ((not (numberp (car line-list))) nil)
          ((= (car line-list) 0) (concatenate 'string ".    " (build-board-horizontal-arc (cdr line-list))))
          ((= (car line-list) 1) (concatenate 'string ".----" (build-board-horizontal-arc (cdr line-list))))
          (t nil)
    )
)

(defun build-board-vertical-arc (vertical-list)
"Returns a string with all of the columns in VERTICAL-LIST represented for the CLI."
     (cond ((null vertical-list) "")
          ((not (numberp (car vertical-list))) nil)
          ((= (car vertical-list) 0) (concatenate 'string "     " (build-board-vertical-arc (cdr vertical-list))))
          ((= (car vertical-list) 1) (concatenate 'string "|    " (build-board-vertical-arc (cdr vertical-list))))
          (t nil)
    )
)

(defun build-board (lines-list column-list)
"Returns a string with the visual representation for the CLI of the board, given LINES-LIST and COLUMN-LIST."
    (let ((vertical-list (mapcar #'car column-list))
          (next-column-list (mapcar #'cdr column-list)))
        (cond ((null lines-list) "")
              ((null lines-list) (concatenate 'string (build-board-vertical-arc vertical-list) '(#\NewLine) (build-board lines-list next-column-list)))
              (t (concatenate 'string (build-board-horizontal-arc (car lines-list)) '(#\NewLine) (build-board-vertical-arc vertical-list) '(#\NewLine) (build-board (cdr lines-list) next-column-list)))
        )
    )
)

(defun output-results (solution algorithm)
"Outputs the SOLUTION with used ALGORITHM to a file results.txt."
    (let ((board (display-board (caar solution)))
        (runtime (float (* (car (last solution)) 0.000001)))
        (total-nodes (cadr solution))
        (expanded-nodes (caddr solution))
        (penetrance (cadddr solution))
        (branch-factor (cadr (reverse solution)))
        (path-to-sol (get-all-states (car solution)))
        )
        (with-open-file (out "results.txt" :direction :output :if-exists :append :if-does-not-exist :create)
            (format out "Board: ~%~a~%" board)
            (format out "Number of generated nodes: ~a~%" total-nodes)
            (format out "Number of expanded nodes: ~a~%" expanded-nodes)
            (format out "Algorithm used: ~a~%" algorithm)
            (format out "Run time: ~fms~%" runtime)
            (format out "Penetrance: ~f~%" penetrance)
            (format out "Effective branching factor: ~f~%" branch-factor)
            (format out "Path to solution: ~a~%" path-to-sol)
            (format out "--------------------------------------------------------------------------------------~%")
        )
    )
)

(defun print-results (solution algorithm)
"Outputs the SOLUTION with used ALGORITHM to the CLI."
    (let ((board (display-board (caar solution)))
        (runtime (float (* (car (last solution)) 0.000001)))
        (total-nodes (cadr solution))
        (expanded-nodes (caddr solution))
        (penetrance (cadddr solution))
        (branch-factor (cadr (reverse solution)))
        (path-to-sol (get-all-states (car solution)))
        )
        (format t "Board: ~%~a~%" board)
        (format t "Number of generated nodes: ~a~%" total-nodes)
        (format t "Number of expanded nodes: ~a~%" expanded-nodes)
        (format t "Algorithm used: ~a~%" algorithm)
        (format t "Run time: ~fms~%" runtime)
        (format t "Penetrance: ~f~%" penetrance)
        (format t "Effective branching factor: ~f~%" branch-factor)
        (format t "Path to solution: ~a~%" path-to-sol)
        (output-results solution algorithm)
    )
)

(defun main-menu ()
"Main menu function. Displays the welcome message and problem to pick."
    (format t "~%~%~%~%~%~%~%~%~%")
    (format t "Welcome to the Dots and Boxes problem solving project!~%")
    (format t "Developed by: Andre Dias and Joao Caetano~%~%")
    (problem-menu)
)

(defun problem-menu ()
"Problem menu function. Loads problems from problemas.dat and shows it to the user in the CLI. User can pick one problem to solve."
    (let ((problems-list (load-problems "problemas.dat")))
       (format t "Choose the problem to solve: ~%")
       (show-problems problems-list)
       (let ((inp (read)))
            (if (or (not (numberp inp)) (> inp (length problems-list)) (< inp 1))
                (problem-menu)
                (let ((problem (nth (- inp 1) problems-list)))
                    (algorithm-menu problem)
                )
            )
        )
    ) 
)

(defun depth-menu (problem algorithm)
"Depth menu function. Given PROBLEM and ALGORITHM, user can input the max tree depth for dfs."
    (format t "Pick the maximum tree depth (bigger than 0): ")
    (let ((inp (read)))
        (if (or (not (numberp inp)) (< inp 0))
            (depth-menu problem algorithm)
            (run-search problem algorithm inp)
        )
    )
)

(defun heuristic-menu (problem algorithm)
"Heuristic menu function. Given PROBLEM and ALGORITHM, user can which heuristic to use for a*"
    (format t "Pick a search heuristic:~%1.Default o(x) - c(x)~%2.Project (o(x)-c(x)/a(x))~%~%o(x) = Closed box objective~%c(x) = Closed boxes~%a(x) = Sum of types of adjacent arcs~%")
    (let ((inp (read)))
        (if (or (not (numberp inp)) (> inp 2) (< inp 1))
            (heuristic-menu problem algorithm)
            (ecase inp
                (1 (run-search problem algorithm 0 'h1))
                (2 (run-search problem algorithm 0 'h2))
            )
        )    
    )
)

(defun algorithm-menu (problem)
"Algorithm menu function. Given PROBLEM, user can select which algorithm to search with."
    (format t "Choose the searching algorithm:~%~%1.BFS~%2.DFS~%3.A*~%")
    (let ((inp (read)))
        (if (or (not (numberp inp)) (> inp 3) (< inp 1))
            (algorithm-menu problem)
            (ecase inp
                (1 (run-search problem 'bfs))
                (2 (depth-menu problem 'dfs))
                (3 (heuristic-menu problem 'a*))
            )
        )    
    )
)

(defun run-search (problem algorithm &optional depth h-func)
"Runs the search on a given PROBLEM, ALGORITHM, DEPTH (for dfs) and H-FUNC (for a*)."
    (format t "~%Calculating...~%")
    (ecase algorithm
        ('bfs (print-results (funcall algorithm (list (create-node (car problem))) nil (cadr problem)) algorithm))
        ('dfs (print-results (funcall algorithm (list (create-node (car problem))) nil (cadr problem) depth) algorithm))
        ('a* (print-results (funcall algorithm (list (create-node (car problem))) nil (cadr problem) h-func) algorithm))
    )
)

(defun show-problems (problems &optional (i 1))
"Prints all available PROBLEMS to the cli."
    (cond ((null problems) (format t ""))
          (t (let ((problem (car problems)))
            (format t "~a. Board: ~%~a~%" i (display-board (car problem)))
            (show-problems (cdr problems) (1+ i))))
    )
)

(defun load-lisp-files ()
"Loads and compiles all of the program's files."
    (load (compile-file "puzzle.lisp"))
    (load (compile-file "procura.lisp"))
    (load (compile-file "projeto.lisp"))
)

(defun start ()
"Starts the program."
    (main-menu)
)
