;
; CS161 Spring 2020 HW6 Problem 3: Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw6.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
   (+ c (* k (- n 1))))

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
; Clause example: (1 2 3) for n = 1, c = 1, k = 3
; This says something like "node 1 receives color 1 or node 1
; receives color 2 or node 1 receives color 3" which fits the
; above constraint
;
(defun at-least-one-color (n c k)
  (cond ((> c k) ())
	((= c k) (list (node2var n c k)))
        (t (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))))

; HELPER FOR at-most-one-color (n c k)
; returns *a list of clauses* for the constraint:
; "node n does not get color o or color c from the set {c,c+1,...,k}."
;
(defun expand (n o c k)
  (cond ((> c k) ())
	((= c k) ())
	(t (cons (list (- (node2var n o k)) (-(node2var n (+ c 1) k))) 
		 (expand n o (+ c 1) k)))))

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
; Clause example: ((-1 -2) (-1 -3) (-2 -3)) for n = 1, c = 1, k = 3
; This says something like "node 1 does not receive color 1 or color 2
; and node 1 does not receive color 1 or 3 and node 1 does not receive
; color 2 oe 3" which matches the above constraint. I first formulated
; the problem as a DNF: "node 1 receives color 1 and does not receive
; color 2 and not color 3 or node 1 receives color 2 and ..."
; To convert it to CNF, I changed the above sentence to "NOT node 1
; receives more than 1 color"
; (~a ^ b) v (a ^ ~b) = (~a v ~b)
;
(defun at-most-one-color (n c k)
  (cond ((> c k) ())
	((= c k) ())
	(t (append (expand n c c k) (at-most-one-color n (+ c 1) k)))))

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
; Clause example: ((1 2 3) (-1 -2) (-1 -3) (-2 -3)) for n = 1, k = 3
; In order for a node n to get exactly one color, we can 
; restrict it to have at least one color and at most 1 color.
;
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k)))

; HELPER FOR generate-edge-clauses (e k)
; returns *a list of clauses* to ensure that 
; "the nodes x and y cannot have the same color from the set {c,c+1,...,k}." 
;
(defun helper (x y c k)
  (cond ((> c k) ())
        ((= c k) (list (list (- (node2var x c k)) (- (node2var y c k)))))
        (t (cons (list (- (node2var x c k)) (- (node2var y c k))) (helper x y (+ c 1) k))))) 

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
; Clause example: ((-1 -4) (-2 -5) (-3 -6)) for e = (1 2), k = 3
; We can apply the same rule that we used for at-most-one-color since the
; only time that two nodes will have the same color is when (~a v ~b) is
; false. This means that the conditions node X does not receive color Y or 
; node Z does not receive color Y are both false; so both nodes are color Y.
; (~a ^ b) v (a ^ ~b) = (~a v ~b) 
;
(defun generate-edge-clauses (e k)
  (let ((x (car e))
        (y (cadr e)))
       (helper x y 1 k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
