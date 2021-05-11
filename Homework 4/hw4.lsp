;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta) 
   (if (= n 0)
	()				; if n = 0, return an empty list
	(backtrack-dfs n delta '())))	; otherwise, perform backtracking DFS on the CNF

; input: the total number of variables n, the CNF delta, and a list of variable assignments 
; ouput: NIL if no assignment list can be made or the assignment list if one could be made
(defun backtrack-dfs (n delta assignmnt-lst)
   (cond ((not (eval-cnf assignmnt-lst delta)) nil)							; eval-cnf ONLY returns NIL when a clause is false 
         ((= (length assignmnt-lst) n) assignmnt-lst)							; eval-cnf = t && the assignment list is complete
	 (t (or (backtrack-dfs n delta (cons (+ (length assignmnt-lst) 1) assignmnt-lst))		; add a t value
		(backtrack-dfs n delta (cons (- (+ (length assignmnt-lst) 1)) assignmnt-lst))))))	; add a NIL value

; input: a list assignmnt-lst and a list of lists cnf
; output: NIL if the assignment does not satisfy the cnf
;         t if the assignment satisfies the cnf thus far (assignment list can be incomplete)
(defun eval-cnf (assignmnt-lst cnf)
   (cond ((NULL cnf) t)						; got to the end of the CNF without finding any NIL ones 
	 ((not (eval-clause assignmnt-lst (car cnf))) NIL)	; if a clause is NIL, then the assignment doesn't work 
	 (t (eval-cnf assignmnt-lst (cdr cnf)))))		; keep checking the next clauses in the cnf

; input: a list assignmnt-lst and a list clause
; output: NIL if the clause evaluates to NIL with that assignmnt-lst
;         t if the clause evaluates to t (assignment list can be incomplete)
(defun eval-clause (assignmnt-lst clause)
   (cond ((NULL clause) NIL)				; an empty clause is NIL because no assignment can make it t
	 ((eval-literal assignmnt-lst (car clause)) t)	; if any literal in the clause is t, the clause is good
	 (t (eval-clause assignmnt-lst (cdr clause)))))	; keep checking the next literals in the clause

; input: a list assignmnt-lst and an int literal
; output: t or NIL depending on the value that the literal was assigned 
(defun eval-literal (assignmnt-lst literal) 
   (if (NULL assignmnt-lst)
	t 
   	(let ((assigned-var (car assignmnt-lst)))
   	     (cond ((NULL assignmnt-lst) t)				; literal not assigned, return t (innocent until proven guilty)
	           ((= literal assigned-var) t)				; (t and t) or (NIL and NIL) both evaluate to t
	           ((= literal (- assigned-var)) NIL)			; (t and NIL) or (NIl and t) both evaluate to NIL
		   (t (eval-literal (cdr assignmnt-lst) literal))))))	; keep searching through rest of assignment list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))
