;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

; input: a list (one row of state)  
; output: t (there are no keepers or boxes) or NIL otherwise
; helper function for goal-test that checks if this row
; contains a keeper or boxes
;
(defun check-goal-row (r)
   (cond ((NULL r) t)
         (t (if (or (isBox (car r)) (isKeeper (car r)))
                NIL
                (check-goal-row (cdr r))))))

(defun goal-test (s) 
   (cond ((NULL s) t)
	 (t (if (NULL (check-goal-row (car s)))
	        NIL
		(goal-test (cdr s))))))

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 

; input: a state s, a row number r, and a column number c
; output: return the integer content of state s at square (r,c) or the value of a wall if the square is 
; outside the scope of the problem
; helper function that will get the integer content as a square
;
(defun get-square (s r c)
   (cond ((NULL s) wall) 
	 ((atom s) s)
	 ((or (< (- (length s) 1) r) (< (- (length (car s)) c) 0)) wall) ; r or c are greater than s' rows and columns
	 ((or (< r 0) (< c 0)) wall) 					 ; r or c is negative
  	 ((> r 0) (get-square (cdr s) (- r 1) c))
         ((> c 0) (get-square (cons (cdar s) (cdr s)) r (- c 1)))
	 (t (get-square (caar s) r c)))) 				 ; default value is a wall

; input: a state s, a row number r, a column number c, a value v
; output: return a new state with the square at (r, c) changed to value v
; function does not change the current state, will return a new state with the modification
;
(defun set-square (s r c v)
  (cond ((> r 0) (cons (car s) (set-square (cdr s) (- r 1) c v)))
	((= r 0) (cons (set-square (car s) (- r 1) c v) (cdr s)))
        ((> c 0) (cons (car s) (set-square (cdr s) r (- c 1) v)))
	((= c 0) (cons v (cdr s)))
	(t NIL)))

; input: the specific update we want, a state s, coordinates, and the current keeper character
; r = keeper's row
; rr = row next to keeper
; rrr = row 2 spots away from keeper
; same concept for c, cc, ccc but with columns
; output: return the updated grid if possible; direction is dictated by pre-calculated coordinates
; 
; all the possible updates are hard-coded and identified by a specific number from 1-6
; the function updates the board accordingly using set-square
;
(defun update-grid (update s r c rr cc rrr ccc keeper-char)
   (let ((end-char (if (isKeeper keeper-char) blank star))) ; determines if the square that the keeper left should be blank or star
        (cond ((= update 1) (set-square (set-square s rr cc keeper) r c end-char))
      	      ((= update 2) (set-square (set-square s rr cc keeperstar) r c end-char))
              ((= update 3) (set-square (set-square (set-square s rrr ccc box) rr cc keeper) r c end-char))
              ((= update 4) (set-square (set-square (set-square s rrr ccc boxstar) rr cc keeper) r c end-char))
              ((= update 5) (set-square (set-square (set-square s rrr ccc box) rr cc keeperstar) r c end-char))
              ((= update 6) (set-square (set-square (set-square s rrr ccc boxstar) rr cc keeperstar) r c end-char)))))

; input: a state s and coordinates
; r = keeper's row
; rr = row next to keeper
; rrr = row 2 spots away from keeper
; same concept for c, cc, ccc but with columns
; output: return the next state if possible; direction is dictated by pre-calculated coordinates 
; 
; all the possible outcomes are hard-coded to call update-grid with a specific number from 1-6
; all other possible outcomes are invalid and return NIL
; first the function gets the keeper's character and the object next and 2 spaces away from the keeper
; based on those 3 pieces of information, it then determines what the next move should be
;
(defun try-move (r c s rr cc rrr ccc)
  (let ((keeper-char (get-square s r c))
	(dir-sqr (get-square s rr cc))
	(dir-dir-sqr (get-square s rrr ccc)))  									; keeper will either be @ or +
       (cond ((isBlank dir-sqr) (update-grid 1 s r c rr cc rrr ccc keeper-char))				; keeper moves to a blank
	     ((isStar dir-sqr) (update-grid 2 s r c rr cc rrr ccc keeper-char))					; keeper moves to a goal
	     ((and (isBox dir-sqr) (isBlank dir-dir-sqr)) (update-grid 3 s r c rr cc rrr ccc keeper-char))	; keeper moves box to blank
	     ((and (isBox dir-sqr) (isStar dir-dir-sqr)) (update-grid 4 s r c rr cc rrr ccc keeper-char))	; keeper moves box to goal
	     ((and (isBoxStar dir-sqr) (isBlank dir-dir-sqr)) (update-grid 5 s r c rr cc rrr ccc keeper-char))	; keeper moves box on goal to blank
	     ((and (isBoxStar dir-sqr) (isStar dir-dir-sqr)) (update-grid 6 s r c rr cc rrr ccc keeper-char))	; keeper moves box on goal to goal
	     (t NIL)))) 											; all other cases

; input: a state s
; output: a set of states that are the possible outcomes
;
; next-states will call try-move 4 times with pre-calculated inputs to determine
; the 4 directions instead of passing in a direction as a parameter
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (c (car pos))
	 (r (cadr pos))
	 ;r and c are now the coordinate of the keeper in s.
	 (result (list (try-move r c s (- r 1) c (- r 2) c)	; UP
		       (try-move r c s r (+ c 1) r (+ c 2))	; RIGHT
		       (try-move r c s (+ r 1) c (+ r 2) c)	; DOWN
		       (try-move r c s r (- c 1) r (- c 2))))	; LEFT
	 )
    (cleanUpList result)))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s) 0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
 helper function adds up the number of misplaced
; boxes in that row by recursing through the row and return 
; the count
(defun check-row-box (r)
  (cond ((NULL r) 0)
	(t (if (isBox (car r))
	       (+ 1 (check-row-box (cdr r))) 
	       (check-row-box (cdr r))))))

; function feeds a row of the state at a time to the check-row-box
; which will count how many boxes are misplaces in that row
;
; Admissible means that the heuristic doesn't overestimate the true cost, 
; and only returns 0 for the goal state. This heuristic is admissible 
; because the number of boxes left to push is always less than the 
; remaining number of moves necessary to reach the final goal state.
;
(defun h1 (s)
  (cond ((NULL s) 0)
	(t (+ (check-row-box (car s)) (h1 (cdr s))))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; input: a state s, the object that we want to find, the start row r and start column c
; output: a list of all the coordinates of found target in the form (r c)
; the function chips away at the state until it is an atom at which it will check
; if it is a target
; if YES: then return the coordinates as a list of lists ((r c))
; if NO: keep chipping away at s
;
(defun get-coords (s target r c)
  (cond ((NULL s) ())
	((atom s) (if (= s target) 
		      (list (list r (- c 1)))
		      ()))
	(t (if (= c 0)
	       (append (get-coords (car s) target r (+ c 1)) (get-coords (cdr s) target (+ r 1) c))
	       (append (get-coords (car s) target r c) (get-coords (cdr s) target r (+ c 1)))))))

; input: a state s, the object that we want to locate target, the start coords (r c)
; output: the shortest distance between the start (r c) to a target
; perform DFS from the start coords to find distances to targets
; take the minimum of those targets
;
(defun DFS-1 (s target r c) 
   (let ((curr-sqr (get-square s r c))
	 (up-sqr (get-square s (- r 1) c))
	 (ri-sqr (get-square s r (+ c 1)))
	 (do-sqr (get-square s (+ r 1) c))
	 (le-sqr (get-square s r (- c 1)))
	 (marked-s (set-square s r c 7))
	)
   	(cond ((= curr-sqr target) 0)
	      ((= curr-sqr 7) 0)
	      (t (min (if (or (isBlank up-sqr) (= up-sqr target)) (+ 1 (DFS-1 marked-s target (- r 1) c)) 100)
		      (if (or (isBlank ri-sqr) (= ri-sqr target)) (+ 1 (DFS-1 marked-s target r (+ c 1))) 100)
		      (if (or (isBlank do-sqr) (= do-sqr target)) (+ 1 (DFS-1 marked-s target (+ r 1) c)) 100)
                      (if (or (isBlank le-sqr) (= le-sqr target)) (+ 1 (DFS-1 marked-s target r (- c 1))) 100))))))

; input: a state s
; sr = start row
; sc = end column
; er = end row
; ec = end column
; output: the shortest distance between the start coords and the end coords
; perform DFS from the start coords to the end coords and return the minimum distance found
;
(defun DFS-2 (s sr sc er ec)
   (let ((curr-sqr (get-square s sr sc))
         (up-sqr (get-square s (- sr 1) sc))
         (ri-sqr (get-square s sr (+ sc 1)))
         (do-sqr (get-square s (+ sr 1) sc))
         (le-sqr (get-square s sr (- sc 1)))
         (marked-s (set-square s sr sc 7))
        )
        (cond ((and (= sr er) (= sc ec)) 0)
              ((= curr-sqr 7) 0)
              (t (min (if (or (isBlank up-sqr) (and (= (- sr 1) er) (= sc ec))) (+ 1 (DFS-2 marked-s (- sr 1) sc er ec)) 100)
                      (if (or (isBlank ri-sqr) (and (= sr er) (= (+ sc 1) ec))) (+ 1 (DFS-2 marked-s sr (+ sc 1) er ec)) 100)
                      (if (or (isBlank do-sqr) (and (= (+ sr 1) er) (= sc ec))) (+ 1 (DFS-2 marked-s (+ sr 1) sc er ec)) 100)
                      (if (or (isBlank le-sqr) (and (= sr er) (= (- sc 1) ec))) (+ 1 (DFS-2 marked-s sr (- sc 1) er ec)) 100))))))

; input: a state s, the row and column of the keeper kr and kc, a list of all box coords bcs 
; output: the minimum distance from a box to a goal
; helper function to get the minimum distance from keeper to a box to any goal
;
(defun min-dist (s kr kc bcs)
  (if (NULL bcs)
      100
      (min (+ (DFS-2 s kr kc (caar bcs) (cadar bcs))	 ; keeper -> box
	      (DFS-1 s star (caar bcs) (cadar bcs)) 	 ; same box -> goal
	   )  
	   (min-dist s kr kc (cdr bcs)))))

; input: a list of a pair of coords bc in the form of (r c)
; output: t (the box is in a corner) or NIL (box is not in a corner)
; helper function that checks if a box is in a corner
;
(defun check-for-walls (s bc)
  (let* ((rr (car bc))
	 (cc (cadr bc))
	 (u (get-square s (- rr 1) cc))
	 (r (get-square s rr (+ cc 1)))
	 (d (get-square s (+ rr 1) cc))
	 (l (get-square s rr (- cc 1)))
        )
        (cond ((and (isWall u) (isWall r)) t)
	      ((and (isWall r) (isWall d)) t)
	      ((and (isWall d) (isWall l)) t)
	      ((and (isWall l) (isWall u)) t)
	      (t NIL))))

; input: a state s and a list of box coords bcs
; output: t (at least one of the boxes is in a corner) or NIL (no boxes are in a corner)
; function that checks if any of the boxes are in a corner by passing a box coordinate
; one at a time to check-for-walls
;
(defun box-in-corner (s bcs)
  (cond ((NULL bcs) NIL)
  	(t (if (check-for-walls s (car bcs)) t (box-in-corner s (cdr bcs))))))

; get coordinates of all boxes and goals
; for each box, check if it is in a corner
; if YES: return large constant
; if NO: return shortest distance from keeper -> a box -> a goal 
;
(defun h105375663 (s) 
  (let ((bcs (get-coords s box 0 0))	; bcs = box-coords
	 (pos (getKeeperPosition s 0))
        )	
        (if (box-in-corner s bcs)
	    300 
	    (h1 s))))
;min-dist s (car pos) (cadr pos) bcs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
