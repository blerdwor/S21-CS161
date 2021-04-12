; CS161 Homework 1
; Belle Lerdworatawee

; input: a number N and an ordered tree TREE
; output: T if N is in TREE, NIL otherwise
; basecase is an empty tree (return NIL) or single atom (return t or NIL if it's equal to N)
; otherwise recurse on the left and right branch of the tree until we get to the atom case
; check every atom in the tree to see if any are equal to N
(defun TREE-CONTAINS (N TREE)
  (cond ((null TREE) NIL)                                   
	((atom TREE) (equal N TREE))
	(t (or (TREE-CONTAINS N (car TREE)) (TREE-CONTAINS N (cdr TREE))))))

; input: an ordered tree TREE
; output: the minimum number in TREE if TREE is not empty, NIL otherwise
; basecase is an empty tree (return NIL) or single atom (return the atom)
; otherwise because the tree is ordered, we just need to get the leftmost atom
; and we recurse on the car of tree until we get an atom
(defun TREE-MIN (TREE)
  (cond ((null TREE) NIL)
	((atom TREE) TREE)
	(t (TREE-MIN (car TREE)))))

; input: an ordered tree TREE
; output: pre-ordered list
; basecase is an empty tree (return NIL) or a single atom (return a list with only that atom)
; or a single element list (return the list)
; otherwise concatenate the root node of the tree with the pre-ordered list from the left and 
; right of the tree
(defun TREE-ORDER (TREE)
  (cond ((null TREE) NIL)
        ((atom TREE) (list TREE))
	((equal (cdr TREE) NIL) TREE)
	(t (cons (cadr TREE) (append (TREE-ORDER (car TREE)) (TREE-ORDER (caddr TREE)))))))

; input: a list L, the starting index START, the length of the sublist LEN
; output: sublist of L starting at START, and has LEN elements
; basecase is an empty list (return NIL)
; otherwise check if start is still greater than 0, in which subtract start
; after start is 0, cons the head of the current lists until len is 0
(defun SUB-LIST (L START LEN)
  (cond ((null L) NIL)
	((> START 0) (SUB-LIST (cdr L) (- START 1) LEN))
	((> LEN 0) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))))

; input: a list L
; output: a list L' containing two lists L1 and L2 such that L' = (append L1 L2) and L1
;         is 1 or 0 elements larger than L2
; basecase is an empty list (return NIL)
; if the list has an even number of elements
;    then SUB-LIST will be called to split the list in half
; if the list has an odd number of elements
;    then SUB-LIST will be called to split the list  
(defun SPLIT-LIST (L)
  (cond ((null L) NIL)
	((evenp (length L)) 
	        (list (SUB-LIST L 0 (/ (length L) 2))
	              (SUB-LIST L (/ (length L) 2) (/ (length L) 2))))
	((oddp (length L)) 
	       (list (SUB-LIST L 0 (ceiling (/ (length L) 2)))
	             (SUB-LIST L (ceiling (/ (length L) 2)) (- (/ (length L) 2) (/ 1 2)))))))

; input: a binary tree TREE
; output: the height of TREE as an integer
; basecase is an empty tree (return NIL) or a single atom (return 0) or 
; a single element list (return 0) 
; otherwise calculate the height of the left sub tree and the right sub tree
; add 1 to the taller of the two
(defun BTREE-HEIGHT (TREE)
  (cond ((null TREE) NIL) 
	((atom TREE) 0)
	((equal (second TREE) NIL) 0)
	(t (let ((left-tree-height (BTREE-HEIGHT (first TREE)))
		 (right-tree-height (BTREE-HEIGHT (second TREE))))
		 (if (> left-tree-height right-tree-height)
		     (+ (BTREE-HEIGHT (first TREE)) 1)
		     (+ (BTREE-HEIGHT (second TREE)) 1))))))

; input: a non-empty list LEAVES
; output: a binary tree
; basecase is an empty list (return NIL) or a single atom (return the atom) or 
; a single element list (return the single element) or if the length of the list is 2 (return that list)
; otherwise LIST2BTREE will return a tree so make the left part of the list into tree nodes and then
; concatenate them 
(defun LIST2BTREE (LEAVES)
  (cond ((null LEAVES) NIL)
	((atom LEAVES) LEAVES)
	((equal (cdr LEAVES) NIL) (car LEAVES))
	((= (length LEAVES) 2) LEAVES)
	(t (list (LIST2BTREE (car (SPLIT-LIST LEAVES))) (LIST2BTREE (cadr (SPLIT-LIST LEAVES)))))))

; input: a binary tree TREE
; output: a list 
; basecase is an empty tree (return NIL) or a single atom (return the atom as a list) or a list
; containing 2 atoms (return the list)
; otherwise form two lists from the head and tails of BTREE2LIST and append them to each other
(defun BTREE2LIST (TREE)
  (cond ((null TREE) NIL)
        ((atom TREE) (list TREE))
	((and (atom (first TREE)) (atom (second TREE)) (equal (cddr TREE) NIL)) TREE)
	(t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cdr TREE))))))

; input: two LISP expressions E1 and E2
; output: t if E1 is identical to E2; NIL otherwise
; basecase is 2 empty expressions (return t) or 1 of them is empty (return NIL)
; or both are atoms (return t if they are equal, NIL otherwise)
; or E1 is an atom but E2 is a list (return NIL) or vice versa (return NIL)
; otherwise recursively compare the heads of each expression and the tails  
(defun IS-SAME (E1 E2)
  (cond ((and (null E1) (null E2)) t)
        ((or (null E1) (null E2)) NIL)
	((and (atom E1) (atom E2)) (= E1 E2)) 
	((atom E1) NIL)
	((atom E2) NIL)
      	(t (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))))
