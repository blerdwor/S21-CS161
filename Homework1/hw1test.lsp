; These are test cases from the spec as well as some that I've written.

(print "(TREE-CONTAINS 3 '((1 2 3) 7 8))")
(print (equal (TREE-CONTAINS 3 '((1 2 3) 7 8)) t))

(print "(TREE-CONTAINS 4 '((1 2 3) 7 8))")
(print (equal (TREE-CONTAINS 4 '((1 2 3) 7 8)) NIL))

(print "(TREE-CONTAINS 3 '((1 (2 (3))) 7 8))")
(print (equal (TREE-CONTAINS 3 '((1 (2 (3) ) ) 7 8)) t))

(print "(TREE-CONTAINS 9 '((1 (2 (3))) 7 8))")
(print (equal (TREE-CONTAINS 9 '((1 (2 (3) ) ) 7 8)) NIL))

(print "(TREE-MIN '((1 2 3) 7 8))")
(print (equal (TREE-MIN '((1 2 3) 7 8)) 1))

(print "(TREE-MIN '(((1 2 3) 4 (5 6 7)) 7 8))")
(print (equal (TREE-MIN '(((1 2 3) 4 (5 6 7)) 7 8)) 1))

(print "(TREE-ORDER 3)")
(print (equal (TREE-ORDER 3) '(3)))

(print "(TREE-ORDER '((1 2 3) 7 8))")
(print (equal (TREE-ORDER '((1 2 3) 7 8)) '(7 2 1 3 8)))

(print "(TREE-ORDER '(((1 2 3) 7 (8 9 10)) 11 12)")
(print (equal (TREE-ORDER '(((1 2 3) 7 (8 9 10)) 11 12)) '(11 7 2 1 3 9 8 10 12)))

(print "(SUB-LIST '(a b c d) 0 3)")
(print (equal (SUB-LIST '(a b c d) 0 3) '(a b c)))

(print "(SUB-LIST '(a b c d) 3 1)")
(print (equal (SUB-LIST '(a b c d) 3 1) '(d)))

(print "(SUB-LIST '(a b c d) 2 0)")
(print (equal (SUB-LIST '(a b c d) 2 0) NIL))

(print "(SUB-LIST '(a (b c) d e f g) 1 2)")
(print (equal (SUB-LIST '(a (b c) d e f g) 1 2) '((b c) d)))

(print "(SPLIT-LIST '(a))")
(print (equal (SPLIT-LIST '(a)) '((a) ())))

(print "(SPLIT-LIST '(a b c d))")
(print (equal (SPLIT-LIST '(a b c d)) '((a b)(c d))))

(print "(SPLIT-LIST '(a b c d e)")
(print (equal (SPLIT-LIST '(a b c d e)) '((a b c)(d e))))

(print "(SPLIT-LIST '(a b c d e f)")
(print (equal (SPLIT-LIST '(a b c d e f)) '((a b c)(d e f))))

(print "(SPLIT-LIST '(a b c d e f g)")
(print (equal (SPLIT-LIST '(a b c d e f g)) '((a b c d)(e f g))))

(print "(BTREE-HEIGHT 1)")
(print (equal (BTREE-HEIGHT 1) 0))

(print "(BTREE-HEIGHT '(1 2))")
(print (equal (BTREE-HEIGHT '(1 2)) 1))

(print "(BTREE-HEIGHT '(1 (2 3)))")
(print (equal (BTREE-HEIGHT '(1 (2 3))) 2))

(print "(BTREE-HEIGHT '((1 2) (3 4)))")
(print (equal (BTREE-HEIGHT '((1 2) (3 4))) 2))

(print "(BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))")
(print (equal (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3))

(print "(BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8))))")
(print (equal (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3))

(print "(BTREE-HEIGHT '((1 (2 ((3 4) 5))) 6)")
(print (equal (BTREE-HEIGHT '((1 (2 ((3 4) 5))) 6)) 5))

(print "(LIST2BTREE '(1))")
(print (equal (LIST2BTREE '(1)) 1))

(print "(LIST2BTREE '(1 2))")
(print (equal (LIST2BTREE '(1 2)) '(1 2)))

(print "(LIST2BTREE '(1 2 3))")
(print (equal (LIST2BTREE '(1 2 3)) '((1 2) 3)))

(print "(LIST2BTREE '(1 2 3 4))")
(print (equal (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4))))

(print "(LIST2BTREE '(1 2 3 4 5 6 7))")
(print (equal (LIST2BTREE '(1 2 3 4 5 6 7)) '(((1 2) (3 4)) ((5 6) 7))))

(print "(LIST2BTREE '(1 2 3 4 5 6 7 8)) ")
(print (equal (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8)))))

(print "(BTREE2LIST 1)")
(print (equal (BTREE2LIST 1) '(1)))

(print "(BTREE2LIST '(1 2))")
(print (equal (BTREE2LIST '(1 2)) '(1 2)))

(print "(BTREE2LIST '((1 2) 3))")
(print (equal (BTREE2LIST '((1 2) 3)) '(1 2 3)))

(print "(BTREE2LIST '((1 2) (3 4)))")
(print (equal (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4)))

(print "(BTREE2LIST '(((1 2) (3 4)) ((5 6) 7)))")
(print (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) '(1 2 3 4 5 6 7)))

(print "(BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))")
(print (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8)))

(print "(BTREE2LIST '((1 (2 ((3 4) 5))) 6)")
(print (equal (BTREE2LIST '((1 (2 ((3 4) 5))) 6)) '(1 2 3 4 5 6)))

(print "(IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) ")
(print (equal (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) t))

(print "(IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8))")
(print (equal (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) NIL))

(print "(IS-SAME '((1) 2 (3)) '((1) 2 (3))")
(print (equal (IS-SAME '((1) 2 (3)) '((1) 2 (3))) t))

(print "(IS-SAME '(() ()) '(() ())")
(print (equal (IS-SAME '(() ()) '(() ())) t))

