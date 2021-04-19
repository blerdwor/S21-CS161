; These are test cases from the spec as well as some that I've written.

(print "(DFS '((A (B)) C (D))")
(print (dfs '((a (b)) c (d))))
(print (equal (dfs '((a (b)) c (d))) '(d c b a)))

(print "(DFS '((a b) (c d))")
(print (dfs '((a b) (c d))))
(print (equal (dfs '((a b) (c d))) '(d c b a)))

(print "(DFS '((a (b)) c (d ((g h) f)))")
(print (dfs '((a (b)) c (d ((g h) f)))))
(print (equal (dfs '((a (b)) c (d ((g h) f)))) '(f h g d c b a)))

(print "(DFS '(a (b c) (d) (e (f g))))")
(print (dfs '(A (B C) (D) (E (F G)))))
(print (equal (dfs '(a (b c) (d) (e (f g)))) '(g f e d c b a)))

(print "(BFS '((A (B)) C (D))")
(print (bfs '((a (b)) c (d))))
(print (equal (bfs '((a (b)) c (d))) '(c a d b)))

(print "(BFS '((a b) (c d))")
(print (bfs '((a b) (c d))))
(print (equal (bfs '((a b) (c d))) '(a b c d)))

(print "(BFS '((a (b)) c (d ((g h) f)))")
(print (bfs '((a (b)) c (d ((g h) f)))))
(print (equal (bfs '((a (b)) c (d ((g h) f)))) '(c a d b f g h)))

(print "(BFS '((a) (b c d) (e) (f g))")
(print (bfs '((a) (b c d) (e) (f g))))
(print (equal (bfs '((a) (b c d) (e) (f g))) '(a b c d e f g)))

(print "(LIMIT-DFS '((A (B)) C (D)) 1")
(print (limit-dfs '((a (b)) c (d)) 1))
(print (equal (limit-dfs '((a (b)) c (d)) 1) '(c)))

(print "(LIMIT-DFS '((A (B)) C (D)) 2")
(print (limit-dfs '((a (b)) c (d)) 2))
(print (equal (limit-dfs '((a (b)) c (d)) 2) '(a c d)))

(print "(LIMIT-DFS '((A (B)) C (D)) 3")
(print (limit-dfs '((a (b)) c (d)) 3))
(print (equal (limit-dfs '((a (b)) c (d)) 3) '(a b c d)))

(print "(LIMIT-DFS '((a b) (c d)) 1")
(print (limit-dfs '((a b) (c d)) 1))
(print (equal (limit-dfs '((a b) (c d)) 1) NIL))

(print "(LIMIT-DFS '((a b) (c d)) 2")
(print (limit-dfs '((a b) (c d)) 2))
(print (equal (limit-dfs '((a b) (c d)) 2) '(a b c d)))

(print "(LIMIT-DFS '((a (b)) c (d ((g h) f))) 3)")
(print (limit-dfs '((a (b)) c (d ((g h) f))) 3))
(print (equal (limit-dfs '((a (b)) c (d ((g h) f))) 3) '(a b c d f)))

(print "(DFID '((A (B)) C (D)) 3)")
(print (dfid '((A (B)) C (D)) 3))
(print (equal (dfid '((A (B)) C (D)) 3) '(C A C D A B C D)))

(print "(DFID '((A (B)) C (D)) 2)")
(print (dfid '((A (B)) C (D)) 2))
(print (equal (dfid '((A (B)) C (D)) 2) '(C A C D)))

(print "(on-path '(3 3 t) '((3 3 t) (1 1 t) (0 1 NIL)))")
(print (on-path '(3 3 t) '((3 3 t) (1 1 t) (0 1 NIL))))
(print (equal (on-path '(3 3 t) '((3 3 t) (1 1 t) (0 1 NIL))) t))

(print "(on-path '(3 3 t) '((1 1 t) (0 1 NIL) (3 3 t)))")
(print (on-path '(3 3 t) '((1 1 t) (0 1 NIL) (3 3 t))))
(print (equal (on-path '(3 3 t) '((1 1 t) (0 1 NIL) (3 3 t))) t))

(print "(on-path '(3 3 t) '((1 1 t) (0 1 NIL)))")
(print (on-path '(3 3 t) '((1 1 t) (0 1 NIL))))
(print (equal (on-path '(3 3 t) '((1 1 t) (0 1 NIL))) NIL))

(print "(next-state '(3 3 t) 1 0)")
(print (next-state '(3 3 t) 1 0))
(print (equal (next-state '(3 3 t) 1 0) NIL))

(print "(next-state '(3 3 t) 0 1)")
(print (next-state '(3 3 t) 0 1))
(print (equal (next-state '(3 3 t) 0 1) '((0 1 NIL))))

(print "(next-state '(3 3 t) 0 0)")
(print (next-state '(3 3 t) 0 0))
(print (equal (next-state '(3 3 t) 0 0) NIL))

(print "(next-state '(3 3 t) 1 1)")
(print (next-state '(3 3 t) 1 1))
(print (equal (next-state '(3 3 t) 1 1) '((1 1 NIL))))

(print "(next-state '(3 3 t) 2 0)")
(print (next-state '(3 3 t) 2 0))
(print (equal (next-state '(3 3 t) 2 0) NIL))

(print "(next-state '(1 3 t) 2 2)")
(print (next-state '(1 3 t) 2 2))
(print (equal (next-state '(3 3 t) 2 2) NIL))

(print "(succ-fn '(3 3 t))")
(print (succ-fn '(3 3 t)))
(print (equal (succ-fn '(3 3 t)) '((0 1 NIL) (1 1 NIL) (0 2 NIL))))

(print "(succ-fn '(3 3 NIL))")
(print (succ-fn '(3 3 NIL)))
(print (equal (succ-fn '(3 3 NIL)) '((0 1 T) (1 1 T) (0 2 T))))

(print "(succ-fn '(1 1 t))")
(print (succ-fn '(1 1 t)))
(print (equal (succ-fn '(1 1 t)) '((3 2 NIL) (3 3 NIL))))

(print "(succ-fn '(2 2 NIL))")
(print (succ-fn '(2 2 NIL)))
(print (equal (succ-fn '(2 2 NIL)) '((2 2 T) (3 1 T))))

(print "(mc-dfs '(3 3 t) NIL)")
(print (mc-dfs '(3 3 t) NIL))
