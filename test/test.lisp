(progn
    (load "../src/match.lisp")
    (load "../src/simplify.lisp")
    (trace simplify)
    (trace simplify-term)
    (trace match)
    (trace find-matching-reduction)
    (trace apply-reduction)
    (setq r
        '(
            ((+ x (- x)) 0)
            ((+ (- x) x) 0)
            ((+ x 0) x)
            ((+ 0 x) x)
            ((/ (* x y) x) y)
            ((/ (* y x) x) y)
            ((* x 0) 0)
            ((* 0 x) 0))))

(and
    (equal
        (simplify
            '(+ c (+ (+ a (+ b (- b))) (- a)))
            r)
        'C)
    (equal
        (simplify
            '(+ (- (* a b)) (* a b))
            r)
        '0)
    (equal
        (simplify
            '(+ (* (+ (- a) a) (- b)) (/ (* a (+ b c)) (+ b c)))
            r)
        'A))

