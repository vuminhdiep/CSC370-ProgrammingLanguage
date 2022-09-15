;1.
42
;2.
3.14159
;3.
"hello world"
;4.
'("Let" "the" "Grecian" "dream" "of" "his" "sacred" "stream")
;5.
(+ 3 4)
;6
(* (+ (* 4 (/ 5 9)) 17) (- 6.7 13))
;7
(+ 0 1 2 3 4 5 6 7)
;8: I have to use global variable to use in exercise 12 below. For local variable x it should be (let [x "hello world"] x)
(define x "hello world")
;9
(list? (* (+ (* 4 (/ 5 9)) 17) (- (* 6 7) 13)))
;10
(lambda [x] x)
;11
(define identity (lambda [e] e))
;12
(identity x)
;13
(let [[a 2] [b 7] [c 18]] (/ (+ (- b)(sqrt(- (expt b 2) (* 4 a c)))) (* 2 a)))
;14
(define plus42 (lambda [e] (if (number? e) (+ e 42) "the answer to...")))
;15
(car (list 1 1 2 3 5))
;16
(car (cdddr '(1 1 2 3 5)))
;17
(cons 1 '(1 2 3 5))
;18
(cons 3 4)
;19
'((cons '((cons 1 2) 3 4) 5))
;20
(and (or #t #f) #t)
;21
(define (xor a b) (not (boolean=? a b)))
(let [[a #t][b #f][c #f]] (or (xor a (not b)) (and c (not a)) b))
;22
(if (string? x) 42 "no")
;23
(define (positive? e) (if (> e 0) #t #f))
;24
(define numMonth->strMonth (lambda [month] (cond [(= month 1) "January"] [(= month 2) "February"] [(= month 3) "March"] [(= month 4) "April"] [(= month 5) "May"] [(
= month 6) "June"] [(= month 7) "July"] [(= month 8) "August"] [(= month 9) "September"] [(= month 10) "October"] [(= month 11) "November"] [(= month 12) "December"]
[else "invalid month"])))
;25
(define (list-member? e ls) (cond [(null? ls) #f][(equal? (car ls) e) #t][else (list-member? e (cdr ls))]))
;26:
(define (range num1 num2)
(if (> num1 num2)
    '()
    (cons num1
          (range (+ num1 1) num2))))
;27
(define (list-append ls1 ls2)
(cond [(null? ls1) ls2]
      [(null? ls2) ls1]
      [else (cons (car ls1) (list-append(cdr ls1) ls2))]

)

)
;28: The recursive step I plan to do is: concat(list-flatten(lls[0]), list-flatten(lls[1:]))
(define (list-flatten lls)
(cond [(null? lls) '()]
    [(not (pair? lls)) (list lls)]
      [else (append (list-flatten (car lls))
                    (list-flatten (cdr lls)))]
)
)
;29: The recursive step I plan to do in python is: concat(fn(ls[0]), list-map(fn, ls[1:]))
(define (list-map fn ls)
    (lambda [fn ls]
        (if (null? ls) '()
            (append (fn (car ls)) (list-map(fn (cdr ls))))
    )
    )
)
;30
(define (list-filter p? ls)
(cond [(null? ls) '()]
      [(p? (car ls)) (cons (car ls) (list-filter p? (cdr ls)))]
      [else (list-filter p? (cdr ls))]))
;31: Use list-filter function above and list-length function in class to call in the list-counts function
(define list-length
(lambda [ls]
    (if (null? ls)
    0
    (+ 1 (list-length (cdr ls))))))

(define (list-counts p? ls)
(lambda [p? ls] (list-length (list-filter p? ls)))

)
;32: I have exception error: apply to non-procedure 8. For the recursive step I plan to do in python is: (3n+3)/5*fib2(n) - n/5*fib2(n+1)
(define fib2 (lambda [n] (cond [(< n 0) "invalid input"][(= n 0) '()][(= n 1) '(0)][(= n 2) '(0 1)][else (- (* (/ (+ (* n 3) 3) 5) (fib2(n))) (* (/ n 5) (fib2(+ n 1))))])))
;33

;34

;35

;36

;37

;38

;39
