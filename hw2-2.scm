;GRADE
;1
(define times10 
(lambda [nums]
  (map (lambda [x] (* x 10)) nums)))

;GRADE
;2
(define (pair-up elt ls)
(map (lambda (x) (cons elt x)) ls))

;GRADE
;3: Use dual recursion for odd and even function - from week02_examples.scm
(define even?
(lambda [x]
  (cond
   [(eq? x 0) #t]
   [else (odd? (- x 1))])))

(define odd?
(lambda [x]
  (cond
   [(eq? x 0) #f]
   [else (even? (- x 1))])))

(define (x-odds nums)
(map (lambda [e] (if (odd? e) "x" e)) nums))

;GRADE
;4
(define (replace old new syms)
(map (lambda [x]
(if (equal? old x) new x)) syms))


;GRADE
;5
(define (remove elt ls)
(filter (lambda (n) (not (equal? n elt))) ls))

;GRADE
;7
(define length
(lambda [ls]
  (fold-left (lambda [count top] (+ count 1)) 0 ls)))

;GRADE
;8
(define (average nums)
(/ (fold-right (lambda (x y) (+ x y)) 0 nums) 
   (length nums)))


;10
(define (reverse ls)
(fold-right 
(lambda (e acc) 
(append acc (list e))) ls '())) 




