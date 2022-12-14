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
;3
(define (x-odds nums)
(map (lambda [e] (if (equal? (modulo e 2) 1) 'x e)) nums))

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




