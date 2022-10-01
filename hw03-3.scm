(define true
(lambda (a)
  (lambda (b)
    a)))

(define false
(lambda (a)
  (lambda (b)
    b)))

(define if-ba 
(lambda (test exp1 exp2)
  ((test exp1) exp2)))

;1
;Give the opposite ba-boolean value: not true => false, not false => true
(define not-ba
(lambda (a)
  ((a false) true)))

;Combine the two ba-boolean values
(define and-ba
(lambda (a b)
  ((a b) false)))

;I tried all 4 test cases: (a: true, b: true), (a: true, b: false), (a: false b:true), (a: false, b: false)
; but fail the case when (a: false b:true) and when (a: false, b: false)
(define xor-ba
(lambda (a b)
  (cond [(a #t) (not-ba b)] 
    [else b])))  

;2
;Convert from ba-boolean to scheme boolean
(define ba->boolean
(lambda (a)
  ((a #t) #f)))

;Convert from scheme boolean to ba-boolean
(define boolean->ba
(lambda (b)
  (if b true sfalse)))