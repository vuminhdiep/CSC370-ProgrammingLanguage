;1
(define subtract
(lambda (a b)
  (if (is-Zero? a)
    (zero)
    (if (is-Zero? b)
        a
        (subtract (predecessor a) (predecessor b))))))

(define divide
(lambda (a b)
(if (less-than? a b) (zero) (sum (successor (zero)) (divide (subtract a b) b)))
))

(define remainder
(lambda (a b)
(if (less-than? a b) a (remainder (subtract a b) b))

;2
(define binary->number
(lambda (n)))
;3
(define equals?
(lambda (a b)
(cond [(is-Zero? a) (is-Zero? b)] 
      [(is-Zero? b) (is-Zero? a)] 
      [else (equals? (predecessor a) (predecessor b))])
))
(define less-than?
(lambda (a b)
(if (is-Zero? b) #f (if (is-Zero? a) #t (less-than? (predecessor a) (predecessor b))))
))
(define sum
(lambda (a b)
  (if (is-Zero? a) 
    b
    (sum (predecessor a) (successor b)))))

(define prod
(lambda (a b)
    (if (is-Zero? a) 
        (zero)
        (sum b (prod (predecessor a) b)))))