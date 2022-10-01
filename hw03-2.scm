;1
(define zero
(lambda () '(0)))

(define is-Zero?
(lambda (n)
  (equal? n (zero))))

;Call the helper function on the reverse of n to get the representation we want
(define successor 
(lambda (n)
  (reverse (successor* (reverse n)))))

;Helper function to recursive on finding the successor of n 
;by putting 1 in front of the rest of n if the first digit is 0, 
;otherwise recusively add 0 with the rest of n
(define successor*
(lambda (n)
  (cond 
   [(null? n) (list 1)]
   [(= (car n) 0) (cons 1 (cdr n))]
   [else (cons 0 (successor* (cdr n)))])))

;Call the helper function on the reverse of reverse of n to get the representation we want
(define predecessor
(lambda (n)
  (cond 
    [(is-Zero? n) (zero)]
    [(equal? n (one)) (zero)]
    [else (reverse (predecessor* (reverse n)))])))

;Helper function to find the predecessor of a number by adding 0 to the beginning if the first digit is 1, otherwise recursively add the digit after 1
(define predecessor*
  (lambda (n) 
    (cond
     [(= (car n) 1) (if (null? (cdr n)) '() 
	                    (cons 0 (cdr n)))] 
     [else (cons 1 (predecessor* (cdr n)))])))


;2
;Convert binary to number by starting from the left, recursively double the previous total and add the current digit 
(define binary->number
(lambda (n)
  (fold-left (lambda (acc top) (+ (* 2 acc) top)) 0 n)))

;representation of number 1
(define one
(lambda () (successor (zero)))
)

;Function convert number to binary by calling the helper and reverse the remainders to form binary
(define number->binary
(lambda (n)
  (reverse (number->binary* n))))

;Helper function to call recursive get the remainder of modulo number and 2
(define number->binary*
(lambda (n)
  (cond
   [(= n 0) (zero)]
   [(= n 1) (one)]
   [else (let ([a (modulo n 2)])
     (cons a (number->binary* (/ (- n a) 2))))]
)))


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