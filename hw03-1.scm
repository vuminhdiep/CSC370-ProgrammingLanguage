(define lambda?    ; SType -> Boolean
(lambda [exp]
  (and (list? exp)                 
   (equal? (length exp) 3)
   (equal? (car exp) 'lambda)
   (list? (cadr exp))
   (equal? (length (cadr exp)) 1)
   (symbol? (caadr exp)))))

(define var?      ; SType -> Boolean
(lambda [exp]
  (symbol? exp)))

(define apply?    ; SType -> Boolean
(lambda [exp]
  (and (list? exp)
   (equal? (length exp) 2))))

;1: I get the answer in a reverse order of a list compared to the examples
;Call helper function with the expression on an empty list
(define get-lvars
(lambda (exp)
(get-lvars* exp '())))

;helper function to call recursive and add on a list
(define get-lvars*
(lambda (exp lst)
  (cond [(var? exp) (cons exp lst)]
        [(lambda? exp) (get-lvars* (caddr exp) lst)]
        [(apply? exp) (get-lvars* (cadr exp) (get-lvars* (car exp) lst))]
        [else (eopl:error 'get-lvars "Concrete get variables error with expression ~s" exp)]
  )
))

;2 Call helper function with the expression on an empty list
(define get-lparams
(lambda (exp)
  (get-lparams* exp '())))

;helper function to call recursive and add on a list
(define get-lparams*
(lambda (exp lst)
  (cond 
   [(var? exp) lst]
   [(lambda? exp) (get-lparams* (caddr exp) (cons (caadr exp) lst))]
   [(apply? exp) (get-lparams* (cadr exp) (get-lparams* (car exp) lst))]
   [else (eopl:error 'get-lparams "Concrete get parameters error with expression ~s" exp)]
  )))

;3 Call helper function with expression on number 0
(define replace-vars
(lambda (exp)
  (replace-vars* exp 0)
))

;helper function to call recursive on replace variable with an accumulator
(define replace-vars*
(lambda (exp acc)
(cond
  [(var? exp) acc]
  [(lambda? exp) (list 'lambda (cadr exp) (replace-vars* (caddr exp) (+ acc 1)))]
  [(apply? exp) (list (replace-vars* (car exp) acc) (replace-vars* (cadr exp) acc))]
  [else (eopl:error 'replace-vars "Concrete replace variables error with expression ~s" exp)]

)))

;4
(define free-vars
(lambda (exp)
  (free exp '())))

;Helper function to call recursive on free expression
(define free
(lambda (exp formals-seen)
  (cond
   [(var? exp) (if (is-member? exp formals-seen) '() 
                    (list exp))]
   [(lambda? exp) (free (caddr exp) (cons (caadr exp) formals-seen))]
   [(apply? exp) (append (free (car exp) formals-seen) (free (cadr exp) formals-seen))]
   [else (eopl:error 'free "Concrete free error with expression ~s" exp)]
  
  )))

;Helper function to check if an element is a member of a list
(define is-member?
(lambda (x lst)
  (cond
   [(null? lst) #f]
   [(equal? x (car lst)) #t]
   [else (is-member? x (cdr lst))])))

