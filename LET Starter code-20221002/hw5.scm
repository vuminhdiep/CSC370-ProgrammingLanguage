
;;QUICK NOTE: I couldn't figure out the def-prog question (part 2 problem 5) and the printing the environment nicely (part 1 problem 3) 
;;Therefore I put the code I attempted as comments so that the program still runs with the other questions. 
;;Sorry Professor I couldn't entirely fix the bug before the Redfin interview and after 4 straight hour interview I was too tired to think so the code I asked you in the afternoon was still the same.
;;Just a quick rant but by the way I did well in the interview and had really high hopes on that, 
;;Also Salesforce emailed me to schedule a call tomorrow so hopefully I'll get the offer from them too because they asked whether I have accepted the offer with Bank of America or not and when I said I haven't they said they'd call me tomorrow to talk.
;;I think I'll stop interviewing and focus on your class. I just want to update with you my job hunting results.

;; LET-interp-starter.scm
;; CSC 370
;; Fall 2022

(load "helpers.scm")

;; ================ Parser Definitions ==================================

;; This defines the translation from the concrete syntax to the abstract syntax.
;; Whenever  you add to or modify the concrete or abstract syntax the specification
;; below must be updated.

(define the-grammar
  '(
    (program                                              ;; <program> ::= 
     (expression)                                         ;;   Concrete    <expression>
     a-prog)                                              ;;   Abstract    (a-prog exp)
    
    (expression                                           ;; <expression> ::= 
     (number)                                             ;;   Concrete     <number> 
     const-exp)                                           ;;   Abstract     (const-exp num)
                                                          
    (expression                                           ;; <expression> ::= -(<exp>,<exp>)
      ("-(" expression "," expression ")")                ;;                  (diff-exp exp1 exp2)
      diff-exp
    )

    (expression
      ("zero?(" expression ")")                           ;;Concrete
      zero?-exp
    )

    (expression
      ("if" expression "then" expression "else" expression)
      if-exp
    )

    (expression
      (identifier)
      var-exp
    )
    
    (expression
      ("let" identifier "=" expression "in" expression)
      let-exp
    )

    (expression
       ("#true")
       const-true-exp
     )

     (expression
       ("#false")
       const-false-exp
     )

    (expression                                           ;; <expression> ::= +(<exp>,<exp>)
      ("+(" expression "," expression ")")                ;;                  (plus-exp exp1 exp2)
      plus-exp
    )

    (expression                                           ;; <expression> ::= *(<exp>,<exp>)
      ("*(" expression "," expression ")")                ;;                  (times-exp exp1 exp2)
      times-exp
    )

     (expression                                           ;; <expression> ::= /(<exp>,<exp>)
       ("/(" expression "," expression ")")                ;;                  (div-exp exp1 exp2)
       div-exp
     )

    (expression                                           ;; <expression> ::= <(<exp>,<exp>)
       ("<(" expression "," expression ")")                ;;                  (less-than-exp exp1 exp2)
       less-than-exp
     )

    (expression                                           ;; <expression> ::= =(<exp>,<exp>)
        ("=(" expression "," expression ")")                ;;                  (equal-exp exp1 exp2)
        equal-exp
    )

    (expression                                           ;; <expression> ::= <=(<exp>,<exp>)
        ("<=(" expression "," expression ")")             ;;                  (less-than-or-equal-exp exp1 exp2)
        less-than-or-equal-exp
    )

    (expression                                           ;; <expression> ::= and(<exp>,<exp>)
        ("and(" expression "," expression ")")            ;;                  (and-exp exp1 exp2) 
        and-exp
    )

    (expression                                           ;; <expression> ::= or(<exp>,<exp>)
        ("or(" expression "," expression ")")             ;;                  (or-exp exp1 exp2)
        or-exp
    )

    (expression                                           ;; <expression> ::= not(<exp>)
        ("not(" expression ")")                           ;;          (not-exp expression)
        not-exp
    )
    
    (program
        ("def!" identifier "=" expression)
        def-prog
    )
    
    (expression
      ("cons(" expression "," expression ")")
      cons-exp
    )

    (expression
      ("car(" expression ")")
      car-exp
    )
    
    (expression
      ("cdr(" expression ")")
      cdr-exp
    )
    
    (expression
      ("null?(" expression ")")
      null?-exp
    )
    (expression 
      ("#emptylist") 
      emptylist-exp
    )


    
    ))

;; Sets up the parser using the above concrete <-> abstract grammars.
;; Defines a function call parse that takes a string in the concrete
;; syntax and returns the corresponding abstract syntax tree. You must
;; have defined the-grammar first.
(load "lex-scan-parse.scm")

;; ==================== Expressed Values ==================================

(define-datatype expval expval? ;;create a class
  (num-val
   (num number?))
  (bool-val
    (b boolean?))
  (pair-val
  (car expval?)
  (cdr expval?))
  (emptylist-val)
  (unit-val)
  )

(define expval->bool
  (lambda (ev)
    (cases expval ev
      [bool-val (b) b]
      [else (raise-exception 'expval->bool "Expressed value is not a boolean: ~s" ev)])))

(define expval->num
  (lambda (ev)
    (cases expval ev
	   [num-val (num) num]
	   [else (raise-exception 'expval->num "Expressed value is not a number: ~s" ev)])))

(define expval->pair
(lambda (ev)
  (cases expval ev
    [pair-val (car cdr) (cons car cdr)]
    [else (raise-exception 'expval->pair "Expressed value is not a pair: ~s" ev)])))

(define expval->car
(lambda (ev)
  (cases expval ev
    [pair-val (car cdr) car]
    [else (raise-exception 'expval->car "Expressed value is not a car: ~s" ev)])))

(define expval->cdr
  (lambda (ev)
    (cases expval ev
	   [pair-val (car cdr) cdr]
	   [else (raise-exception 'expval->cdr "Expressed value is not a cdr: ~s" ev)])))

(define expval->null?
  (lambda (ev)
    (cases expval ev
	   [emptylist-val () (bool-val #t)]
	   [else (bool-val #f)])))

(define expval->unit
  (lambda (ev)
    (cases expval ev
    [unit-val () "#void"]
    [else (raise-exception 'expval->unit "Expressed value is not a unit value: ~s" ev)])))
  
;;The commented part for num-val and bool-val cases is for question 4 part 2. I don't want to uncomment this because it will affect other questions I tested and it works.
(define expval->string
(lambda (ev)
    (cases expval ev
    [num-val (num) (number->string num)] ;[num-val (num) (if (= num 0) "#false" "#true")] ;update so that non-zero numbers is #true and zero is #false
    [bool-val (b) (if b "#true" "#false")] ;[bool-val (b) (if b 1 0)] ;update so that #true is 1 and #false is 0
    [pair-val (exp1 exp2) (string-append "(" (expval->string exp1) " . " (expval->string exp2) ")")]
    [emptylist-val () "#emptylist"]
    [unit-val () "#void"]
  )))

;; =================== Environment ========================================
(define make-init-env
(lambda ()
    (extend-env
        'pi (num-val 3.14159)
    (extend-env
        'e (num-val 2.71828)
(empty-env)))))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
   (env environment?)))

(define apply-env ; Env x Var -> Expval
  (lambda (env target-var)
    (cases environment env
	   [extend-env (var val env)
		       (if (equal? var target-var)
			   val
			   (apply-env env target-var))]
	   [empty-env () (raise-exception 'apply-env "No binding for ~s" target-var)]
	   )))

(define environ->string
  (lambda (env)
    (cases environment env
      [extend-env (var val env) 
      (string-append (symbol->string var) " = " (expval->string val) "\n" (environ->string env))] ;;sorry I couldn't figure out the formatted like the example with square brackets in the question so I added a new line everytime it prints a variable like this
      [empty-env () ""]
      
    )
)
)


;; ==================== Evaluater =========================================

(define value-of-prog ;;prog return an environment
  (lambda (prog env)
    (cases program prog
      [a-prog (exp1)  (value-of-exp exp1 env)] 
    ;;this a-prog and def-prog are commented because they are my attempt for part 2 problem 5 but I couldn't get it to fixed

	   ;[a-prog (exp1)  (list (value-of-exp exp1 env) env)] 
     ;[def-prog (var exp1) (let [[val (value-of-exp exp1 env)]]
      ;                    (list (unit-val) (extend-env var val env))
    ;)] 
	   [else (raise-exception 'value-of-prog "Abstract syntax case not implemented: ~s" (car prog))])))

(define value-of-exp
  (lambda (exp env)
    (cases expression exp
	   [const-exp [num] (num-val num)]
     [diff-exp [exp1 exp2] 
        (let [
              [ev1 (value-of-exp exp1 env)]
              [ev2 (value-of-exp exp2 env)]]
      (num-val (- (expval->num ev1) (expval->num ev2))))]
     [zero?-exp [expression]
        (let [[ev (value-of-exp expression env)]]
        (bool-val (= (expval->num ev) 0)))]
      [if-exp [exp1 exp2 exp3]
        (let [[ev (value-of-exp exp1 env)]]
          (if (expval->bool ev)
            (value-of-exp exp2 env)
            (value-of-exp exp3 env)))]
      [var-exp [var]
        (apply-env env var)] 
      [let-exp [var expression body]
        (let [[ev (value-of-exp expression env)]]
          (value-of-exp body (extend-env var ev env)))] 
      [const-true-exp () (bool-val #t)] 
      [const-false-exp () (bool-val #f)] 
      [plus-exp [exp1 exp2] 
        (let [
              [ev1 (value-of-exp exp1 env)]
              [ev2 (value-of-exp exp2 env)]]
      (num-val (+ (expval->num ev1) (expval->num ev2))))]
      [times-exp [exp1 exp2] 
        (let [
              [ev1 (value-of-exp exp1 env)]
              [ev2 (value-of-exp exp2 env)]]
      (num-val (* (expval->num ev1) (expval->num ev2))))]
      [div-exp [exp1 exp2]  
        (let [
              [ev1 (value-of-exp exp1 env)]
              [ev2 (value-of-exp exp2 env)]
              
            ]
        (if (= (expval->num ev2) 0) (raise-exception 'div-exp "Cannot divide a 0 expression in the denominator ~s" (car exp)) 
              (num-val (/ (expval->num ev1) (expval->num ev2))))
            
      )]
      [less-than-exp [exp1 exp2]
        (let [
          [ev1 (value-of-exp exp1 env)]
          [ev2 (value-of-exp exp2 env)]]
      (bool-val (< (expval->num ev1) (expval->num ev2)))
      )
      ]

      [equal-exp [exp1 exp2]
      (let [
              [ev1 (value-of-exp exp1 env)]
              [ev2 (value-of-exp exp2 env)]]
      (bool-val (= (expval->num ev1) (expval->num ev2)))
      )
      ]

      [less-than-or-equal-exp [exp1 exp2]
      (let [
              [ev1 (value-of-exp exp1 env)]
              [ev2 (value-of-exp exp2 env)]]
      (bool-val (<= (expval->num ev1) (expval->num ev2)))
      )
      ]
      [and-exp [exp1 exp2]                      
          (let [
              [ev1 (value-of-exp exp1 env)]
              [ev2 (value-of-exp exp2 env)]
          ]
      (and (bool-val (expval->bool ev1)) (bool-val (expval->bool ev2))))
      ]

      [or-exp [exp1 exp2]                      
          (let [
              [ev1 (value-of-exp exp1 env)]
              [ev2 (value-of-exp exp2 env)]
          ]
      (or (bool-val (expval->bool ev1)) (bool-val (expval->bool ev2))))
      ]
      [not-exp [expression]                      
            (let [
                [ev (value-of-exp expression env)]
            ]
        (bool-val (not (expval->bool ev))))
      ]
      [cons-exp [exp1 exp2]
        (let [
          [ev1 (value-of-exp exp1 env)]
          [ev2 (value-of-exp exp2 env)]
        ]
        (pair-val ev1 ev2)
        )
      ]
      [car-exp [body]
        (expval->car (value-of-exp body env))
      ]
      [cdr-exp [body]
        (expval->cdr (value-of-exp body env))
      ]
      [null?-exp [expression]
        (expval->null? (value-of-exp expression env))
      ]
      [emptylist-exp () (emptylist-val)] 
	   [else (raise-exception 'value-of-exp "Abstract syntax case not implemented: ~s" (car exp))])))


;; =================== Interpreter =========================================

;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (display "\n=== Welcome to the CSC-370 Almost-LET Interpreter === \n\n")
      (display "\n
       _______________                        _______________
      |  ___________  |     .-.     .-.      |  ___________  |
      | |           | |    .****. .****.     | |           | |
      | |   x   x   | |    .*****.*****.     | |   ^   ^   | |
      | |     -     | |     .*********.      | |     o     | |
      | |    xxx    | |      .*******.       | |   -----   | |
      | |___     ___| |       .*****.        | |___________| |
      |_____|/_/|_____|        .***.         |_______________|
        _|__|/ /|_|_.............*.............._|________|_
       [ ********** ]                          [ ********** ]
      [ ************ ]                        [ ************ ] \n\n")
      (read-eval-print (make-init-env)))))


;; (read-eval-print) -- Main read, eval, and print loop.
(define read-eval-print
  (lambda (env)
    ;; Display an interpreter prompt
    (display "==> ")
    ;; Read a line user input
    (let ([concrete-code (get-input-string)])
      (cond
       [(equal? concrete-code "!quit")  
	      (display "Goodbye!")  ;; Quit if 'quit entered.
	      (newline)]
       [(equal? concrete-code "!debug1")
        (trace value-of-prog value-of-exp) 
        (read-eval-print env) 
       ]
       [(equal? concrete-code "!debug2")
        (trace value-of-prog value-of-exp 
        expval->bool expval->car expval->cdr expval->null? expval->num expval->pair expval->string)
        (read-eval-print env)
       ]
       [(equal? concrete-code "!debug0")
        (untrace value-of-prog value-of-exp 
        expval->bool expval->car expval->cdr expval->null? expval->num expval->pair expval->string)
        (read-eval-print env)
       ]
       [(equal? concrete-code "!env")
        (display (environ->string env))
        (newline)
        (read-eval-print env)
       ]
       [(equal? concrete-code "!reset-env")
        (read-eval-print (make-init-env))
       ]
       [else
        (guard ;;catch except
        [ex [else (display "PARSE ERROR: \n") (display-exception ex)]]
        ;; Parse code, eval expression, and print result.
        (let
            ([abstract-code (parse concrete-code)])
          (guard
            [ex [else (display "RUNTIME ERROR: \n") (display-exception ex)]]
            ; (display (expval->string (value-of-prog (car abstract-code) (cadr env)))) ;;this is what I intend to do for problem 5 part 2 but it didn't seem to work, still say expval, program is not a variant and went into an infinite loop
            (display (expval->string (value-of-prog abstract-code env)))
            (newline))))
        ;; "Loop".  Notice it is tail recursive.
        (read-eval-print env)]
))))
    
