; Israel Hill idh
; Michael Rosenfield mer95
; Sarah Whelan slw96

; Part 2

(load "simpleParser.scm")
(load "lex.scm")

;gets the parse tree of the input file and interprets the program
(define interpret
  (lambda (filename)
    (lookup 'return (decideState (parser filename) (initialState) (lambda (v) v)))))
    ;(decideState (parser filename) (initialState) (lambda (v) v))))
    ;(parser filename)))

;the default state
(define initialState
  (lambda ()
      '((true false return) (#t #f 'noReturnValueSet))))

;decide state determines and changes the state of an statement
(define decideState
  (lambda (l state return)
    (cond
     ((null? l) (return state))
     ((atom? l) (return state))
     ((list? (car l)) (decideState (car l) state (lambda (v) (decideState (cdr l) v return))))
     ((eq? (car l) 'return) (stateReturn l state return))
     ((eq? (car l) 'while) (stateWhile l state return))
     ((eq? (car l) 'var) (stateDeclaration l state return))
     ((eq? (car l) 'if) (stateIf l state return))
     ((eq? (car l) 'begin) (stateBegin l (addLayer state) return))
     ((eq? (car l) '=) (stateAssign l state return))
     ((not (null? (cdr l))) (decideState (cdr l) state return))
     (else (return state))
     )))

;handles return statements
(define stateReturn
  (lambda (l state return)
    (cond
      ((eq? (getValue (cdr l) state) '#t) (variable-handler 'return 'true state return))
      ((eq? (getValue (cdr l) state) '#f) (variable-handler 'return 'false state return))
      (else (decideState (cdr l) state (lambda (v) (variable-handler 'return (getValue (cdr l) v) v return)))))))
      
;handles declarations
(define stateDeclaration
  (lambda (l state return)
    (cond
      ((doesExist (leftoperand l) state) (error 'variableAlreadyDeclared))
      ((null? (cdr (cdr l))) (variable-handler (leftoperand l) 'declared state return))
      (else (decideState (rightoperand l) state (lambda (v)(variable-handler (leftoperand l) (getValue (rightoperand l) v) v return)))))))

;handles assignments
(define stateAssign
  (lambda (l state return)
    (cond
      ((not (doesExist (leftoperand l) state)) (error 'usingBeforeDeclaring))
      ((eq? (lookup (leftoperand l) state) 'declared) (decideState (rightoperand l) state (lambda (v) (variable-handler (leftoperand l) (getValue l v) v return))))
      (else (decideState (rightoperand l) state (lambda (v)(variable-handler (leftoperand l) (getValue l v) v return)))))))

;handles if statements
(define stateIf
  (lambda (l state return)
    (cond
      ((getTruth (car (cdr l)) state) (decideState (car (cdr l)) state (lambda (v) (decideState (car (cdr (cdr l))) v return))))
      ((null? (cdr (cdr (cdr l)))) (decideState (car (cdr l)) state return))
      (else (decideState (car (cdr l)) state (lambda (v) (decideState (car (cdr (cdr (cdr l)))) v return)))))))

;handles blocks/begin
(define stateBegin
  (lambda (l state return)
    (cond
      ((null? l) (return (removeLayer state)))
      (else (decideState (car l) state (lambda (v)(stateBegin (cdr l) v return)))))))

;handles while loops
(define stateWhile
  (lambda (l state return)
    (cond
      ((null? l) state)
      ((getTruth (leftoperand l) state) (decideState (leftoperand l) state (lambda (v1)
                                                                             (decideState (rightoperand l) v1 (lambda (v2)(decideState l v2 return))))))
      (else (decideState (leftoperand l) state return)))))

;returns the value of an expression
(define getValue
  (lambda (expression state)
       (cond
         ((number? expression) expression)
         ((and (atom? expression) (eq? (lookup expression state) 'declared)) (error 'usingBeforeAssigning))
         ((and (atom? expression) (eq? (lookup expression state) '())) (error 'usingBeforeDeclaring))
         ((atom? expression) (lookup expression state))
         ((eq? '+ (operator expression)) (+ (getValue (leftoperand expression) state)
                                            (getValue (rightoperand expression) state)))         
         ((eq? '/ (operator expression)) (quotient (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) state)))
         ((eq? '% (operator expression)) (remainder (getValue (leftoperand expression) state)
                                                    (getValue (rightoperand expression) state)))
         ((eq? '* (operator expression)) (* (getValue (leftoperand expression) state)
                                            (getValue (rightoperand expression) state)))
         ((and (eq? '- (operator expression))(not (null? (cdr (cdr expression)))))
          (- (getValue (leftoperand expression) state)(getValue (rightoperand expression) state)))
         ((eq? '- (operator expression)) (- (getValue (leftoperand expression) state)))
         ((eq? '= (operator expression)) (getValue (rightoperand expression) state))
         ((eq? 'var (operator expression)) (getValue (rightoperand expression) state))
         
         ((eq? '!= (operator expression))  (getTruth expression state))
         ((eq? '== (operator expression))  (getTruth expression state))
         ((eq? '<= (operator expression))  (getTruth expression state))
         ((eq? '>= (operator expression))  (getTruth expression state))
         ((eq? '< (operator expression))  (getTruth expression state))
         ((eq? '> (operator expression))  (getTruth expression state))
         ((eq? '! (operator expression))  (getTruth expression state))
         ((eq? '&& (operator expression))  (getTruth expression state))
         ((eq? '|| (operator expression))  (getTruth expression state))
         ((null? (cdr expression)) (getValue (car expression) state))
        (else (error 'illegalExpression)))
       ))

;evaluates boolean result of an expression
(define getTruth
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((not (pair? expression)) (lookup expression state))
      ((eq? '< (operator expression)) (< (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) state)))
      ((eq? '> (operator expression)) (> (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) state)))
      ((eq? '<= (operator expression)) (<= (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) state)))
      ((eq? '>= (operator expression)) (>= (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) state)))
      ((eq? '== (operator expression)) (eq? (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not(eq? (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) state))))
      ((eq? '&& (operator expression)) (and (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) state)))
      ((eq? '|| (operator expression)) (or (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) state)))
      ((eq? '! (operator expression))  (not(getValue (leftoperand expression)state)))
      )))

;looks up the value of a variable
(define lookup
  (lambda (name state)
    (cond
      ((null? state) (error 'lookupVariableNotDeclared))
      ((atom? (car (car state)))(lookup-helper name state))
      ((and (list? (car (car state))) (not (null? (lookup-helper name (car state))))) (lookup-helper name (car state)))
      (else (lookup name (cdr state))))))

;lookup a variable's value in the current state
(define lookup-helper
  (lambda (name state)
    (cond
     ((null? (car state)) '())
     ((eq? (car (variableList state)) name) (car (valueList state)))
     (else (lookup-helper name (cons (cdr (variableList state)) (cons(cdr(valueList state)) '())))))))

(define variable-handler
  (lambda (name value state return)
    (if (doesExist name state) (return (Update name value state))
        (return(Add name value state)))))

(define Update
 (lambda (name value state)
  (cond 
     ((null? state) '())
      ((and (atom? (car (car state))) (doesExist-helper name state)) (Update-helper name value state))
      ((and (list? (car (car state))) (doesExist-helper name (car state))) (cons (Update-helper name value (car state)) (cdr state)))
      (else (cons (car state) (Update name value (cdr state)))))))

(define Update-helper
  (lambda (name value state)
    (cond
      ((null? (car state)) '())
      ((eq? (car (variableList state)) name) (cons (car state) (cons (cons value (cdr (car (cdr state)))) '())))
      (else (cons 
             (cons (car (variableList state)) (car (Update-helper name value (cons (cdr (variableList state)) (cons(cdr(valueList state)) '()))) ))
             (cons
              (cons (car(valueList state)) (car (cdr (Update-helper name value (cons (cdr (variableList state)) (cons(cdr(valueList state)) '()))) )))
              '())
             )
            ))))

(define Add
  (lambda (name value state)
    (cond 
      ((atom? (car (car state)))(Add-helper name value state))
      ((list? (car state))(cons (Add-helper name value (car state)) (cdr state)))
      (else (Add-helper name value state)))))
          

;add or update variables in the current state
(define Add-helper
  (lambda (name value state)
    (cond
     ((null? (car state))
             (cons (append (variableList state) (cons name '()))
                   (cons 
                    (append (valueList state) (cons value '()))
                    '())))
     (else (cons 
             (cons (car (variableList state)) (car (Add-helper name value (cons (cdr (variableList state)) (cons(cdr(valueList state)) '()))) ))
             (cons
             (cons (car(valueList state)) (car (cdr (Add-helper name value (cons (cdr (variableList state)) (cons(cdr(valueList state)) '()))) )))
             '())
             )
            ))))

 ;abstraction of operators     
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

;checks if the input is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define doesExist
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((atom? (car (car state))) (doesExist-helper name state))
      ((list? (car state)) (or (doesExist name (car state)) (doesExist name (cdr state))))
      (else (doesExist-helper name state)))))

;checks if the given variable is declared/assigned in the current state
(define doesExist-helper
  (lambda (name state)
    (cond
     ((null? (car state)) #f)
     ((eq? (car (variableList state)) name) #t)
     (else (doesExist-helper name (cons (cdr (variableList state)) (cons(cdr(valueList state)) '())))))))

;get all the current variables in the state
(define variableList
  (lambda (state)
    (car state)))

;get all the current values of the associated variables in the state
(define valueList
  (lambda (state)
    (car(cdr state))))

;adds a layer to the current state
(define addLayer
  (lambda (state)
    (cond
    ((not (list? (car (car state))))(cons (initialState)(cons state '())))
    (else (cons (initialState) state)))))       

;remove a layer from the current state
(define removeLayer
  (lambda (state)
    (cdr state)))
