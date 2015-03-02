; Israel Hill idh
; Michael Rosenfield mer95
; Sarah Whelan slw96

; Part 2

(load "simpleParser.scm")
(load "lex.scm")

;gets the parse tree of the input file and interprets the program
(define interpret
  (lambda (filename)
    (lookup 'return (decideState (parser filename) (initialState) (lambda (v) v) (lambda (v) v) (lambda (v) v)))))
   ; (parser filename)))

;the default state
(define initialState
  (lambda ()
      '((true false return) (#t #f 'noReturnValueSet))))

;decide state determines and changes the state of an statement
(define decideState
  (lambda (l state return break continue)
    (cond
     ((null? l) state)
     ((atom? l) state)
     ;((list? (car l)) (decideState (cdr l) (decideState (car l) state)))
     ((list? (car l)) (decideState (car l) state (lambda (v) (decidestate (cdr l) v return break continue)) break continue))
     ((eq? (car l) 'return) (stateReturn l state return break continue))
     ((eq? (car l) 'var) (stateDeclaration l state return break continue))
     ((eq? (car l) 'if) (stateIf l state return break continue))
     ((eq? (car l) 'begin) (stateBegin l (addLayer state) return break continue))
     ((eq? (car l) '=) (stateAssign l state return break continue))
     ((not (null? (cdr l))) (decideState (cdr l) state return break continue))
     (else state)
     )))

;handles return statements
(define stateReturn
  (lambda (l state return break continue)
    (cond
      ((eq? (getValue (cdr l) state return break continue) '#t) (variable-handler 'return 'true state))
      ((eq? (getValue (cdr l) state return break continue) '#f) (variable-handler 'return 'false state))
      (else (variable-handler 'return (getValue (cdr l) state return break continue) (decideState (cdr l) state return break continue))))))

;handles declarations
(define stateDeclaration
  (lambda (l state return break continue)
    (cond
      ((doesExist (leftoperand l) state) (error 'variableAlreadyDeclared))
      ((null? (cdr (cdr l))) (variable-handler (leftoperand l) 'declared state))
      (else (variable-handler (leftoperand l) (getValue (rightoperand l) state return break continue) (decideState (rightoperand l) state return break continue))))))

;handles if statements
(define stateIf
  (lambda (l state return break continue)
    (cond
      ((getTruth (car (cdr l)) state) (decideState(car (cdr (cdr l))) (decideState (car (cdr l)) state return break continue)) return break continue)
      ((null? (cdr (cdr (cdr l)))) (decideState (car (cdr l)) state return break continue))
      (else (decideState (car (cdr (cdr (cdr l)))) (decideState (car (cdr l)) state return break continue)) return break continue))))

;handles assignments
(define stateAssign
  (lambda (l state return break continue)
    (cond
      ((eq? (lookup (leftoperand l) state) 'declared) (variable-handler (leftoperand l) (getValue l state) (decideState (rightoperand l) state return break continue)))
      (else (variable-handler (leftoperand l) (getValue l state) (decideState (rightoperand l) state return break continue))))))

;handles blocks/begin
(define stateBegin
  (lambda (l state return break continue)
    (cond
      ((null? l) state)
      (else (stateBegin (cdr l) (decideState (car l) state return break continue))))))


;returns the value of an expression
(define getValue
  (lambda (expression state return break continue)
       (cond
         ((number? expression) expression)
         ((and (atom? expression) (eq? (lookup expression state) 'declared)) (error 'usingBeforeAssigning))
         ((atom? expression) (lookup expression state))
         ((eq? '+ (operator expression)) (+ (getValue (leftoperand expression) state)
                                            (getValue (rightoperand expression) (decideState (leftoperand expression)state return break continue))))
         ((eq? '/ (operator expression)) (quotient (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
         ((eq? '% (operator expression)) (remainder (getValue (leftoperand expression) state)
                                                    (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
         ((eq? '* (operator expression)) (* (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
         ((and (eq? '- (operator expression))(not (null? (cdr (cdr expression))))) 
          (- (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
         ((eq? '- (operator expression)) (- (getValue (leftoperand expression) (decideState (leftoperand expression) state return break continue))))
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
         ((null? (cdr expression)) (getValue (car expression) state return break continue))
        (else (error expression)))
       ))

;evaluates boolean result of an expression
(define getTruth
  (lambda (expression state return break continue)
    (cond
      ((number? expression) expression)
      ((not (pair? expression)) (lookup expression state))
      ((eq? '< (operator expression)) (< (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
      ((eq? '> (operator expression)) (> (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
      ((eq? '<= (operator expression)) (<= (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
      ((eq? '>= (operator expression)) (>= (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
      ((eq? '== (operator expression)) (eq? (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
      ((eq? '!= (operator expression)) (not(eq? (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue)))))
      ((eq? '&& (operator expression)) (and (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
      ((eq? '|| (operator expression)) (or (getValue (leftoperand expression) state)
                                         (getValue (rightoperand expression) (decideState (leftoperand expression) state return break continue))))
      ((eq? '! (operator expression))  (not(getValue (leftoperand expression) (decideState (leftoperand expression) state return break continue))))
      )))


(define lookup
  (lambda (name state)
    (cond
      ((null? state) (error 'lookupvariableNotDecalared))
      ((not (null? (lookup-helper name (car state)))) (lookup-helper name (car state)))
      (else (lookup name (cdr state))))))

(define lookup
  (lambda (name state)
    (cond
      ((null? state) (error 'lookvariableNotDeclaed))
      ((atom? (car (car state))) (lookup-helper name state))
      ((and (list? (car (car state))) (not (null? (lookup-helper name (car state))))) (lookup-helper (car state)))
      (else (lookup name (cdr state))))))

;lookup a variable's value in the current state
(define lookup-helper
  (lambda (name state)
    (cond
     ((null? (car state)) '())
     ((eq? (car (variableList state)) name) (car (valueList state)))
     (else (lookup-helper name (cons (cdr (variableList state)) (cons(cdr(valueList state)) '())))))))

(define variable-handler
  (lambda (name value state)
    (if (doesExist name state) (Update name value state)
        (Add name value state))))

(define Update
 (lambda (name value state)
  (cond 
     ((null? state) '())
      ((and (atom? (car (car state))) (doesExist-helper name state)) (cons (Update-helper name value state) (cdr state)))
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
    (cons (initialState)(cons state '()))))