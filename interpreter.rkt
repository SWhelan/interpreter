(load "simpleParser.scm")
(load "lex.scm")

(define interpret
  (lambda (filename)
    (lookup 'return (decideState (parser filename) (initialState)))))
    ;(parser filename)))

(define initialState
  (lambda ()
      '((true false return) (#t #f 'noReturnValueSet))))

(define decideState
  (lambda (l state)
    (cond
     ((null? l) state)
     ((atom? l) state)
     ((list? (car l)) (decideState (cdr l) (decideState (car l) state)))
     ((eq? (car l) 'return) (stateReturn l state))
     ((eq? (car l) 'var) (stateDeclaration l state))
     ((eq? (car l) 'if) (stateIf l state))
     ((eq? (car l) '=) (stateAssign l state))
     ((not (null? (cdr l))) (decideState (cdr l) state))
     (else state)
     )))

(define stateReturn
  (lambda (l state)
    (cond
      ((eq? (getValue (cdr l) state) '#t) (Add 'return 'true state))
      ((eq? (getValue (cdr l) state) '#f) (Add 'return 'false state))
      (else (Add 'return (getValue (cdr l) state) (decideState (cdr l) state))))))

(define stateDeclaration
  (lambda (l state)
    (cond
      ((doesExist (leftoperand l) state) (error 'variableAlreadyDeclared))
      ((null? (cdr (cdr l))) (Add (leftoperand l) 'declared state))
      (else (Add (leftoperand l) (getValue (rightoperand l) state) (decideState (rightoperand l) state))))))

(define stateIf
  (lambda (l state)
    (cond
      ((getTruth (car (cdr l)) state) (decideState(car (cdr (cdr l))) (decideState (car (cdr l)) state)))
      ((null? (cdr (cdr (cdr l)))) (decideState (car (cdr l)) state))
      (else (decideState (car (cdr (cdr (cdr l)))) (decideState (car (cdr l)) state))))))

(define doesExist
  (lambda (name state)
    (cond
     ((null? (car state)) #f)
     ((eq? (car (variableList state)) name) #t)
     (else (doesExist name (cons (cdr (variableList state)) (cons(cdr(valueList state)) '())))))))

(define variableList
  (lambda (state)
    (car state)))

(define valueList
  (lambda (state)
    (car(cdr state))))

(define stateAssign
  (lambda (l state)
    (cond
      ((eq? (lookup (leftoperand l) state) 'declared) (Add (leftoperand l) (getValue l state) (decideState (rightoperand l) state)))
      (else (Add (leftoperand l) (getValue l state) (decideState (rightoperand l) state))))))

(define lookup
  (lambda (name state)
    (cond
     ((null? (car state)) (error 'lookupvariableNotDecalared))
     ((eq? (car (variableList state)) name) (car (valueList state)))
     (else (lookup name (cons (cdr (variableList state)) (cons(cdr(valueList state)) '())))))))

(define Add
  (lambda (name value state)
    (cond
     ((null? (car state))
             (cons (append (variableList state) (cons name '()))
                   (cons 
                    (append (valueList state) (cons value '()))
                    '())))
     ((eq? (car (car state)) name) 
       (cons (car state) (cons (cons value (cdr (car (cdr state)))) '())))
     (else (cons 
             (cons (car (variableList state)) (car (Add name value (cons (cdr (variableList state)) (cons(cdr(valueList state)) '()))) ))
             (cons
             (cons (car(valueList state)) (car (cdr (Add name value (cons (cdr (variableList state)) (cons(cdr(valueList state)) '()))) )))
             '())
             )
            ))))

(define getValue
  (lambda (expression state)
       (cond
         ((number? expression) expression)
         ((and (atom? expression) (eq? (lookup expression state) 'declared)) (error 'usingBeforeAssigning))
         ((atom? expression) (lookup expression state))
         ((eq? '+ (operator expression)) (+ (getValue (leftoperand expression) state)
                                            (getValue (rightoperand expression) (decideState (leftoperand expression)state))))
         ((eq? '/ (operator expression)) (quotient (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) (decideState (leftoperand expression) state))))
         ((eq? '% (operator expression)) (remainder (getValue (leftoperand expression) state)
                                                    (getValue (rightoperand expression) (decideState (leftoperand expression) state))))
         ((eq? '* (operator expression)) (* (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) (decideState (leftoperand expression) state))))
         ((and (eq? '- (operator expression))(not (null? (cdr (cdr expression))))) 
          (- (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) (decideState (leftoperand expression) state))))
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
        (else (error expression)))
       ))

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
      ((eq? '! (operator expression))  (not(getValue (leftoperand expression) state)))
      )))
      
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))