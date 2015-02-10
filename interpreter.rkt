(load "simpleParser.scm")
(load "lex.scm")

(define interpret
  (lambda (filename)
    (decideState (parser filename) '((true false return) (#t #f 'noReturnValueSet)))))
    ;(parser filename)))

(define decideState
  (lambda (l state)
    (cond
     ((null? l) (lookup 'return state))
     ((list? (car l)) (decideState (cdr l) (decideState (car l) state)))
     ((eq? (car l) 'return) (stateReturn l state))
     ((eq? (car l) 'var) (stateDeclaration l state))
     ((eq? (car l) 'if) (stateIf l state))
     (else (stateAssign l state)))))

(define stateReturn
  (lambda (l state)
    (cond
      (else (Add 'return (getValue (cdr l) state) state)))))

(define stateDeclaration
  (lambda (l state)
    (cond
      ((null? (cdr (cdr l))) (Add (car(cdr l)) 'declared state))
      (else (Add (cdr l) (getValue (cdr (cdr l)) state) state)))))

(define stateIf
  (lambda (l state)
    (cond
      ((getTruth (car (cdr l)) state) (decideState (cdr (cdr l))))
      (else (decideState (cdr (cdr (cdr l))))))))

(define stateAssign
  (lambda (l state)
    (cond
      ((eq? (lookup (leftoperand l) state) 'declared) (Add (car(cdr l)) (getValue l state) state))
      (else (Add (car(cdr l)) (getValue l state) state)))))

(define lookup
  (lambda (name state)
    (cond
     ((null? (car state)) (error 'lookupvariableNotDecalared))
     ((eq? (car (car state)) name) (car (car (cdr state))))
     (else (lookup name (cons (cdr (car state)) (cons(cdr(car(cdr state))) '())))))))

;(define lookup
;  (lambda (name state)
;    (cond
;     ((null? (car state)) (error 'youmessedup))
;     ((eq? (car (car state)) name) (car (car (cdr state))))
;     (else (lookup name (cons (remainingVariables state) (remainingValues state)))))))
;
;(define remainingVariables (cons cdr (cons car '())))
;(define remainingValues (cons cdr (cons car (cons cdr '()))))

;(define Add
 ; (lambda (name value state)
  ;  (cons (append (car state) (cons name '())) (cons (append (car (cdr state)) (cons value '())) '()))


(define Add
  (lambda (name value state)
    (cond
     ((null? (car state))
             (cons (append (car state) (cons name '()))
                   (cons 
                    (append (car (cdr state)) (cons value '()))
                    '())))
     ((eq? (car (car state)) name) 
       (cons (car state) (cons (cons value (cdr (car (cdr state)))) '())))
     (else (cons 
             (cons (car (car state)) (car (Add name value (cons (cdr (car state)) (cons(cdr(car(cdr state))) '()))) )); (x z)
             (cons
             (cons (car(car(cdr state))) (car (cdr (Add name value (cons (cdr (car state)) (cons(cdr(car(cdr state))) '()))) )))
             '())
             )
            ))))

(define Remove
  (lambda (name state)
    (cond
      ((null? (car state)) state)
      ((eq? (car(car state)) name) (cons (cdr (car state))(cons (cdr(car(cdr state))) '())))
      (else (cons 
             (cons (car (car state)) (car (Remove name (cons (cdr (car state)) (cons(cdr(car(cdr state))) '()))))); (x z)
             (cons
             (cons (car(car(cdr state))) (car (cdr (Remove name (cons (cdr (car state)) (cons(cdr(car(cdr state))) '()))))))
             '())
             )
            )))) ; don't delete this


(define getValue
  (lambda (expression state)
       (cond
        ; ((null? expression) '())
         ((number? expression) expression)
         ((atom? expression) (lookup expression state))
         ((eq? '+ (operator expression)) (+ (getValue (leftoperand expression) state)
                                            (getValue (rightoperand expression) state)))
         ((eq? '/ (operator expression)) (quotient (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) state)))
         ((eq? '% (operator expression)) (remainder (getValue (leftoperand expression) state)
                                                    (getValue (rightoperand expression) state)))
         ((eq? '* (operator expression)) (* (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) state)))
         ((and (eq? '- (operator expression))(not (null? (rightoperand expression)))) (- (getValue (leftoperand expression) state)
                                                   (getValue (rightoperand expression) state)))
         ((eq? '- (operator expression)) (- (getValue (leftoperand expression) state)))
         ((eq? '= (operator expression)) (getValue (rightoperand expression) state))
         
         
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