; Israel Hill idh
; Michael Rosenfield mer95
; Sarah Whelan slw96

; Part 3

(load "functionParser.scm")
(load "lex.scm")

;main interpret function
(define interpret
  (lambda (filename)
    (lookup 'return (functionCall '(funcall main) (interpretOuter filename) (lambda (v) v)))))

(define interpretDebug
  (lambda (filename)
    (functionCall '(funcall main) (interpretOuter filename) (lambda (v) v))))

;adds global variables and creates functions
(define interpretOuter
  (lambda (filename)(decideStateOuter (parser filename) (initialState) (lambda (v) v) (lambda (v) (v)) (lambda (v) v) (lambda (v) v))))

;the default state
(define initialState
  (lambda ()
    (cons '(true false return) (cons (cons (box #t) (cons (box #f)(cons (box 'noReturnValueSet) '()))) '()))))

(define parserOutput
  (lambda (filename)
    (parser filename)))

;;;;;; Interpret Whole File

;only allows variables and functions
(define decideStateOuter
  (lambda (l state return continue break exit)
    (cond
     ((null? l) (return state))
     ((list? (car l)) (decideStateOuter (car l) state (lambda (v) (decideStateOuter (cdr l) v return continue break exit)) continue break exit))
     ((eq? (car l) 'function) (stateFunction l state return continue break exit))
     ((eq? (car l) 'var) (stateDeclarationOuter l state return continue break exit))
     ((eq? (car l) '=) (stateAssignOuter l state return continue break exit))
     (else (return state)))))

;handles variable declarations
(define stateDeclarationOuter
  (lambda (l state return continue break exit)
    (cond
      ((not (null? (lookup (leftoperand l) state))) (error 'variableAlreadyDeclared))
      ((null? (cdr (cdr l))) (variable-handler (leftoperand l) 'declared state return))
     ; (else (decideStateOuter (rightoperand l) state (lambda (v)(variable-handler (leftoperand l) (getValue (rightoperand l) v) v return)) continue break exit)))))
(else (variable-handler (leftoperand l) (getValue (rightoperand l) state) state return)))))
;(else (variable-handler (leftoperand l) (getValue (rightoperand l) v) v return)))))

;handles variable assignments
(define stateAssignOuter
  (lambda (l state return continue break exit)
    (cond
      ((null? (lookup (leftoperand l) state)) (error 'usingBeforeDeclaring))
      ((eq? (lookup (leftoperand l) state) 'declared) (variable-handler (leftoperand l) (getValue (rightoperand l) state) state return))
      (else (variable-handler (leftoperand l)(getValue (rightoperand l) state) state return)))))

;handles function definitions
(define stateFunction
  (lambda (l state return continue break exit)    
    (return (variable-handler (functionName l) (makeClosure (functionBody l) (functionParamList l) state) state (lambda(v) v)))))

;make a function closure to store in the environment
(define makeClosure
  (lambda (body params state)
    (cons params (cons body (cons (getNumLayers state) '())))))

;evaluate a functioncall
(define functionCall
  (lambda (l state return)
    (cond
      ((eq? (lookup (functionCallName l) state) '()) (error 'illegalFunctionCall))
      (else (decideState (functionClosureBody (lookup (functionCallName l) state))
                         (copyParams (functionCallParamList l) state (functionClosureParamList (lookup (functionCallName l) state))
                                     (addLayer (getLastN state (getFunctionClosureLayerNum (lookup (functionCallName l) state))))
                                     )
                         (lambda (v) v) (lambda (v) (v)) (lambda (v) v) (lambda (v) v))))))

;copy the actual paramters into the formal parameters
(define copyParams
  (lambda (actual state formal stateFromClosure)
    (cond
      ((not (equalNumElements? actual formal)) (error 'mismatchParameters))
      ((null? actual) stateFromClosure)
      (else (copyParams (cdr actual) state (cdr formal) (Add (car formal) (getValue (car actual) state) stateFromClosure))))))

(define equalNumElements?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (not (null? l2))) #f)
      ((and (null? l2) (not (null? l1))) #f)
      (else (equalNumElements? (cdr l1) (cdr l2))))))

;get the outer layer of the environment
(define outerLayer
  (lambda (state)
    (cond
      ((atom? (car (car state))) state)
      ((list? (car state)) (car (cdr state)))
      (else (outerLayer (cdr state))))))

(define getNumLayers
  (lambda (l)
    (cond
      ((null? l) 0)
      ((atom? (car (car l))) 1)
      (else (+ 1 (getNumLayers (cdr l)))))))

(define removeLast
  (lambda (l)
    (cond 
      ((null? (cdr l)) '())
      (else (cons (car l) (removeLast (cdr l)))))))
 
(define getLast
  (lambda (l)
    (cond
      ((null? (cdr l)) (car l))
      (else (getLast (cdr l))))))
 
(define getLastNInner
  (lambda (l1 l2 n)
    (cond 
      ((null? l1) l2)
      ((and (zero? n) (null? (cdr l2))) (car l2))
      ((zero? n) l2)
      (else (getLastNInner (removeLast l1) (cons (getLast l1) l2) (- n 1))))))

(define getLastN
  (lambda (l1 n)
    (cond
      ((and (eq? 1 n) (atom? (car (car l1)))) l1)
      (else (getLastNInner l1 '() n)))))

;get the top layer of the environment
(define topLayer
  (lambda (state)
    (cond
      ((null? (car state)) state)
      ((atom? (car (car state))) state)
      ((list? (car state)) (car state))
      (else (topLayer (cdr state))))))

;;;;;; Interpret the Function Body

;decide state determines and changes the state of a statement
(define decideState
  (lambda (l state return continue break exit)
    (cond
     ((null? l) (return state))
     ((atom? l) (return state))
     ((list? (car l)) (decideState (car l) state (lambda (v) (decideState (cdr l) v return continue break exit)) continue break exit))
     ((eq? (car l) 'return) (stateReturn l state return continue break exit))
     ((eq? (car l) 'while) (stateWhile l state return continue (lambda (v) (return (removeLayer v))) exit))
     ((eq? (car l) 'function) (stateFunction l state return continue break exit))
     ((eq? (car l) 'funcall) (functionCall l state return))
     ((eq? (car l) 'var) (stateDeclaration l state return continue break exit))
     ((eq? (car l) 'if) (stateIf l state return continue break exit))
     ((eq? (car l) 'break) (break state))
     ((eq? (car l) 'begin) (stateBlock l (addLayer state) return continue break exit))
     ((eq? (car l) 'continue) (continue state))
     ((eq? (car l) '=) (stateAssign l state return continue break exit))
     (else (return state))
     )))

;handles return statements by exiting to the beginning of the program (using the exit continuation set at the inital call of stateDecide)
(define stateReturn
  (lambda (l state return continue break exit)
    (cond
      ((eq? (getValue (cdr l) state) '#t) (variable-handler 'return 'true state exit))
      ((eq? (getValue (cdr l) state) '#f) (variable-handler 'return 'false state exit))
      ;(else (decideState (cdr l) state (lambda (v)(variable-handler 'return (getValue (cdr l) v) v exit)) continue break exit)))))
      (else (variable-handler 'return (getValue (cdr l) state) state exit)))))

;handles variable declarations
(define stateDeclaration
  (lambda (l state return continue break exit)
    (cond
      ((not (null? (lookup (leftoperand l) (topLayer state)))) (error 'variableAlreadyDeclared))
      ((null? (cdr (cdr l))) (variable-handler (leftoperand l) 'declared state return))
      (else (variable-handler (leftoperand l) (getValue (rightoperand l) state) (topLayer state)
                              (lambda (v) (return (cond
                                                    ;((null? (remainingLayers state)) v)
                                                    (else (cons v (cons (remainingLayers state)'())))))))))))

(define remainingLayers
  (lambda (state)
    (cond
      ((null? (car state)) '())
      ((atom? (car (car state))) '())
      ((and (list? (car state)) (null? (cdr (cdr state)))) (car (cdr state)))
      ((list? (car state)) (cdr state)))))

;handles variable assignments
(define stateAssign
  (lambda (l state return continue break exit)
    (cond
      ((null? (lookup (leftoperand l) state)) (error 'usingBeforeDeclaringOrOutOfScope))
      ((eq? (lookup (leftoperand l) state) 'declared) (variable-handler (leftoperand l) (getValue (rightoperand l) state) state return))
      (else (variable-handler (leftoperand l)(getValue (rightoperand l) state) state return)))))

;handles if statements
(define stateIf
  (lambda (l state return continue break exit)
    (cond
      ((getTruth (car (cdr l)) state) (decideState (car (cdr l)) state (lambda (v) (decideState (car (cdr (cdr l))) v return continue break exit)) continue break exit))
      ((null? (cdr (cdr (cdr l)))) (decideState (car (cdr l)) state return continue break exit))
      (else (decideState (car (cdr l)) state (lambda (v) (decideState (car (cdr (cdr (cdr l)))) v return continue break exit)) continue break exit)))))

(define condition
  (lambda (l)
    (car (cdr l))))

(define conditionTrue
  (lambda (l)
    (car (cdr (cdr l)))))

(define conditionFalse
  (lambda (l)
    (car (cdr (cdr (cdr l))))))

;handles blocks
(define stateBlock
  (lambda (l state return continue break exit)
    (cond
      ((null? l) (return (removeLayer state)))
      (else (decideState (car l) state (lambda (v)
                                         (stateBlock (cdr l) v return continue break exit)) (lambda (v) (return v)) break exit)))))

;handles while loops
(define stateWhile
  (lambda (l state return continue break exit)
    (cond
      ((null? l) state)
      ((getTruth (leftoperand l) state) (decideState (leftoperand l) state (lambda (v1)
        (decideState (rightoperand l) v1 (lambda (v2)(decideState l v2 return continue break exit)) continue break exit)) continue break exit))
      (else (decideState (leftoperand l) state return continue break exit)))))

;;;;;; Value

;returns the value of an expression
(define getValue
  (lambda (expression state)
       (cond
         ((number? expression) expression)
         ((and (atom? expression) (eq? (lookup expression state) 'declared)) (error 'usingBeforeAssigning))
         ((and (atom? expression) (eq? (lookup expression state) '())) (error 'usingBeforeDeclaring))
         ((atom? expression) (lookup expression state))
         ((eq? (operator expression) 'funcall) (lookup 'return (functionCall expression state (lambda (v) v))))
         
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
         ((eq? '= (operator expression)) (getValue (leftoperand expression) (variable-handler (leftoperand expression) (getValue (rightoperand expression) state) state (lambda (v) v))))
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
      ((eq? (operator expression) 'funcall) (lookup 'return (functionCall expression state (lambda (v) v))))
      )))

;;;;;; Environment

;looks up the value of a variable handles multiple layers
(define lookup
  (lambda (name state)
    (cond
      ((null? state) '())
      ((atom? (car (car state)))(lookup-helper name state))
      ((and (list? (car (car state))) (not (null? (lookup-helper name (car state))))) (lookup-helper name (car state)))
      (else (lookup name (cdr state))))))

;lookup a variable's value in the layer it is given
(define lookup-helper
  (lambda (name state)
    (cond
     ((null? (car state)) '())
     ((eq? (firstVariable state) name) (unbox(firstValue state)))
     (else (lookup-helper name (cons (remainingVariables state) (cons(remainingValues state) '())))))))

;decides to add or update a variable in the state
(define variable-handler
  (lambda (name value state return)
    (if (not (null? (lookup name state))) (return (Update name value state))
        (return(Add name value state)))))

;updates the value of a variable in the state handles multiple layers
(define Update
 (lambda (name value state)
  (cond 
     ((null? state) '())
      ((and (atom? (car (car state))) (not (null? (lookup-helper name state)))) (Update-helper name value state))
      ((and (list? (car (car state))) (not (null? (lookup-helper name (car state))))) (cons (Update-helper name value (car state)) (cdr state)))
      (else (cons (car state) (Update name value (cdr state)))))))

;updates the value of a variable in the layer it was given
(define Update-helper
  (lambda (name value state)
    (cond
      ((null? (car state)) '())
      ((eq? (firstVariable state) name) (begin (set-box! (firstValue state) value) state))
      (else (cons 
             (cons (firstVariable state) (car (Update-helper name value (cons (remainingVariables state) (cons(remainingValues state) '()))) ))
             (cons
              (cons (firstValue state) (car (cdr (Update-helper name value (cons (remainingVariables state) (cons(remainingValues state) '()))) )))
              '())
             )
            ))))

;add a variable to the state handles multiple layers
(define Add
  (lambda (name value state)
    (cond 
      ((atom? (car (car state)))(Add-helper name value state))
      ((list? (car state))(cons (Add-helper name value (car state)) (cdr state)))
      (else (Add-helper name value state)))))


;adds a variable to the layer it is given
(define Add-helper
  (lambda (name value state)
    (cond
     ((null? (car state))
             (cons (append (variableList state) (cons name '()))
                   (cons 
                    (append (valueList state) (cons (box value) '()))
                    '())))
     (else (cons 
             (cons (firstVariable state) (car (Add-helper name value (cons (remainingVariables state) (cons(remainingValues state) '()))) ))
             (cons
             (cons (firstValue state) (car (cdr (Add-helper name value (cons (remainingVariables state) (cons(remainingValues state) '()))) )))
             '())
             )
            ))))

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

;;;;;; Abstraction

;abstraction of operators     
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

;checks if the input is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
 
;get all but the first variable in the state
(define remainingVariables
  (lambda (state)
    (cdr (variableList state))))

;get all but the first associated variable in the state
(define remainingValues
  (lambda (state)
    (cdr(valueList state))))

;get the first variable in the state
(define firstVariable
  (lambda (state)
    (car (variableList state))))

;get the first associated value in the state
(define firstValue
  (lambda (state)
    (car(valueList state))))

;get all the current variables in the state
(define variableList
  (lambda (state)
    (car state)))

;get all the current values of the associated variables in the state
(define valueList
  (lambda (state)
    (car(cdr state))))

;abstractions for the function definition passed by the parser
(define functionName
  (lambda (l)
    (car (cdr l))))

(define functionBody
  (lambda (l)
    (car (cdr (cdr (cdr l))))))

(define functionParamList
  (lambda (l)
    (car (cdr (cdr l)))))

;abstractions for the function closures
(define functionClosureBody
  (lambda (l)
    (car (cdr l))))

(define functionClosureParamList
  (lambda (l)
    (car l)))

(define getFunctionClosureLayerNum
  (lambda (l)
    (car (cdr (cdr l)))))

;abstractions for the function calls
(define functionCallName
  (lambda (l)
    (car (cdr l))))

(define functionCallParamList
  (lambda (l)
    (cdr (cdr l))))