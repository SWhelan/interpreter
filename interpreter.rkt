; Israel Hill idh
; Michael Rosenfield mer95
; Sarah Whelan slw96

; Part 3

(load "classParser.scm")
(load "lex.scm")

;main interpret function
(define interpret
  (lambda (filename classname)
    (lookup 'return (stateFunctionCall '(funcall main) 
                                       (interpretClasses (parser filename) (initialEmpty)) 
                                       (string->symbol classname) 
                                       (lambda (v) v) 
                                       (lambda (v) v))
            (string->symbol classname))))

(define interpretClasses
  (lambda (l state)
    (cond
      ((null? l) state)
      ((eq? (car l) 'class)(decideStateClass (classBody l) (Add (className l) (makeClass l) state) (className l) (lambda (v) v) (lambda (v) (v)) (lambda (v) v) (lambda (v) v)(lambda (v) v)))
      (else (interpretClasses (cdr l) (interpretClasses (car l) state))))))

(define initialEmpty
  (lambda ()
    (cons '(true false) (cons (cons (box #t) (cons (box #f) '())) '()))))

(define makeClass
  (lambda (l)
    (cond
      ((null? (classHeader l)) (cons (classHeader l) (cons (initialEmpty) (cons (initialEmpty) (cons (initialEmpty) '())))))
      (else (cons (car (cdr (classHeader l)))  (cons (initialEmpty) (cons (initialEmpty) (cons (initialEmpty) '()))))))))

(define decideStateClass
  (lambda (l state className return continue break exit catch)
    (let ([class (lookup className state className)])
    (cond
     ((null? l) (return state))
     ((list? (operator l)) (decideStateClass (operator l) state className (lambda (v) (decideStateClass (cdr l) v className return continue break exit catch)) continue break exit catch))
     ((eq? (operator l) 'static-function) (stateFunction l state className return continue break exit catch))
     ((eq? (operator l) 'static-var)(stateDeclarationClasses l state className return continue break exit catch))
     (else (return state))))))

;handles variable declarations
(define stateDeclarationClasses
  (lambda (l state className return continue break exit catch)
    (let ([class (lookup className state className)])
    (cond
      ((not (null? (lookupLocal (leftoperand l) (topLayer state)))) (error 'variableAlreadyDeclared))
      ((null? (cdr (cdr l))) (return (Update className (cons (classParent class) (cons (Add (leftoperand l) 'declared (classFields class)) (cons (classMethods class) (cons (classInitials class) '())))) state)))
      (else (return (Update className (cons (classParent class) (cons (Add (leftoperand l) (getValue (rightoperand l) state className catch) (classFields class)) (cons (classMethods class) (cons (classInitials class) '())))) state)))))))


(define parserOutput
  (lambda (filename classname)
    (parser filename)))


;the default state
(define initialStateWithReturn
  (lambda ()
    (cons '(true false return) (cons (cons (box #t) (cons (box #f)(cons (box 'noReturnValueSet) '()))) '()))))

;the additonal layer for each function call
(define initialState
  (lambda ()
    (cons '(true false) (cons (cons (box #t) (cons (box #f) '())) '()))))

;;;;;; Interpret Each Class

;decide state determines and changes the state of a statement
(define decideState
  (lambda (l state className return continue break exit catch)
    (cond
     ((null? l) (return state))
     ((atom? l) (return state))
     ((list? (operator l)) (decideState (operator l) state className (lambda (v) (decideState (cdr l) v className return continue break exit catch)) continue break exit catch))
     ((eq? (operator l) 'return) (stateReturn l state className return continue break exit catch))
     ((eq? (operator l) 'while) (stateWhile l state className return continue (lambda (v) (return (removeLayer v))) exit catch))
     ((eq? (operator l) 'function) (stateFunction l state className return continue break exit catch))
     ((eq? (operator l) 'funcall) (stateFunctionCall l state className return catch))
     ((eq? (operator l) 'var) (stateDeclaration l state className return continue break exit catch))
     ((eq? (operator l) 'if) (stateIf l state className return continue break exit catch))
     ((eq? (operator l) 'break) (break state))
     ((eq? (operator l) 'try) (stateBlock (tryBody l) state className 
                                          (lambda (v) (stateBlock (finallyBody l) state className return continue break exit catch))
                                          continue break exit 
                                          (lambda (v) (stateBlock (catchBody l) (Add (exceptionName l) v state) className
                                                                  return
                                                                  continue break exit catch))))
     ((eq? (operator l) 'throw) (catch (getValue (cadr l) state className catch)))
     ((eq? (operator l) 'begin) (stateBlock l (addLayer state) className return continue break exit catch))
     ((eq? (operator l) 'continue) (continue (removeLayer state)))
     ((eq? (operator l) '=) (stateAssign l state className return continue break exit catch))
     (else (return state))
     )))

     (define exceptionName
       (lambda (l)
         (caadr (caddr l))))


;handles return statements by exiting to the beginning of the program (using the exit continuation set at the inital call of stateDecide)
(define stateReturn
  (lambda (l state className return continue break exit catch)
    (cond
      ((eq? (getValue (cdr l) state className catch) '#t) (exit (Add 'return 'true state)))
      ((eq? (getValue (cdr l) state className catch) '#f) (exit (Add 'return 'false state)))
      (else (exit (Add 'return (getValue (cdr l) state className catch) state))))))

;handles variable declarations
(define stateDeclaration
  (lambda (l state className return continue break exit catch)
    (cond
      ((not (null? (lookupLocal (leftoperand l) (topLayer state)))) (error 'variableAlreadyDeclared))
      ((null? (cdr (cdr l))) (return (Add (leftoperand l) 'declared state)))
      (else (return (Add (leftoperand l) (getValue (rightoperand l) state className catch) state))))))

;handles variable assignments
(define stateAssign
  (lambda (l state className return continue break exit catch)
    (let ([variable (lookup (leftoperand l) state className)])
      (let ([class (lookup className state className)])
      (cond      
      ((null? variable) (error 'errorUsingBeforeDeclaringOrOutOfScope))
      ((and (not (null? (lookupLocal (leftoperand l) state))) (or (eq? variable 'declared) (atom? variable))) (return (Update (leftoperand l) (getValue (rightoperand l) state className catch) state)))
      ((or (eq? variable 'declared) (atom? variable)) (return (Update className  (cons (classParent class) (cons (Update (leftoperand l) (getValue (rightoperand l) state className catch) (classFields class)) (cons (classMethods class) (cons (classInitials class) '())))) state)))
      (else (return (Add (leftoperand l)(getValue (rightoperand l) state className catch) state))))))))

;handles if statements
(define stateIf
  (lambda (l state className return continue break exit catch)
    (cond
      ((getTruth (condition l) state className catch) 
       (decideState (ifBody l) state className return continue break exit catch))
      ((null? (elseBody l))(return state))
      (else (decideState (elseBody l) state className return continue break exit catch)))))

;handles blocks
(define stateBlock
  (lambda (l state className return continue break exit catch)
    (cond
      ((null? l) (return state))
      (else (decideState (front l) state className (lambda (v)
                                         (stateBlock (remaining l) v className return continue break exit catch))
                         (lambda (v) (return v)) break exit catch)))))

;handles while loops
(define stateWhile
  (lambda (l state className return continue break exit catch)
    (cond
      ((null? l) state)
      ((getTruth (leftoperand l) state className catch) 
       (decideState (leftoperand l) state className (lambda (v1)
                                                      (decideState (rightoperand l) v1 className (lambda (v2)
                                                                                                   (decideState l v2 className return continue break exit catch)) continue break exit catch)) continue break exit catch))
      (else (decideState (leftoperand l) state className return continue break exit catch)))))

;handles function definitions
(define stateFunction
  (lambda (l state className return continue break exit catch)
    (let ([class (lookup className state className)])
    (return (Update className (cons (classParent class) 
                  (cons (classFields class) (cons (Add (functionName l) (makeClosure (functionBody l) (functionParamList l) state) (classMethods class))
                                                  (cons (classInitials class) '())))) state)))))

;evaluate a functioncall
(define stateFunctionCall
  (lambda (l state className return catch)
    (let ([class (getClass l state className)])
      (let ([currentClassName (getCurrentClassName l state className)])
      (let ([closure (getClosure l state className)])
    (cond
      ;((eq? (lookup (functionCallName l) (classMethods class) className) '()) (error 'illegalFunctionCall))
      (else (decideState (functionClosureBody closure)
                         (copyParams (functionCallParamList l) state (functionClosureParamList closure)
                                     (addLayer (getLastN state (getFunctionClosureLayerNum closure)))
                                     currentClassName catch)
                         currentClassName
                         (lambda (v) (return state))
                         (lambda (v) (return v)) (lambda (v) (return v)) (lambda (v) (return v)) catch))))))))

(define getCurrentClassName
  (lambda (l state className)
    (cond
      ((and (list? (functionCallName l)) (eq? (leftoperand (functionCallName l)) 'super)) (classParent (lookup className state className)))
      ((list? (functionCallName l))(leftoperand (functionCallName l)))
      (else className))))
  
(define getClass
  (lambda (l state className)
    (cond
      ((and (list? (functionCallName l)) (eq? (leftoperand (functionCallName l)) 'super)) (lookupLocal (classParent (lookup className state className)) state))
      ((list? (functionCallName l))(lookupLocal (leftoperand (functionCallName l)) state))
      (else (lookup className state className)))))

(define getClosure
  (lambda (l state className)
    (cond
      ((and (list? (functionCallName l)) (eq? (leftoperand (functionCallName l)) 'super))(lookupInClassMethods (rightoperand (functionCallName l)) state (classParent (lookupLocal className state))))
      ((list? (functionCallName l))(lookupInClassMethods (rightoperand (functionCallName l)) state (leftoperand (functionCallName l))))
      (else (lookupInClassMethods (functionCallName l) state className)))))
          

;;;;;; Value

;returns the value of an expression
(define getValue
  (lambda (expression state className catch)
       (cond
         ((number? expression) expression)
         ((and (atom? expression) (eq? (lookup expression state className) 'declared)) (error 'usingBeforeAssigning))
         ((and (atom? expression) (eq? (lookup expression state className) '())) (error 'usingBeforeDeclaringOrOutOfScope))
         ((atom? expression) (lookup expression state className))
         ((and (eq? (operator expression) 'dot) (eq? (leftoperand expression) 'super)) (getValue (rightoperand expression) state (classParent (lookup className state className)) catch))
         ((eq? (operator expression) 'dot) (getValue (rightoperand expression) state (leftoperand expression) catch))
         ((eq? (operator expression) 'funcall) (lookupLocal 'return (stateFunctionCall expression state className (lambda (v) v) catch)))
         ((eq? '+ (operator expression)) (+ (getValue (leftoperand expression) state className catch)
                                            (getValue (rightoperand expression) state className catch)))
         ((eq? '/ (operator expression)) (quotient (getValue (leftoperand expression) state className catch)
                                                   (getValue (rightoperand expression) state className catch)))
         ((eq? '% (operator expression)) (remainder (getValue (leftoperand expression) state className catch)
                                                    (getValue (rightoperand expression) state className catch)))
         ((eq? '* (operator expression)) (* (getValue (leftoperand expression) state className catch)
                                            (getValue (rightoperand expression) state className catch)))
         ((and (eq? '- (operator expression))(not (null? (cdr (cdr expression)))))
          (- (getValue (leftoperand expression) state className catch)(getValue (rightoperand expression) state className catch)))
         ((eq? '- (operator expression)) (- (getValue (leftoperand expression) state className catch)))
         ((eq? '= (operator expression)) (getValue (leftoperand expression)(Update (leftoperand expression) (getValue (rightoperand expression) state className catch) state className)))
         ((eq? 'var (operator expression)) (getValue (rightoperand expression) state className catch))
         
         ((eq? '!= (operator expression))  (getTruth expression state className catch))
         ((eq? '== (operator expression))  (getTruth expression state className catch))
         ((eq? '>= (operator expression))  (getTruth expression state className catch))
         ((eq? '< (operator expression))  (getTruth expression stat classNamee catch))
         ((eq? '> (operator expression))  (getTruth expression state className catch))
         ((eq? '! (operator expression))  (getTruth expression state className catch))
         ((eq? '&& (operator expression))  (getTruth expression state className catch))
         ((eq? '|| (operator expression))  (getTruth expression state className catch))
         ((null? (cdr expression)) (getValue (operator expression) state className catch))
        (else (error 'illegalExpression)))
       ))

;evaluates boolean result of an expression
(define getTruth
  (lambda (expression state className catch)
    (cond
      ((number? expression) expression)
      ((not (pair? expression)) (lookup expression state className))
      ((eq? '< (operator expression)) (< (getValue (leftoperand expression) state className catch)
                                         (getValue (rightoperand expression) state className catch)))
      ((eq? '> (operator expression)) (> (getValue (leftoperand expression) state className catch)
                                         (getValue (rightoperand expression) state className catch)))
      ((eq? '<= (operator expression)) (<= (getValue (leftoperand expression) state className catch)
                                         (getValue (rightoperand expression) state className catch)))
      ((eq? '>= (operator expression)) (>= (getValue (leftoperand expression) state className catch)
                                         (getValue (rightoperand expression) state className catch)))
      ((eq? '== (operator expression)) (eq? (getValue (leftoperand expression) state className catch)
                                         (getValue (rightoperand expression) state className catch)))
      ((eq? '!= (operator expression)) (not(eq? (getValue (leftoperand expression) state className catch)
                                         (getValue (rightoperand expression) state className catch))))
      ((eq? '&& (operator expression)) (and (getValue (leftoperand expression) state className catch)
                                         (getValue (rightoperand expression) state className catch)))
      ((eq? '|| (operator expression)) (or (getValue (leftoperand expression) state className catch)
                                         (getValue (rightoperand expression) state className catch)))
      ((eq? '! (operator expression))  (not(getValue (leftoperand expression)state className catch)))
      ((eq? (operator expression) 'funcall) (stateFunctionCall expression state className (lambda (v) (lookup 'return v className)) catch))
      ((eq? '= (operator expression)) (getValue (leftoperand expression) (Update (leftoperand expression) (getValue (rightoperand expression) state className catch) state) className catch))
      )))

;;;;;; Function Helpers

;make a function closure to store in the environment
(define makeClosure
  (lambda (body params state)
    (cons params (cons body (cons (getNumLayers state) '())))))

;copy the actual paramters into the formal parameters
(define copyParams
  (lambda (actual state formal stateFromClosure className catch)
    (cond
      ((not (equalNumElements? actual formal)) (error 'mismatchParameters))
      ((null? actual) stateFromClosure)
      (else (copyParams (remaining actual) state (remaining formal) (Add (front formal) (getValue (front actual) state className catch) stateFromClosure) className catch)))))

;a check for equal number of parameters
(define equalNumElements?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (not (null? l2))) #f)
      ((and (null? l2) (not (null? l1))) #f)
      (else (equalNumElements? (cdr l1) (cdr l2))))))

;get the number of layers in the state to store with each function
(define getNumLayers
  (lambda (l)
    (cond
      ((null? l) 0)
      ((atom? (singleLayerTest l)) 1)
      (else (+ 1 (getNumLayers (cdr l)))))))
 
;get the last 'n' number of layers
(define getLastN
  (lambda (l1 n)
    (cond
      ((and (eq? 1 n) (atom? (singleLayerTest l1))) l1)
      (else (getLastNInner l1 '() n)))))

;getLastN helper
(define getLastNInner
  (lambda (l1 l2 n)
    (cond 
      ((null? l1) l2)
      ((and (zero? n) (null? (cdr l2))) (car l2))
      ((zero? n) l2)
      (else (getLastNInner (removeLast l1) (cons (getLast l1) l2) (- n 1))))))

;getLastNInner helper
(define getLast
  (lambda (l)
    (cond
      ((null? (cdr l)) (car l))
      (else (getLast (cdr l))))))

;getLastNInner helper
(define removeLast
  (lambda (l)
    (cond 
      ((null? (cdr l)) '())
      (else (cons (car l) (removeLast (cdr l)))))))

;;;;;; Environment

(define lookup
  (lambda (name state className)
    (cond       
      ((and (null? (lookupLocal name state)) (and (null? (lookupInClassFields name state className)) (not (null? (classParent (lookupLocal className state)))))) (lookup name state (classParent (lookupLocal className state))))
      ((null? (lookupLocal name state)) (lookupInClassFields name state className))
      (else (lookupLocal name state)))))

(define lookupInParentClass
  (lambda (name state className)
    (lookupInClassFields name state (classParent (lookupLocal className state)))))

;looks up the value of a variable handles multiple layers
(define lookupLocal
  (lambda (name state)
    (cond
      ((null? state) '())
      ((null? (car state)) '()) ;TODO
      ((atom? (singleLayerTest state))(lookup-helper name state))
      ((and (list? (singleLayerTest state)) (not (null? (lookup-helper name (variableList state))))) (lookup-helper name (variableList state)))
      (else (lookupLocal name (removeLayer state))))))

;lookup a variable's value in the layer it is given
(define lookup-helper
  (lambda (name state)
    (cond
     ((null? (variableList state)) '())
     ((eq? (firstVariable state) name) (unbox(firstValue state)))
     (else (lookup-helper name (cons (remainingVariables state) (cons(remainingValues state) '())))))))

;lookup a variable in a class
(define lookupInClassFields
  (lambda (name state className)
    (lookupLocal name (classFields (lookupLocal className state)))))

(define lookupInClassMethods
    (lambda (name state className)
      (cond 
        ((and (null? (lookupLocal name (classMethods (lookupLocal className state)))) 
              (not (null? (classParent (lookupLocal className state))))) 
         (lookupLocal name (classMethods (lookupLocal (classParent (lookupLocal className state)) state))))
        (else (lookupLocal name (classMethods (lookupLocal className state)))))))

;updates the value of a variable in the state handles multiple layers
(define Update
 (lambda (name value state)
  (cond 
     ((null? state) '())
      ((and (atom? (singleLayerTest state)) (not (null? (lookup-helper name state)))) (Update-helper name value state))
      ((and (list? (singleLayerTest state)) (not (null? (lookup-helper name (variableList state))))) (cons (Update-helper name value (variableList state)) (cdr state)))
      (else (cons (variableList state) (Update name value (removeLayer state)))))))

;updates the value of a variable in the layer it was given
(define Update-helper
  (lambda (name value state)
    (cond
      ((null? (variableList state)) '())
      ((eq? (firstVariable state) name) (begin (set-box! (firstValue state) value) state))
      (else (cons 
             (cons (firstVariable state) (variableList (Update-helper name value (cons (remainingVariables state) (cons(remainingValues state) '()))) ))
             (cons
              (cons (firstValue state) (valueList (Update-helper name value (cons (remainingVariables state) (cons(remainingValues state) '())))))
              '()))))))

;add a variable to the state handles multiple layers
(define Add
  (lambda (name value state)
    (cond 
      ((or (null? (car state))(atom? (singleLayerTest state)))(Add-helper name value state))
      ((list? (variableList state))(cons (Add-helper name value (variableList state)) (remaining state)))
      (else (Add-helper name value state)))))

;adds a variable to the layer it is given
(define Add-helper
  (lambda (name value state)
    (cond
     ((null? (variableList state))
             (cons (append (variableList state) (cons name '()))
                   (cons 
                    (append (valueList state) (cons (box value) '()))'())))
     (else (cons 
             (cons (firstVariable state) (variableList (Add-helper name value (cons (remainingVariables state) (cons(remainingValues state) '()))) ))
             (cons
             (cons (firstValue state) (valueList (Add-helper name value (cons (remainingVariables state) (cons(remainingValues state) '())))))
             '()))))))

;adds a layer to the current state
(define addLayer
  (lambda (state)
    (cond
    ((not (list? (singleLayerTest state)))(cons (initialEmpty)(cons state '())))
    (else (cons (initialEmpty) state)))))

;remove a layer from the current state
(define removeLayer cdr)

;get the top layer of the environment
(define topLayer
  (lambda (state)
    (cond
      ((null? (variableList state)) state)
      ((atom? (singleLayerTest state)) state)
      (else (variableList state)))))

;;;;;; Abstraction

(define front car)
(define remaining cdr)

;abstraction of operators     
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

;checks if the input is an atom
(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))
 
;get all but the first variable in the state
(define remainingVariables cdar)
;get all but the first associated variable in the state
(define remainingValues cdadr)
;get the first variable in the state
(define firstVariable caar)
;get the first associated value in the state
(define firstValue caadr)
;get all the current variables in the state
(define variableList car)
;get all the current values of the associated variables in the state
(define valueList cadr)

;abstractions for the function definition passed by the parser
(define functionName cadr)
(define functionBody cadddr)
(define functionParamList caddr)

;abstractions for the function closures
(define functionClosureBody cadr)
(define functionClosureParamList car)
(define getFunctionClosureLayerNum caddr)

;abstractions for the function calls
(define functionCallName cadr)
(define functionCallParamList cddr)

;abstractions for if statements
(define condition cadr)
(define ifBody caddr)
(define elseBody cdddr)

;a way to tell if the state has more than one layer
(define singleLayerTest caar)

;class closure
(define classParent car)
(define classFields cadr)
(define classMethods caddr)
(define classInitials cadddr)
 
;class parser
(define className cadr)
(define classHeader caddr)
(define classBody cadddr)

(define tryBody cadr)
(define catchBody 
  (lambda (l)
    (cond
      ((null? (caddr l)) (caddr l))
      (else  (caddr (caddr l))))))
(define finallyBody
  (lambda (l)
    (cond
      ((null? (cadddr l)) (cadddr l))
      (else (cadr (cadddr l))))))

         