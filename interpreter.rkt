; Israel Hill idh
; Michael Rosenfield mer95
; Sarah Whelan slw96

; Part 4

(load "classParser.scm")
(load "lex.scm")

;main interpret function calls the main function of the class specified in the inital invocation
(define interpret
  (lambda (filename classname)
    (lookup 'return 
            (stateFunctionCall '(funcall main) (interpretClasses (parser filename) (initialEmpty)) (string->symbol classname) (lambda (v) v) (lambda (v) v))
            (string->symbol classname))))

;goes through each class (now the only thing allowed in the global context) 
(define interpretClasses
  (lambda (l state)
    (cond
      ((null? l) state)
      ((eq? (car l) 'class)(decideStateClass (classBody l)(add (className l) (makeClass l) state) (className l) (lambda (v) v) (lambda (v) (v)) (lambda (v) v) (lambda (v) v)(lambda (v) v)))
      (else (interpretClasses (cdr l) (interpretClasses (car l) state))))))

;creates a closure for each class in the style:
;'(ParentClassName((fieldNames)(fieldValues))((methodNames)(methodClosures))((initialFields)(initialFieldValues)))
;where initialFieldValues are non static variables and fieldValues are static varibales both static and non static methods are stored in the methodNames/Closures
(define decideStateClass
  (lambda (l state className return continue break exit catch)
    (let ([class (lookup className state className)])
    (cond
     ((null? l) (return state))
     ((list? (operator l)) (decideStateClass (operator l) state className (lambda (v) (decideStateClass (cdr l) v className return continue break exit catch)) continue break exit catch))
     ((eq? (operator l) 'static-function) (stateFunction l state className return continue break exit catch))
     ((eq? (operator l) 'function) (stateNonStaticFunction l state className return continue break exit catch))
     ((eq? (operator l) 'static-var)(stateStaticVariable l state className return continue break exit catch))
     ((eq? (operator l) 'var)(stateNonStaticVariable l state className return continue break exit catch))
     (else (return state))))))


;handles non static field declarations per class
(define stateNonStaticVariable
  (lambda (l state className return continue break exit catch)
    (let ([class (lookup className state className)])
    (cond
      ((not (null? (lookupLocal (leftoperand l) (topLayer state)))) (error 'variableAlreadyDeclared))
      ((null? (cdr (cdr l))) (return (Update className (cons (classParent class) (cons (classFields class) (cons (classMethods class) (cons (add (leftoperand l) 'declared (classInitials class)) '())))) state)))
      (else (return (Update className (cons (classParent class) (cons (classFields class) (cons (classMethods class) (cons (add (leftoperand l) (getValue (rightoperand l) state className catch) (classInitials class)) '())))) state)))))))

;handles static field declarations per class
(define stateStaticVariable
  (lambda (l state className return continue break exit catch)
    (let ([class (lookup className state className)])
    (cond
      ;if the variable is already in the state do not add it again cannot redefine variables
      ((not (null? (lookupLocal (leftoperand l) (topLayer state)))) (error "Variable Already Declared"))
      ;if the variable is not in the state and this is just declares (instead of declare and assign) add the variable to the state and leave it marked declared
      ((null? (cddr l)) (return (Update className (cons (classParent class) (cons (add (leftoperand l) 'declared (classFields class)) (cons (classMethods class) (cons (classInitials class) '())))) state)))
      ;if this is both the definition and the assignment add to the state the appropriate value
      (else (return (Update className (cons (classParent class) (cons (add (leftoperand l) (getValue (rightoperand l) state className catch) (classFields class)) (cons (classMethods class) (cons (classInitials class) '())))) state)))))))

(define parserOutput
  (lambda (filename)
    (parser filename)))

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
     ((and (eq? (operator l) 'funcall) (list? (leftoperand l))) (return (dotOperatorForFunctionCallsReturnsState l state className catch)))
     ((eq? (operator l) 'funcall) (stateFunctionCall l state className return catch))
     ((eq? (operator l) 'var) (stateDeclaration l state className return continue break exit catch))
     ((eq? (operator l) 'if) (stateIf l state className return continue break exit catch))
     ((eq? (operator l) 'break) (break state))
     ((eq? (operator l) 'try) (stateTry (cdr l) state className return continue break exit catch))
     ((eq? (operator l) 'throw) (catch (getValue (cadr l) state className catch)))
     ((eq? (operator l) 'begin) (stateBlock l (addLayer state) className return continue break exit catch))
     ((eq? (operator l) 'continue) (continue (removeLayer state)))
     ((eq? (operator l) '=) (stateAssign l state className return continue break exit catch))
     (else (return state))
     )))

;handles return statements by exiting to the beginning of the program (using the exit continuation set at the inital call of decideState)
(define stateReturn
  (lambda (l state className return continue break exit catch)
    (cond
      ((eq? (getValue (cdr l) state className catch) '#t) (exit (add 'return 'true state)))
      ((eq? (getValue (cdr l) state className catch) '#f) (exit (add 'return 'false state)))
      (else (exit (add 'return (getValue (cdr l) state className catch) state))))))

;handles variable declarations
(define stateDeclaration
  (lambda (l state className return continue break exit catch)
    (cond
      ((not (null? (lookupLocal (leftoperand l) (topLayer state)))) (error 'variableAlreadyDeclared))
      ((null? (cddr l)) (return (add (leftoperand l) 'declared state)))
      (else (return (add (leftoperand l) (getValue (rightoperand l) state className catch) state))))))

;handles variable assignments
(define stateAssign
  (lambda (l state className return continue break exit catch)
    (let ([variable (lookup (leftoperand l) state className)])
      (let ([class (lookup className state className)])
      (cond
       ;if the assignment has a dot operator as the left hand side decide using dotOperatorForAssignments
      ((list? (leftoperand l)) (return (dotOperatorForAssignments l state className catch)))
      ;if the variable isn't found error out
      ((null? variable) (error 'errorUsingBeforeDeclaringOrOutOfScope))
      ;if the variable is declared or assigned a value AND in the fist/top layer of the state update the value
      ((and (not (null? (lookupLocal (leftoperand l) state))) (or (eq? variable 'declared) (atom? variable)))
       (return (Update (leftoperand l) (getValue (rightoperand l) state className catch) state)))
      ;if the variable is declared or assigned a value BUT NOT in the first/top layer of the state it is a static variable
      ((or (eq? variable 'declared) (atom? variable)) 
      ;and should be updated in the classFields
       (return (Update className (cons (classParent class) 
                                       (cons (Update (leftoperand l) 
                                                     (getValue (rightoperand l) state className catch) 
                                                     (classFields class)) 
                                             (cons (classMethods class) (cons (classInitials class) '())))) state)))
      ;else it is not already in the state and should be added
      (else (return (add (leftoperand l)(getValue (rightoperand l) state className catch) state))))))))

;if an assignment contains a dot operator on the left hand side
(define dotOperatorForAssignments 
  (lambda (l state className catch)
    (let ([x (leftoperand (leftoperand l))])
      (let ([class (lookup x state className)])
        (cond
          ;and the left hand side of the dot operator is 'this' update the value of 'this' in the current layer/top of state
          ;necessary becuase once another function is called the layer for the current function typically goes away (unless it is nested) and 
          ;that means the object/this that was passed in is not in the state except in the 'this' instance in the current layer
          ((eq? x 'this) (Update 
                          'this
                          (cons (getTHISName (lookupLocal 'this state))
                                (cons 
                                 (updateInObject 
                                  (rightoperand (leftoperand l)) 
                                  (getValue (rightoperand l) state className catch) 
                                  (getTHISObject (lookupLocal 'this state))
                                  state) '()))
                                state))
          ;if the left hand side of the dot operator is an object
          ((eq? (whatIsIt? (lookupLocal x state)) 'object) 
           ;update the value in that object and update the state with the new object
           (Update x (updateInObject 
                      (rightoperand (leftoperand l)) ;the right of the dot
                      (getValue (rightoperand l) state className catch) ;the right of the assignment
                      (lookupLocal x state) ;looking up the object in the state
                      state) 
                   state))
          ;the thing to the left of the dot is a class update the static fields of the class
          (else (Update x (cons (classParent class) ;consing parent, fields, methods, and initials to recreate the class and update it in the state
                                (cons 
                                 (Update 
                                  (rightoperand (leftoperand l)) ;right of the dot
                                  (getValue (rightoperand l) state className catch) ;value of right of the assignment
                                  (classFields (lookup (leftoperand (leftoperand l)) state className))) ;updating in the static fields
                                 (cons (classMethods class) (cons (classInitials class) '())))) 
                        state)))))))
          
;the 'this variable is stored as (objectName objectClosure) where objectClousre is (objectType (instance fields))
(define getTHISName car)
(define getTHISObject cadr)

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

;handles function definitions for static functions
(define stateFunction
  (lambda (l state className return continue break exit catch)
    (let ([class (lookup className state className)])
    (return (Update className (cons (classParent class) 
                  (cons (classFields class) (cons (add (functionName l) (makeClosure (functionBody l) (functionParamList l) state) (classMethods class))
                                                  (cons (classInitials class) '())))) state)))))

;handles function definitions for non static functions - the diffence being adding a 'this reference to the parameter list in the closure
(define stateNonStaticFunction
  (lambda (l state className return continue break exit catch)
    (let ([class (lookup className state className)])
    (return (Update className (cons (classParent class) 
                  (cons (classFields class) (cons (add (functionName l) (makeClosure (functionBody l) (append (functionParamList l) (cons 'this '()) ) state) (classMethods class))
                                                  (cons (classInitials class) '())))) state)))))

;evaluate a static functioncall
(define stateFunctionCall
  (lambda (l state className return catch)
    (let ([class (getClass l state className)])
      (let ([currentClassName (getCurrentClassName l state className)])
        (let ([closure (getMethodClosure l state className)])
          (decideState (functionClosureBody closure)
                       (copyParams (functionCallParamList l) state (functionClosureParamList closure)
                                   (addLayer (getLastN state (getFunctionClosureLayerNum closure)))
                                   currentClassName catch)
                       currentClassName
                       (lambda (v) (return state))
                       (lambda (v) (return v)) (lambda (v) (return v)) (lambda (v) (return v)) catch))))))


;evaluate a non static function call - the difference being upon function end update the 'this reference to the object and store in layer of calling function
(define stateNonStaticFunctionCall
  (lambda (l state className return catch objectName)
    (let ([class (getClass l state className)])
      (let ([currentClassName (getCurrentClassName l state className)])
        (let ([closure (getMethodClosure l state className)])
          (decideState (functionClosureBody closure)
                       (copyParams (functionCallParamList l) state (functionClosureParamList closure)
                                   (addLayer (getLastN state (getFunctionClosureLayerNum closure)))
                                   currentClassName catch)
                       currentClassName
                       (lambda (v) (return (Update 
                                            (getTHISName (lookupLocal 'this v))
                                            (getTHISObject (lookupLocal 'this v))
                                            state)))
                       (lambda (v) (return v)) (lambda (v) (return v)) (lambda (v) (return v)) catch))))))

;handles the continuation changes for the try statements 
(define stateTry
  (lambda (l state className return continue break exit catch)
    ((lambda (tryState)
       (executeFinally (cdr l) tryState className return continue break exit catch))
       (executeTry l state className return continue break exit catch))))

(define executeTry
  (lambda (l state className return continue break exit catch)
    (call/cc
     (lambda (throwWithoutCatch)
       (removeLayer (stateBlock  (car l) (addLayer state) className return continue break exit 
                                 (lambda (v) (throwWithoutCatch (executeCatch v (cdr l) state className return continue break exit catch)))))))))

(define executeCatch
  (lambda (eValue l state className return continue break exit catch)
    ((lambda (newState)
       (cond
         ((null? l) state)
         ((eq? (caar l) 'catch) (removeLayer (stateBlock (caddr (car l)) newState className return continue break exit catch)))))
    (add 'e eValue (addLayer state)))))

(define executeFinally
   (lambda (l state className return continue break exit catch)
     (cond
       ((or (null? (car l)) (null? l)) (return state))
       ((eq? (caar l) 'catch) (executeFinally (cdr l) state className return continue break exit catch))
       (else (return (removeLayer (stateBlock (getFinallyBody l) (addLayer state) className return continue break exit catch)))))))

(define getFinallyBody cadar)

;adds new objects to the state
(define stateNew
  (lambda (l state className return continue break exit catch)
    (return (add (car l) (makeObject (car l) state) state))))

;makes new objects
(define makeObject
  (lambda (className state)
    (let ([parentClassName (classParent(lookup className state className))])
      (cond 
        ((null? parentClassName) (cons className (cons (makeObjectHelper className state) '())))
        (else (cons className (cons (makeObjectHelper className state) '())))))))

(define makeObjectHelper
  (lambda (className state)
    (let ([parentClassName (classParent(lookup className state className))])
      (cond
      ((null? parentClassName) (reverse (valueList (classInitials (lookup className state className)))))
      (else (append (reverse (valueList (classInitials (lookup className state className)))) (makeObjectHelper parentClassName state)))))))
                              

;gets the index of the value in the object
(define getIndex
  (lambda (a l)
    (getIndexHelp a (reverse l))))

;helper require to reverse the input and then loop
(define getIndexHelp
  (lambda (a l)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (a l return break)  
                        (cond
                          ((null? l) (break '()))
                          ((eq? (car l) a) (return 0))
                          (else (loop a (cdr l) (lambda (v) (return (+ 1 v))) break))))))
         (loop a l (lambda (v) v) (lambda (v) v)))))))

;looksup a given variable a in an object
(define lookupInObject
  (lambda (a object state)
      (cond
        ((null? (getIndex a (variableList (classInitials (lookup (classNameOfObject object) state (classNameOfObject object)))))) (lookupInClassFields a state (classNameOfObject object)))
        (else (unbox 
                  (getInstanceValue 
                   (getIndex a (variableList (classInitials (lookup (classNameOfObject object) state (classNameOfObject object))))) 
                   (valuesFromObject object)))))))

;update a given variable a to value x
(define updateInObject
  (lambda (a x object state)
                  (cons 
                   (classNameOfObject object) ;recreate the object by getting the class name and consing the new values 
                   (cons (setInstanceValue ;set one value
                          (getIndex a (variableList (classInitials (lookup (classNameOfObject object) state (classNameOfObject object)))))
                          x
                          (valuesFromObject object) (lambda (v) v)) '()))))

;gets a value at a given index n in the list of values of an object l
(define getInstanceValue
  (lambda (n l)
    (cond
      ((null? l) (error "problem"))
      ((zero? n) (car l))
      (else (getInstanceValue (- n 1) (cdr l))))))

;sets a value at a given index n to a in a list of values of an object l
(define setInstanceValue
  (lambda (n a l return)
    (cond
      ((null? l) (error "problem"))
      ((zero? n) (return (cons (box a) (cdr l))))
      (else (setInstanceValue (- n 1) a (cdr l) (lambda (v) (return (append (cons (car l)'()) v ))))))))

(define classNameOfObject car)
(define valuesFromObject cadr)
;;;;;; Value
;for use with dot operators as a way to decide what is to the left of the dot in a dot operator ie class or object
(define whatIsIt?
  (lambda (l)
    (let ([n (numElements l)])
      (cond
        ((eq? n 4) 'class)
        ((eq? n 2) 'object)))))

;the number of elements in a given list - a helper for whatIsIt? 
(define numElements
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (numElements (cdr l)))))))

;handle dot operators involving fields
(define dotOperator
  (lambda (expression state className catch)
    (cond
      ;if it is super.field return the value of the field using the className's parent
      ((eq? (leftoperand expression) 'super) (getValue (rightoperand expression) state (classParent (lookup className state className)) catch))
      ;if it is this.field return the value of the field in the instance of this in the state
      ((eq? (leftoperand expression) 'this) (lookupInObject (rightoperand expression) (getTHISObject (lookupLocal 'this state)) state))
      ;if the left hand side is an object
      ((eq? (whatIsIt? (lookup (leftoperand expression) state className)) 'object) 
       ;lookup the value in that object
       (lookupInObject (rightoperand expression) (lookup (leftoperand expression) state className) state))
      ;if the left hand side is a class lookup the value in the class
      ((eq? (whatIsIt? (lookup (leftoperand expression) state className)) 'class) 
       (getValue (rightoperand expression) state (leftoperand expression) catch)) 
      (else (getValue (rightoperand expression) state (leftoperand expression) catch)))))
   
;handles dot operators that call functions
(define dotOperatorForFunctionCallsReturnsState
  (lambda (l state className catch)
    (cond
            ;if the left hand side is super
      ((eq? (leftoperand (leftoperand l)) 'super)
       (lookupLocal 'return 
                    (stateFunctionCall (cons (operator l) (cons (rightoperand (leftoperand l)) (cddr l))) 
                                       state 
                                       ;change the class name of the function call to the class parent that is calling the function
                                       (classParent (lookupLocal className state))
                                       (lambda (v) v) catch)))
            ;the left hand side of the dot is 'this and it is calling a non static method
      ((and (eq? (leftoperand (leftoperand l)) 'this) (needsThis? l state className))
                    (stateNonStaticFunctionCall 
                     (createFunctionCallWithTHIS l state)
                     state
                     (classNameOfObject (getTHISObject (lookup (leftoperand (leftoperand l)) state className)))
                     (lambda (v) v)
                     catch
                     (getTHISName (lookupLocal 'this state))))
      ;the left hand side of the dot is 'this and it is calling a static method
      ((eq? (leftoperand (leftoperand l)) 'this)
                    (stateFunctionCall (cons (operator l) (cons (rightoperand (leftoperand l)) (cddr l))) 
                                       state 
                                       ;change the class name of the function call to the class parent that is calling the function
                                       (classNameOfObject (getTHISObject (lookupLocal 'this state)))
                                       (lambda (v) v) catch))
      ;if the left hand side is an object and it is calling a non static method create the instance of this and pass it to the function call
      ((and (eq? (whatIsIt? (lookupLocal (leftoperand (leftoperand l)) state)) 'object) (needsThis? l state className))
                 (stateNonStaticFunctionCall 
                  (createFunctionCallWithTHIS l state)
                  state
                  (classNameOfObject (lookup (leftoperand (leftoperand l)) state className))
                  (lambda (v) v)
                  catch
                  (leftoperand (leftoperand l))))
      ;if the left hand side is an object a it is calling a static method call the function normally
      ((eq? (whatIsIt? (lookupLocal (leftoperand (leftoperand l)) state)) 'object)
                 (stateFunctionCall (cons (operator l) (cons (rightoperand (leftoperand l)) (cddr l))) 
                                    state 
                                    ;change the class name of the function call to the class that is calling the function
                                    (classNameOfObject (lookup (leftoperand (leftoperand l)) state className)) 
                                    (lambda (v) v) catch))
      ;if the left hand side is a class 
      ((eq? (whatIsIt? (lookupLocal (leftoperand (leftoperand l)) state)) 'class)
       (stateFunctionCall l state className (lambda (v) v) catch)))))

;handles dot operators that call functions
(define dotOperatorForFunctionCalls
  (lambda (l state className catch)
    (cond
      ;if the left hand side is 'super and it is calling a non static method
      ((and (eq? (leftoperand (leftoperand l)) 'super) (needsThis? l state className))
       (lookupLocal 'return 
                    (stateNonStaticFunctionCall 
                     ;(cons (operator l) (cons (rightoperand (leftoperand l)) (cddr l))) 
                     ;TODO THIS LINE 
                     (cons (operator l) (cons (rightoperand (leftoperand l)) (append (cddr l) (cons (cons 'notImportant (cons (makeObject (classParent (lookupLocal className state)) state)  '())) '()))))
                     state 
                     (classParent (lookupLocal className state));change the class name of the function call to the class parent that is calling the function
                     (lambda (v) v)
                     catch
                     'notImportant)))
            ;if the left hand side is 'super and it is calling a static method
      ((eq? (leftoperand (leftoperand l)) 'super)
       (lookupLocal 'return 
                    (stateFunctionCall 
                     (cons (operator l) (cons (rightoperand (leftoperand l)) (cddr l))) 
                     state 
                     (classParent (lookupLocal className state));change the class name of the function call to the class parent that is calling the function
                     (lambda (v) v)
                     catch)))
      ;the left hand side of the dot is 'this and it is calling a non static method
      ((and (eq? (leftoperand (leftoperand l)) 'this) (needsThis? l state className))
       (lookupLocal 'return
                    (stateNonStaticFunctionCall 
                     (cons (operator l) (cons (rightoperand (leftoperand l)) (append (cddr l) (cons (cons 'this (cons (getTHISObject (lookupLocal 'this state)) '())) '()))))
                     state
                     (classNameOfObject (getTHISObject (lookup (leftoperand (leftoperand l)) state className)))
                     (lambda (v) v)
                     catch
                     'this)))
      ;the left hand side of the dot is 'this and it is calling a static method
      ((eq? (leftoperand (leftoperand l)) 'this)
       (lookupLocal 'return 
                    (stateFunctionCall (cons (operator l) (cons (rightoperand (leftoperand l)) (cddr l))) 
                                       state 
                                       ;change the class name of the function call to the class parent that is calling the function
                                       (classNameOfObject (getTHISObject (lookupLocal 'this state)))
                                       (lambda (v) v) catch)))
      ;if the left hand side is an object and it is calling a non static method create the instance of this and pass it to the function call
      ((and (eq? (whatIsIt? (lookupLocal (leftoperand (leftoperand l)) state)) 'object) (needsThis? l state className))
              (lookupLocal 'return
                 (stateNonStaticFunctionCall 
                  (createFunctionCallWithTHIS l state)
                  state
                  (classNameOfObject (lookup (leftoperand (leftoperand l)) state className))
                  (lambda (v) v)
                  catch
                  (leftoperand (leftoperand l)))))
      ;if the left hand side is an object a it is calling a static method call the function normally
      ((eq? (whatIsIt? (lookupLocal (leftoperand (leftoperand l)) state)) 'object)
       (lookupLocal 'return 
                 (stateFunctionCall (cons (operator l) (cons (rightoperand (leftoperand l)) (cddr l))) 
                                    state 
                                    ;change the class name of the function call to the class that is calling the function
                                    (classNameOfObject (lookup (leftoperand (leftoperand l)) state className)) 
                                    (lambda (v) v) catch)))
      ;if the left hand side is a class 
      ((eq? (whatIsIt? (lookupLocal (leftoperand (leftoperand l)) state)) 'class)
       (lookupLocal 'return (stateFunctionCall l state className (lambda (v) v) catch))))))

;recreates the function call string to pass
(define createFunctionCallWithTHIS
  (lambda (l state)
    (cons (operator l) ;'funcall
          (cons (rightoperand (leftoperand l)) ;function name (the righ hand side of the dot operator)
                (append (cddr l) ;the current given arguments
                        (cons 
                        ;a 'this instance (objectName (objectClassName (objectInstanceFields)))
                        (cons (leftoperand (leftoperand l)) (cons (lookupLocal (leftoperand (leftoperand l)) state) '())) '()))))))

;determines if a function has this in its list of parameters ie static or non static
(define needsThis?
  (lambda (expression state className)
    (member 'this (functionClosureParamList
                      (getMethodClosure 
                       (cons (operator expression) (cons (rightoperand (leftoperand expression)) (cddr expression))) state className)))))             

;returns the value of an expression
(define getValue
  (lambda (expression state className catch)
       (cond
         ((number? expression) expression)
         ((and (atom? expression) (eq? (lookup expression state className) 'declared)) (error 'usingBeforeAssigning))
         ((and (atom? expression) (eq? (lookup expression state className) '())) (error 'usingBeforeDeclaringOrOutOfScope))
         ((atom? expression) (lookup expression state className))
         ((eq? (operator expression) 'dot) (dotOperator expression state className catch))
         ((eq? (operator expression) 'new) (makeObject className state))
         ((and (eq? (operator expression) 'funcall) (list? (leftoperand expression))) (dotOperatorForFunctionCalls expression state className catch))
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
         ((and (eq? '= (operator expression)) (list? (leftoperand expression))) (dotOperatorForAssignment expression state className catch))
         ((eq? '= (operator expression)) (getValue (leftoperand expression)(Update (leftoperand expression) (getValue (rightoperand expression) state className catch) state) className catch))
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

;function helpers involving class

;current class name for function call
(define getCurrentClassName
  (lambda (l state className)
    (cond
      ((and (list? (functionCallName l)) (eq? (leftoperand (functionCallName l)) 'super)) (classParent (lookup className state className)))
      ((list? (functionCallName l))(leftoperand (functionCallName l)))
      (else className))))

;get class closure for function call
(define getClass
  (lambda (l state className)
    (cond
      ((and (list? (functionCallName l)) (eq? (leftoperand (functionCallName l)) 'super)) (lookupLocal (classParent (lookup className state className)) state))
      ((list? (functionCallName l))(lookupLocal (leftoperand (functionCallName l)) state))
      (else (lookup className state className)))))

;get method closure for function call
(define getMethodClosure
  (lambda (l state className)
    (cond
      ((and (list? (functionCallName l)) (eq? (leftoperand (functionCallName l)) 'super))(lookupInClassMethods (rightoperand (functionCallName l)) state (classParent (lookupLocal className state))))
      ((list? (functionCallName l))(lookupInClassMethods (rightoperand (functionCallName l)) state (leftoperand (functionCallName l))))
      (else (lookupInClassMethods (functionCallName l) state className)))))

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
      ((eq? (car formal) 'this)(copyParams (remaining actual) state (remaining formal) (add (front formal) (front actual) stateFromClosure) className catch))
      (else (copyParams (remaining actual) state (remaining formal) (add (front formal) (getValue (front actual) state className catch) stateFromClosure) className catch)))))

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

;;;;;; Class Helpers
(define makeClass
  (lambda (l)
    (cond
      ((null? (classHeader l)) (cons (classHeader l) (cons (initialEmpty) (cons (initialEmpty) (cons (initialEmpty) '())))))
      (else (cons (car (cdr (classHeader l)))  (cons (initialEmpty) (cons (initialEmpty) (cons (initialEmpty) '()))))))))

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
      ((null? (car state)) '())
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

;plain old add adds to back
(define add
  (lambda (name value state)
    (cond 
      ((or (null? (car state))(atom? (singleLayerTest state)))(add-helper name value state))
      ((list? (variableList state))(cons (add-helper name value (variableList state)) (remaining state)))
      (else (add-helper name value state)))))

;adds a variable to the layer it is given
(define add-helper
  (lambda (name value state)
    (cond
     ((null? (variableList state))
             (cons (append (variableList state) (cons name '()))
                   (cons 
                    (append (valueList state) (cons (box value) '()))'())))
     (else (cons 
             (cons (firstVariable state) (variableList (add-helper name value (cons (remainingVariables state) (cons(remainingValues state) '()))) ))
             (cons
             (cons (firstValue state) (valueList (add-helper name value (cons (remainingVariables state) (cons(remainingValues state) '())))))
             '()))))))

;add a variable to the state handles multiple layers
(define addToFront
  (lambda (name value state)
    (cond 
      ((or (null? (car state))(atom? (singleLayerTest state)))(addToFrontHelper name value state))
      ((list? (variableList state))(cons (addToFrontHelper name value (variableList state)) (remaining state)))
      (else (addToFrontHelper name value state)))))

;adds a variable to the layer it is given
(define addToFrontHelper
  (lambda (name value state)
    (cons (cons name (variableList state)) (cons (cons (box value) (valueList state)) '()))))

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

;abstractions for try/catch
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
(define exceptionName
  (lambda (l)
    (caadr (caddr l))))

;;;;;; Initial State
(define initialEmpty
  (lambda ()
    (cons '(true false) (cons (cons (box #t) (cons (box #f) '())) '()))))