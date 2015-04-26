; Programming Language Concepts
; Interpreter Part 4

(load "classParser.scm")


;***********************************************
; *********  Abstraction Functions  ***********
;***********************************************
; Using abstraction to hide details 

;in this case, we hide the infix/prefix/postfix form
(define prefix (list car cadr caddr))

; provides the state a program starts in
(define initialstate '((()())))

; provides an empty environment
(define empty-environment '(()()))

; given an operation and an expression returns if it is unary
(define unary?
  (lambda (op expression)
    (and (eq? op (operator expression)) (null? (cddr expression)))))


; the helper functions to determine where the operator and operands are depending on the form
(define operator 
  (lambda (form)
    (car form)))

(define leftoperand 
  (lambda (form)
    (cadr form)))

(define rightoperand
  (lambda (form)
    (caddr form)))

; abstractions for retrieving left and right expressions of a statement
(define leftexpression
  (lambda (stmt)
    (cadr stmt)))

(define rightexpression
  (lambda (stmt)
    (if (eq? (cddr stmt) '())
        '()
        (caddr stmt))))
     
; abstractions for parsing input
(define nextline
  (lambda (l)
    (car l)))

(define nextstmttype
  (lambda (l)
    (car (nextline l))))

(define nextstmt
  (lambda (l)
    (cdr (nextline l))))
  
(define remainingprogram
  (lambda (l)
    (cdr l)))

; abstractions for parsing through the state         
(define nextvar
  (lambda (layer)
    (car (car layer))))

(define nextvalue
  (lambda (layer)
    (car (cadr layer))))

(define cdr-vars
  (lambda (layer)
         (cdr (car layer))))

(define cdr-values
  (lambda (layer)
         (cdr (cadr layer))))

(define cdr-layer
  (lambda (layer)
    (cons (cdr-vars layer) (list (cdr-values layer)))))

; abstractions for loop statements
(define condition
  (lambda (l)
    (car l)))

(define body
  (lambda (l)
    (cadr l)))

; abstractions for parsing function declarations
(define functionname
  (lambda (functiondeclaration)
    (cadr functiondeclaration)))

(define functionarguments
  (lambda (functiondeclaration)
    (caddr functiondeclaration)))

(define functionbody
  (lambda (functiondeclaration)
    (cadddr functiondeclaration)))

; abstractions for try-catch blocks
(define finally-body
  (lambda (finally-block)
    (cadr (car finally-block))))

(define catch-body
  (lambda (catch-block)
    (caddr (car catch-block))))


;***********************************************
; *********  Interpreter Functions  ***********
;***********************************************
; Highest level of functions. Takes in input from file and parses each line of that input.

; Given a file name and the class from which to run the main method interprets the contents of the file
(define interpret
  (lambda (filename main-class)
    ((lambda (environment class)
       (Mvalue-function-call 'main '() (generatestate environment class) environment class (lambda (v) (error 'throw-without-try-block))))
     (create-environment (parser filename) empty-environment) (string->symbol main-class))))

; Sets up state to call main method in
(define setupstate
  (lambda (l state)
    (cond
      ((null? l) state)
      ((contains? '(function var) (nextstmttype l)) (setupstate (remainingprogram l) (Mstate-nocont (nextline l) state (lambda (v) (error 'throw-without-try-block)))))
      (else (error 'invalid-stmt-type)))))

; given a class name and an environment, creates a state for all variables and functions
; TODO: make work with non-static stuff
(define generatestate
  (lambda (environment class)
    ((lambda (class-closure)
       (list (list (get-static-vars environment class-closure) (reverse (get-static-values environment class-closure)))))
     (getbox class (list environment)))))
    
; given an environment and class-closure, builds a list of all variable names including of classes that it extends
(define get-static-vars
  (lambda (environment class-closure)
    ((lambda (static-vars)
       (if (null? (car class-closure))
           static-vars
           (append static-vars (get-static-vars environment (getbox (car class-closure) (list environment))))))
     (car (cadr class-closure)))))


(define get-static-values
  (lambda (environment class-closure)
    ((lambda (static-values)
       (if (null? (car class-closure))
           static-values
           (append (get-static-values environment (getbox (car class-closure) (list environment))) static-values)))
     (cadr (cadr class-closure)))))
    

; Given interprets a block of the parsed code.  Any calls to this function from outside should add a layer to the environment
; needs outerstate for functions, when you call a function the state gets changed but the state you want to return so the program
;   can continue is the origional, or outer, state
;   think of 
; def bar(){

; }
; def main() {
;  
; }
; def foo() {

; }
; if you interpret main's block, the state will not include main. But after you are finished interpreting main the state should again include foo
(define execute-body
  (lambda (l state outerstate environment class return break continue throw)
    (cond
      ((null? l) outerstate)
      ((contains? '(var = if while begin try break continue return throw funcall function) (nextstmttype l)) (execute-body (remainingprogram l) (Mstate (nextline l) state environment class return break continue throw) outerstate environment class return break continue throw))
      (else (error 'bad-stmt-type)))))


;***********************************************
; *********  Environment Functions  ***********
;***********************************************
(define create-environment
  (lambda (l environment)
    (if (null? l)
        environment
        (create-environment (cdr l) (add-class (car l) environment)))))

(define classname
  (lambda (class)
    (cadr class)))

(define add-class
  (lambda (class environment)
    (if (eq? (car class) 'class)
        (list (cons (classname class) (car environment)) (cons (create-class-closure class) (cadr environment)))
        (error 'unknown-declarer))))
    

(define class-code
  (lambda (class)
    (cadddr class)))

(define new-class-closure
  (lambda ()
      '((() ()) (() ()) (() ()))))

(define create-class-closure
  (lambda (class)
    (cons (parent class) (create-class-states (class-code class) (new-class-closure)))))

(define create-class-states
  (lambda (class-code class-closure)
    (cond
      ((null? class-code) class-closure)
      ((eq? (caar class-code) 'static-var) (create-class-states (cdr class-code) (add-static-var (cdar class-code) class-closure)))
      ((eq? (caar class-code) 'static-function) (create-class-states (cdr class-code) (add-static-function (car class-code) class-closure)))
      ((eq? (caar class-code) 'non-static-var) (error 'not-yet-available))
      ((eq? (caar class-code) 'non-static-function) (error 'not-yet-availalbe))
      (else (error 'unknow-reserved-word-in-class)))))

(define add-static-var
  (lambda (var-dec class-closure)
    (cons (list (cons (car var-dec) (caar class-closure)) (append (cadar class-closure) (list (box (static-var-value var-dec))))) (cdr class-closure))))

(define static-var-value
  (lambda (var-dec)
    (if (null? (cdr var-dec))
        '()
        (cadr var-dec))))

(define add-static-function
  (lambda (func-dec class-closure)
    (cons (list (cons (functionname func-dec) (caar class-closure)) (append (cadar class-closure) (list (box (list (functionarguments func-dec) (functionbody func-dec)))))) (cdr class-closure))))

(define parent
  (lambda (class)
    ((lambda (parent-list)
      (cond
        ((null? parent-list) '())
        ((eq? (car parent-list) 'extends) (cadr parent-list))
        (else (error 'unknown-class-modifier))))
    (caddr class))))
    

;*****************************************
; *********  State Functions  ***********
;*****************************************
; Given a statement and a state returns that state altered by that statement

; Generic Mstate returns the correct state after the statement has been executed
(define Mstate
  (lambda (stmt state environment class return break continue throw)
    (cond
      ((not (pair? stmt)) state)
      ((eq? (car stmt) 'try) (Mstate-try (cdr stmt) state environment class return break continue throw))
      ((eq? (car stmt) 'if) (Mstate-cond (cdr stmt) state environment class return break continue throw))
      ((eq? (car stmt) 'while) (Mstate-loop (cdr stmt) state environment class return throw))
      ((eq? (car stmt) 'begin) (Mstate-begin (cdr stmt) state environment class return break continue throw))
      ((eq? (car stmt) 'break) (break (removelayer state)))
      ((eq? (car stmt) 'continue) (continue (removelayer state)))
      ((eq? (car stmt) 'return) (return (convertbool (Mvalue (cadr stmt) state environment class throw)))) 
      ((eq? (car stmt) 'throw) (throw (Mvalue (cadr stmt) state environment class throw)))
      (else (Mstate-nocont stmt state environment class throw)))))

; an Mstate for when we know we wont see return break or continue
(define Mstate-nocont
  (lambda (stmt state environment class throw)
    (cond
      ((not (pair? stmt)) state)
      ((eq? (car stmt) 'function) (Mstate-function-create (functionname stmt) (functionarguments stmt) (functionbody stmt) state))
      ((eq? (car stmt) 'funcall) (Mstate-function-call (functionname stmt) (cddr stmt) state environment class throw)) ; for foo();
      ((eq? (car stmt) 'var) (Mstate-decl (cdr stmt) state environment class throw))
      ((eq? (car stmt) '=) (Mstate-assign (cdr stmt) state environment class throw))
      ;TODO: is this necessary? ((eq? (car stmt) 'return) (Mstate-assign stmt state))
      (else (Mstate-nocont (rightexpression stmt) (Mstate-nocont (leftexpression stmt) state environment class throw) environment class throw))))) ;For side effects of expressions

; Given information about a function, creates a closure
(define Mstate-function-create
  (lambda (name arguments body state)
    (add name (list arguments body) #t state)))

; Executes the code from a function while returning a state instead of a value
(define Mstate-function-call
  (lambda (name arguments state environment class throw)
    (call/cc
     (lambda (return)
       ((lambda (closure)
          (cond
            ((not (list? name)) (execute-body (cadr closure) (create-state name (car closure) arguments (addlayer (get-closure-state name state)) state environment class throw) state environment class (lambda (v) (return state)) 
                                              (lambda (v) (error 'break-outside-switch-or-loop)) (lambda (v) (error 'continue-outside-of-loop)) throw))
            ((eq? (car name) 'dot) (execute-body (cadr closure) (create-state (caddr name) (car closure) arguments (generatestate environment (get-class-name (cadr name) class environment)) state environment class throw) state environment (get-class-name (cadr name) class environment) (lambda (v) (return state)) 
                                                 (lambda (v) (error 'break-outside-switch-or-loop)) (lambda (v) (error 'continue-outside-of-loop)) throw))
            (else (error 'unsuported-function-name))))
        (Mvalue name state environment class throw))))))

; given a loop, continuously exectues the body until the condition is no longer true
; doesnt take break or continue because its a new loop, thus outer versions are irrelavent
(define Mstate-loop
  (lambda (loop-stmt initialstate environment class return throw)
    ; Returns to this point with a state when break is called from within the loop
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (loopstate)
                        (if (Mvalue (condition loop-stmt) loopstate environment class throw)
                            (loop
                             ; If we call continue, we make another call to loop on the state passed from the continuation
                             (call/cc
                                   (lambda (continue)
                                     (Mstate (body loop-stmt) loopstate environment class return break continue throw))))
                            loopstate))))
         (loop initialstate))))))

; given a declare expression, will add the variable to the state
; or can evaluate an expression and add it to the state
(define Mstate-decl 
  (lambda (decl-stmt state environment class throw)
    (cond
      ((lookup (car decl-stmt) (list (car state))) (error 'redefining-variable))
      ((null? (cdr decl-stmt)) (add (car decl-stmt) '() #t state))
      (else (Mstate-assign decl-stmt (Mstate-decl (cons (car decl-stmt) '()) state environment class throw) environment class throw)))))

; given an assignment and a state, assigns the evaluated expression to the given variable
(define Mstate-assign
  (lambda (assign-stmt state environment class throw)
    (if (lookup (car assign-stmt) state)
        (changevalue (car assign-stmt) (Mvalue (cadr assign-stmt) state environment class throw) state)
        (error 'undeclared-var))))

; given an if statement and a state, executes the proper statement given the value of the condition
(define Mstate-cond
  (lambda (stmt state environment class return break continue throw)
    (cond
      ((null? stmt) state)
      ((Mvalue (condition stmt) state environment class throw) (Mstate (body stmt) state environment class return break continue throw))
      ((pair? (cddr stmt)) (Mstate (caddr stmt) state environment class return break continue throw))
      (else (Mstate-nocont (condition stmt) state environment class throw)))))

; given a begin statement and a state adds a layer for the statements the follow begin
(define Mstate-begin
  (lambda (stmts state environment class return break continue throw)
    (removelayer (execute-body stmts (addlayer state) (addlayer state) environment class return break continue throw)))) ; find the state with the added layer, then remove the layer

; Returns state caused by a try-catch block
(define Mstate-try
  (lambda (stmt state return environment class break continue throw)
    ((lambda (try-state)
       (execute-finally (cdr stmt) try-state environment class return break continue throw))
     (execute-try stmt state environment class return break continue throw))))

; Executes the a try block and creates the call/cc to return to when a throw occurs
(define execute-try
  (lambda (stmt state environment class return break continue throw)
    (call/cc
     (lambda (throw-without-catch)
       (removelayer (execute-body (car stmt) (addlayer state) (addlayer state) environment class return break continue 
                                  (lambda (v) (throw-without-catch (execute-catch v (cdr stmt) state environment class return break continue throw)))))))))

; Adds a catch block to a throw call/cc if a catch exists
(define execute-catch
  (lambda (e-value body state environment class return break continue throw)
    ((lambda (new-state)
      (cond
        ((null? body) state)
        ((eq? (nextstmttype body) 'catch) (removelayer (execute-body (catch-body body) new-state new-state environment class return break continue throw)))))
     (add 'e e-value #t (addlayer state)))))

; Creates a function which executes a finally block on the state returned by either a catch or a try block
(define execute-finally
  (lambda (body state environment class return break continue throw)
    (cond
      ((or (null? (nextline body)) (null? body)) state)
      ((eq? (nextstmttype body) 'catch) (execute-finally (cdr body) state environment class return break continue throw))
      (else (removelayer (execute-body (finally-body body) (addlayer state) (addlayer state) environment class return break continue throw))))))

; Creates the state a function is to be run in
(define get-closure-state
  (lambda (state-name state)
    (letrec ((loop (lambda (layer)
                     (cond
                       ((and (null? (cdr state))(null? (getvars layer))) (error 'function-does-not-exist))
                       ((null? (getvars layer)) (get-closure-state state-name (removelayer state)))
                       ((eq? (nextvar layer) state-name) (cons (list (car layer) (cadr layer)) (removelayer state)))
                       (else (loop (cdr-layer layer)))))))
      (loop (nextlayer state)))))

; Takes a function-name and the parameters and returns the appropriate state which has the params added
(define create-state
  (lambda (func-name formal actual state outerstate environment class throw)
    (addargs formal actual state outerstate environment class throw)))

; Given a list of formal and actual parameters adds their bindings to state
(define addargs
  (lambda (formal actual state outerstate environment class throw)
    (cond
      ((and (null? formal) (null? actual)) state)
      ((or (null? formal) (null? actual)) (error 'incorrect-number-of-arguments-to-function))
      ((eq? (car formal) '&) (addargs (cddr formal) (cdr actual) (add (cadr formal) (getbox (car actual) outerstate) #f state) outerstate environment class throw))
      (else (addargs (cdr formal) (cdr actual) (add (car formal) (Mvalue (car actual) outerstate environment class throw) #t state) outerstate environment class throw)))))
    

;*****************************************
; *********  Value Functions  ***********
;*****************************************
; Given an expression and a state returns the value of that expression

; given an expression possibly containing variables and a state returns the value of it evaluated
(define Mvalue
  (lambda (expression state environment class throw)
    ((lambda (operator leftoperand rightoperand)
       (cond
         ((eq? expression 'true) #t)
         ((eq? expression 'false) #f)
         ((number? expression) expression)
         ((boolean? expression) expression)
         ; if its a var evaluate it
         ((symbol? expression) (Mvalue (Mvar expression state) state environment class throw)) ; wrap in an Mvalue to evaluate unevaluated expressions
         ((eq? (car expression) 'funcall) (Mvalue-function-call (functionname expression) (cddr expression) state environment class throw))
         ((eq? (car expression) 'dot) (Mvalue (Mvalue-dot (cadr expression) (caddr expression) class environment) state environment class throw)) ; wrap in an Mvalue to evaluate unevaluated expressions
         ; it needs to go to number
         ((contains? '(+ - * / %) (operator expression)) (Mnum expression state environment class throw))
         ; it needs to go to boolean
         ((contains? '(== != < > <= >= && || !) (operator expression)) (Mbool expression state environment class throw))
         ((eq? '= (operator expression)) (Mvalue (leftoperand expression) (Mstate-assign (cdr expression) state environment class throw) environment class throw))
         ((list? (car expression)) expression) ; if it is a function closure, just return the function closure
         (else (error 'bad-operator)))
       )
     (operator prefix) (leftoperand prefix) (rightoperand prefix))))

; M_bool takes a boolean expression and a state and returns either #t or #f
(define Mbool
  (lambda (expression state environment class throw)
    ((lambda (operator leftoperand rightoperand)
       (cond
         ((eq? '&& (operator expression)) (and (Mvalue (leftoperand expression) state environment class throw)
                                            (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '|| (operator expression)) (or (Mvalue (leftoperand expression) state environment class throw)
                                                   (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '! (operator expression)) (not (Mvalue (leftoperand expression) state environment class throw)))
         ((eq? '== (operator expression)) (eq? (Mvalue (leftoperand expression) state environment class throw)
                                            (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '!= (operator expression)) (neq? (Mvalue (leftoperand expression) state environment class throw)
                                            (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '< (operator expression)) (< (Mvalue (leftoperand expression) state environment class throw)
                                            (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '> (operator expression)) (> (Mvalue (leftoperand expression) state environment class throw)
                                            (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '<= (operator expression)) (<= (Mvalue (leftoperand expression) state environment class throw)
                                            (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '>= (operator expression)) (>= (Mvalue (leftoperand expression) state environment class throw)
                                            (Mvalue (rightoperand expression) state environment class throw )))
         (else (error 'bad-operator)))
       )
     (operator prefix) (leftoperand prefix) (rightoperand prefix))))

; Calls a function and returns the state after the call
; take a compile time class
(define Mvalue-function-call
  (lambda (name arguments state environment class throw)
    (call/cc
     (lambda (return)
       ((lambda (closure)
          (cond 
            ((not (list? name)) (execute-body (cadr closure) (create-state name (car closure) arguments (addlayer (get-closure-state name state)) state environment class throw) state environment class return 
                                              (lambda (v) (error 'break-outside-switch-or-loop)) (lambda (v) (error 'continue-outside-of-loop)) throw))
            ((eq? (car name) 'dot) (execute-body (cadr closure) (create-state (caddr name) (car closure) arguments (generatestate environment (get-class-name (cadr name) class environment)) state environment class throw) state environment (get-class-name (cadr name) class environment) return 
                                                 (lambda (v) (error 'break-outside-switch-or-loop)) (lambda (v) (error 'continue-outside-of-loop)) throw))
            (else (error 'unsuported-function-name))))
        (getfunction (getname name) (getfunstate name state environment) environment class throw (length arguments)))))))

(define getname
  (lambda (name)
    (if (list? name)
        (caddr name)
        name)))

(define getfunstate
  (lambda (name state environment)
    (if (list? name)
        (generatestate environment (cadr name))
        state)))

(define getfunction
  (lambda (name state environment class throw num-args)        
    ((lambda (closure)
       (cond 
         ((eq? (length (car closure)) num-args) closure)
         ((null? (getvars (nextlayer state))) (getfunction name (removelayer state) environment class throw num-args))
         (else (getfunction name (cons (list (cdaar state) (cdadar state)) (cdr state)) environment class throw num-args))))
       (Mvar name state))))
  

; given the left operator returns the appropriate class name
(define get-class-name
  (lambda (name currentclass environment)
    (cond
      ((eq? name 'super) (car (getbox currentclass (list environment))))
      ((eq? name 'this) currentclass)
      (else name))))

; M_value takes an expression, a state, and a form that can be infix/prefix/postfix
(define Mnum
  (lambda (expression state environment class throw)
    ((lambda (operator leftoperand rightoperand)
       (cond
         ((unary? '+ expression) (Mvalue (leftoperand expression) state environment class throw))
         ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state environment class throw)
                                            (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state environment class throw)
                                                   (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state environment class throw)
                                                    (Mvalue (rightoperand expression) state environment class throw)))
         ((unary? '- expression) (- (Mvalue (leftoperand expression) state environment class throw)))
         ((eq? '- (operator expression)) (- (Mvalue (leftoperand expression) state environment class throw) 
                                            (Mvalue (rightoperand expression) state environment class throw)))
         ((eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state environment class throw) 
                                            (Mvalue (rightoperand expression) state environment class throw)))
        (else (error 'bad-operator)))
       )
     (operator prefix) (leftoperand prefix) (rightoperand prefix))))



; Mvar (var state)
; returns the value of the variable given in the given state
(define Mvar
  (lambda (var state)
    (unbox (getbox var state))))

(define getbox
  (lambda (var state)
    (letrec ((loop (lambda (layer)
                     (cond
                       ((and (null? (cdr state))(null? (getvars layer))) (error 'variable-not-found))
                       ((null? (getvars layer)) (getbox var (removelayer state)))
                       ((and (eq? var (nextvar layer)) (eq? '() (nextvalue layer))) (error 'var-is-null))
                       ((eq? (nextvar layer) var) (nextvalue layer))
                       (else (loop (cdr-layer layer)))))))
      (loop (nextlayer state)))))

(define Mvalue-dot
  (lambda (left right class environment)
    (cond
      ((eq? left 'super) (get-class-value right (car (getbox class (list environment))) environment))
      ((eq? left 'this) (error 'no-this-yet)) ; for part 5
      (else (get-class-value right left environment)))))

; given a variable name, a class, and an environment finds the variables value within the environment
(define get-class-value
  (lambda (name class environment)
    (unbox (getbox name (generatestate environment class)))))


;*************************************************
; *********  State Editing Functions  ***********
;*************************************************
; State is of the form ((a1 a2 a2...) (value1 value2 value3...))

; Addlayer (state)
(define addlayer
  (lambda (state)
    (cons '(()()) state)))

;Returns the state with the last layer removed
(define removelayer cdr)

;Returns next layer in given state
(define nextlayer car)

; Returns the vars of a layer
(define getvars car)

; Returns the values of a layer
(define getvalues cadr)
             
; Changes the value of the given variable to the given value
(define changevalue
  (lambda (var value state)
    (begin (set-box! (getbox var state) value) state)))

; Add (var value needsbox state)
; adds the given var to the state, pairing that var with the given value and if it's not already inside a box, boxes it
(define add
  (lambda (var value needsbox state)
    ((lambda (currentlayer)
      (cons (list (cons var (getvars currentlayer)) (cons (boxifneeded value needsbox) (getvalues currentlayer))) (removelayer state)))
     (nextlayer state))))

; Given a value, puts it in a box if needsbox is true
(define boxifneeded
  (lambda (value needsbox)
    (if needsbox
        (box value)
        value)))

; Lookup (var state)
; returns true if the variable has been declared, otherwise returns false
(define lookup
  (lambda (var state)
    (letrec ((loop (lambda (layer)
                     (cond
                       ((and (null? (cdr state))(null? (getvars layer))) #f)
                       ((null? (getvars layer)) (lookup var (removelayer state)))
                       ((eq? (nextvar layer) var) #t)
                       (else (loop (cdr-layer layer)))))))
      (loop (nextlayer state)))))

;*******************************************
; *********  Utility Functions  ***********
;*******************************************

; Tells if two things are not equal
(define neq?
  (lambda (a1 a2)
    (not (eq? a1 a2))))

; If returning a boolean, convert it to the actual word true/false
(define convertbool
  (lambda (v)
    (cond
      ((eq? v #t) 'true)
      ((eq? v #f) 'false)
      (else v))))

; Given a list and an atom, returns #t if the atom is in the list
(define contains?
  (lambda (l v)
    (cond
      ((null? l) #f)
      ((eq? (car l) v) #t)
      (else (contains? (cdr l) v)))))
