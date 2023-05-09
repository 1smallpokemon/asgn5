#lang typed/racket

;; Define data structures for the abstract syntax tree (AST)
(require typed/rackunit)

(define-type ExprC (U ValC IfC LamC IdC AppC))
(define-type ValC (U NumC StrC))
(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IfC ([do? : ExprC] [test : ExprC] [else? : ExprC]) #:transparent)
(struct LamC ([args : (Listof Symbol)] [body : ExprC])#:transparent)
(struct IdC ([id : Symbol]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)


;;Value types
(define-type ValV (U CloV PrimV StrV NumV BoolV))
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env])#:transparent)
(struct PrimV ([name : Symbol] [arity : Natural])#:transparent)
(struct StrV ([val : String])#:transparent)
(struct NumV ([val : Real])#:transparent)
(struct BoolV ([val : Boolean])#:transparent)

;; Define the environment data type
(define-type Env (Listof bind))
(struct bind[(name : Symbol) (val : ValV)] #:transparent)
(define mt-env empty)

;; updated bad ID names for VVQS5
(define badsyms
  (hash
   '= #f
   'where #f
   'if #f
   'else #f
   '=> #f))


;; ValidSymbol? checks if a symbol is valid for use in the AST
(define (ValidSymbol? [sym : Symbol]) : Boolean
  (cond
    [(hash-has-key? badsyms sym) #f]
    [else #t]))

;; Define the lookup function for environments
(define (lookup [for : Symbol] [env : Env]) : ValV
  (match env
    [(list) (error 'lookup "VVQS: name not found")]
    [(cons (bind name val) rest-env)
     (if (symbol=? for name)
         val
         (lookup for rest-env))]))


;; Implement the top-interp function
(define (top-interp [prog-sexp : Sexp])
  ;; Define the top-level environment
  (define top-env
    (list (bind '+ (PrimV '+ 2))
          (bind '- (PrimV '- 2))
          (bind '* (PrimV '* 2))
          (bind '/ (PrimV '/ 2))
          (bind '<= (PrimV '<= 2))
          (bind 'equal? (PrimV 'equal? 2))
          (bind 'true (BoolV #t))
          (bind 'false (BoolV #f))
          (bind 'error (PrimV 'error 2))))
  (serialize (interp (parse prog-sexp) top-env)))

;; main VVQS parsing function
;; parse converts an S-expression into an ExprC (AST)
;; Modify the parse function according to the new ExprC definition
;; Parse an S-expression into an ExprC
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? n) (NumC n)]
    [(? symbol? (? ValidSymbol? s)) (IdC s)]
    [(? string? s) (StrC s)]
    [(list body 'if test 'else else)
     (IfC (parse body) (parse test) (parse else))]
    [(list body 'where (list (list (? symbol? (? ValidSymbol? bindings)) ':= exp) ...))
     (if (= (length bindings) (length (remove-duplicates bindings)))
           (AppC (LamC (cast bindings (Listof Symbol)) (parse body))
                 (map parse (cast exp (Listof Sexp))))
           (error 'parse "VVQS: Duplicate parameter names in function definition"))]
    [(list (list (? symbol? (? ValidSymbol? args)) ...) '=> body)
       (if (= (length args) (length (remove-duplicates args)))
           (LamC (cast args (Listof Symbol)) (parse body))
           (error 'parse "VVQS: Duplicate parameter names in function definition"))]
    [(list e ...)
     (match e
       [(cons f r) (AppC (parse f) (map parse r))])]
    [else (error 'parse "VVQS: Invalid expression")]))


(define (interp [expr : ExprC] [env : Env]) : ValV
  (match expr
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)]
    [(IfC do? test else?)
     (define test-result (interp test env))
     (match test-result
       [(BoolV #t) (interp do? env)]
       [(BoolV #f) (interp else? env)]
       [else (error 'interp "VVQS: Test expression in if must return a boolean")])]
    [(LamC args body) (CloV args body env)]
    [(IdC id)
     (lookup id env)]
    [(AppC fun args)
     (define func-val (interp fun env))
     (match func-val
       [(CloV params body closure-env)
        (if (= (length params) (length args))
            (let ([extended-env (append (map (λ (param arg) (bind (cast param Symbol) (interp (cast arg ExprC) env))) params args) closure-env)])
              (interp body extended-env))
            (error 'interp (format "VVQS: Wrong number of arguments in application")))]
       [(PrimV name arity)
        (define arg-values (map (λ (arg) (interp (cast arg ExprC) env)) args))
        (if (= arity (length arg-values))
            (apply-prim func-val arg-values env)
            (error 'interp (format "VVQS: Wrong number of arguments for primitive ~a" name)))]
       [else (error 'interp "VVQS: Attempted to apply non-function value")])]))

;; Function to extend the environment with a list of arguments and their values
(define (extend-env [env : Env] [arg-names : (Listof Symbol)] [args-val : (Listof ValV)]) : Env
  (append env (map (λ ([name : Symbol] [val : ValV]) (bind name val)) arg-names args-val)))

;; Function to check if the argument is a real number
(: check-real (Real -> Real))
(define (check-real x)
  (if (real? x)
      x
      (error (format "user-error: Expected a real number, got ~a" x))))

;; Apply a primitive operation based on its name
(: apply-prim (PrimV (Listof ValV) Env -> ValV))
(define (apply-prim prim-val args env)
  (match prim-val
    [(PrimV name arity)
     (if (= arity (length args))
         (match name
           ['+
            (match (list (first args) (second args))
              [(list (NumV a) (NumV b)) (NumV (+ a b))]
              [else (error "VVQS: Argument must be real")])]
           ['-
            (match (list (first args) (second args))
              [(list (NumV a) (NumV b)) (NumV (- a b))]
              [else (error "VVQS: Argument must be real")])]
           ['*
            (match (list (first args) (second args))
              [(list (NumV a) (NumV b)) (NumV (* a b))]
              [else (error "VVQS: Argument must be real")])]
           ['/
            (match (list (first args) (second args))
              [(list (NumV a) (NumV b))
               (if (zero? b)
                   (error "VVQS: Division by zero")
                   (NumV (/ a b)))]
              [else (error "VVQS: Argument must be real")])]
           ['<=
            (match (list (first args) (second args))
              [(list (NumV a) (NumV b)) (BoolV (<= a b))]
              [else (error "VVQS: Argument must be real")])]
           ['equal?
            (if (andmap (lambda (x) (not (or (CloV? x) (PrimV? x)))) args)
                (BoolV (equal? (serialize (first args)) (serialize (second args))))
                (BoolV #f))]
           [else (error (format "VVQS: Unknown primitive operation ~a" name))])
         (error (format "VVQS: Wrong number of arguments for ~a" name)))]
    [else (error (format "VVQS: Unknown identifier ~a" (PrimV-name prim-val)))]))


(: serialize (ValV -> String))
(define (serialize val)
  (match val
    [(CloV params body env)
     (format "<closure: params: ~a, body: ~a>" params body)]
    [(PrimV name arity)
     (format "<primitive: ~a, arity: ~a>" name arity)]
    [(StrV s) s]
    [(NumV n) (number->string n)]
    [(BoolV b) (if b "true" "false")]
    [else (error (format "VVQS: Cannot serialize value ~a" val))]))