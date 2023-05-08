#lang typed/racket

;; Define data structures for the abstract syntax tree (AST)
(require typed/rackunit)

(define-type ExprC (U ValC IfC LamC IdC AppC))
(define-type ValC (U NumC StrC BoolC))
(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct BoolC ([val : Boolean]) #:transparent)
(struct IfC ([do? : ExprC] [test : ExprC] [else? : ExprC]) #:transparent)
(struct LamC ([args : (Listof Symbol)] [b : ExprC])#:transparent)
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
;(: top-interp (Sexp -> Real))
;(define (top-interp prog-sexp)
;  (define parsed-prog (parse prog-sexp))
;  (interp parsed-prog mt-env))

;; main VVQS parsing function
;; parse converts an S-expression into an ExprC (AST)
;; Modify the parse function according to the new ExprC definition
;; Parse an S-expression into an ExprC
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? n) (NumC n)]
    [(? symbol? (? ValidSymbol? s)) (IdC s)]
    [(? string? s) (StrC s)]
    [(list body 'if test 'then else)
     (IfC (parse body) (parse test) (parse else))]
    [(list body 'where (list (list (? symbol? (? ValidSymbol? bindings)) ':= exp) ...))
     (AppC (LamC (cast bindings (Listof Symbol)) (parse body))
           (map parse (cast exp (Listof Sexp))))]
    [(list (list args ...) '=> body)
     (LamC (map (λ (x) (cast x Symbol)) args) (parse body))]
    [(list e es ...) (AppC (parse e) (map parse es))]
    [else (error 'parse "Invalid expression")]))


;(define (interp [expr : ExprC] [env : Env]) : ValC
;  (match expr
;    [(NumC n) (NumV n)]
;    [(StrC s) (StringV s)]
;    [(IfC do? test else?)
;     (match (interp test env)
;       [(BoolV #t) (interp do? env)]
;       [(BoolV #f) (interp else? env)]
;       [val (error 'interp "VVQS: non-boolean test value")])]
;    [(LamC args body) (ClosureV args body env)]
;    [(IdC id) (lookup id env)]
;    [(AppC fun args)
;     (match (interp fun env)
;       [(ClosureV arg-names body closure-env)
;        (let ([arg-vals (map (λ (arg) (interp arg env)) args)])
;          (if (= (length arg-names) (length arg-vals))
;              (interp body (extend-env closure-env arg-vals))
;              (error 'interp "VVQS: wrong number of arguments")))]
;       [(PrimitiveV prim)
;        (let ([arg-vals (map (λ (arg) (interp arg env)) args)])
;          (apply-prim prim arg-vals))]
;       [_ (error 'interp "VVQS: not a function value")])]))
;
;;; Function to extend the environment with a list of arguments and their values
;(define (extend-env [env : Env] [args-val : (Listof number)]) : Env
;  (append env (map (λ ([name : Symbol] [val : number]) (bind name val)) (FunC-args fundef) args-val)))
;
