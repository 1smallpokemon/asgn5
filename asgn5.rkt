#lang typed/racket

;; Define data structures for the abstract syntax tree (AST)
(require typed/rackunit)

(define-type ExprC (U ValV ifC LamC IdC AppC))
(define-type ValC (U NumC StringC))
(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IfC ([do? : ExprC] [test : ExprC] [else? : ExprC]) #:transparent)
(struct LamC ([args : (Listof Symbol)] [b : (ExprC)])#:transparent)
(struct IdC ([id : Symbol]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

;; updated bad ID names for VVQS5

(define badsyms
  (hash
   '= #f
   'where #f
   'if #f
   'else #f
   '=> #f))

;; Define the environment data type
(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;; ValidSymbol? checks if a symbol is valid for use in the AST
(define (ValidSymbol? [sym : Symbol]) : Boolean
  (cond
    [(hash-has-key? badsyms sym) #f]
    [else #t]))

;; Define the lookup function for environments
(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "VVQS: name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))


;; Implement the top-interp function
(: top-interp (Sexp -> Real))
(define (top-interp prog-sexp)
  (define parsed-prog (parse prog-sexp))
  (interp parsed-prog mt-env))

;; interp-fns interprets a list of FundefC and returns the result of the main function
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main (lookup-fun 'main funs))
  (define init (NumC 0))
  (define main-body-substituted (subst 'init init (FunC-body main)))
  (interp main-body-substituted funs))

;; main VVQS parsing function
;; parse converts an S-expression into an ExprC (AST)
;; Modify the parse function according to the new ExprC definition
(define (parse [expr : Sexp]) : ExprC
  (match expr
    [(? real? n) (NumC n)]
    [(list (? symbol? s) l r) (if (hash-has-key? ops s)
                                  (BinopC s (parse l) (parse r))
                                  (error 'parse "VVQS: illegal operator ~e" s))]
    [(list 'if test 'then then 'else else)
     (IfC (parse test) (parse then) (parse else))]
    [(? symbol? (? ValidSymbol? id)) (IdC id)]
    [(cons (? symbol? (? ValidSymbol? f)) r)
     (AppC (parse f) (map parse (cast r (Listof Sexp))))]))

;; parse-fundef converts an S-expression into a FundefC (function definition)
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (cons (? symbol? (? ValidSymbol? id)) arg-list) '= exp)
     (FunC id (map (λ (x) (match x [(? symbol? (? ValidSymbol? a)) a])) (cast arg-list (Listof Sexp))) (parse exp))]
    [other (error 'parse-fundef "VVQS: illegal function ~e" s)]))


;; Modify the interp function to handle environments and remove substitution
(define (interp [exp : ExprC] [env : Env]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC o l r)
     (define ops (hash ('+ +) ('- -) ('* *) ('/ /)))
     ((hash-ref ops o) (interp l env) (interp r env))]
    [(IfC test then else) (if (<= (interp test env) 0)
                              (interp then env)
                              (interp else env))]
    [(IdC id) (lookup id env)]
    [(AppC fun args)
     (define args-val (map (λ ([x : ExprC]) (interp x env)) args))
     (define new-env (extend-env env args-val))
     (interp fun new-env)]))

;; Function to extend the environment with a list of arguments and their values
(define (extend-env [env : Env] [args-val : (Listof number)]) : Env
  (append env (map (λ ([name : Symbol] [val : number]) (bind name val)) (FunC-args fundef) args-val)))

