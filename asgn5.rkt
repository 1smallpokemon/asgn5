#lang typed/racket

;; Define data structures for the abstract syntax tree (AST)
(require typed/rackunit)

(define-type ExprC (U NumC StrC IfC LamC IdC AppC))
(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IdC ([id : Symbol]) #:transparent)
(struct IfC ([do? : ExprC] [test : ExprC] [else? : ExprC]) #:transparent)
(struct LamC ([args : (Listof IdC)] [b : ExprC])#:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

(define-type ValC (U NumV StrV BoolV CloV))
(struct CloV ([args : (Listof IdC)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimV ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ;not sure if this is correct
(struct NumV ([n : Real]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)

;; Define the environment data type
(struct Binding ([name : IdC] [val : ValC]))
(struct Env ([bindings : (Listof Binding)]))
(define mt-env empty)
(define extend-env cons)

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

;;; returns a value for a symbol if it exists in the environment
;(define (lookup [for : symbol] [env : Env]) : ValC
;  (match env
;    ['() '()]
;    [(cons f r) (if (equal? for f)
;                    (f-val)
;                    (lookup for r))]))

;; main VVQS parsing function
;; parse converts an S-expression into an ExprC (AST)
(define (parse [expr : Sexp]) : ExprC
  (match expr
    [(? real? n) (NumC n)]
    [(? symbol? s) (IdC s)]
    [(list do? 'if test 'else else?)
     (IfC (parse do?) (parse test) (parse else?))]
    [(list args '=> body)
     (define (arg-id [syms : Sexp]) : (Listof IdC)
       (match syms
         ['() '()]
         [(cons f r) (IdC (cast f Symbol))
                     (arg-id r)]))
     (LamC (arg-id args) (parse body))]
    [(list lam args ...)
     (AppC (parse lam) (match args
                         ['() '()]
                         [(cons f r) (parse f)]))]))

;(struct LamC ([args : (Listof IdC)] [b : ExprC])#:transparent)
;(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

(define concreteLam '({x y} => {+ 3 {+ x y}}))
(check-equal? (parse concreteLam)
              (LamC (list (IdC 'x) (IdC 'y))
                    (AppC (IdC '+) (list (NumC 3)
                                         (AppC (IdC '+)
                                               (list (IdC 'x) (IdC 'y)))))))