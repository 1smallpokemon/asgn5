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

;; ValidSymbol? checks if a symbol is valid for use in the AST
(define (ValidSymbol? [sym : Symbol]) : Boolean
  (cond
    [(hash-has-key? badsyms sym) #f]
    [else #t]))

;; top-interp interprets an S-expression as a program and returns the result
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

;; interp-fns interprets a list of FundefC and returns the result of the main function
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main (lookup-fun 'main funs))
  (define init (NumC 0))
  (define main-body-substituted (subst 'init init (FunC-body main)))
  (interp main-body-substituted funs))

;; main VVQS parsing function
;; parse converts an S-expression into an ExprC (AST)
(define (parse [expr : Sexp]) : ExprC
  (match expr
    [(? real? n) (NumC n)]
    [(list (? symbol? s) l r) (if (hash-has-key? ops s)
                                  (BinopC s (parse l) (parse r))
                                  (error 'parse "VVQS: illegal operator ~e" s))]
    [(list 'leq0? test 'then then 'else else)
     (leq0? (parse test) (parse then) (parse else))]
    [(? symbol? (? ValidSymbol? id)) (IdC id)]
    [(cons (? symbol? (? ValidSymbol? f)) r)
     (FunAppC f (map parse (cast r (Listof Sexp))))]
    [other (error 'parse "VVQS: illegal expression: ~e" other)]))

;; parse-fundef converts an S-expression into a FundefC (function definition)
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (cons (? symbol? (? ValidSymbol? id)) arg-list) '= exp)
     (FunC id (map (λ (x) (match x [(? symbol? (? ValidSymbol? a)) a])) (cast arg-list (Listof Sexp))) (parse exp))]
    [other (error 'parse-fundef "VVQS: illegal function ~e" s)]))


(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC o l r)
     ((hash-ref ops o) (interp l funs) (interp r funs))]
    [(leq0? test then else) (if (<= (interp test funs) 0)
                                 (interp then funs)
                                 (interp else funs))]
    [(IdC id) (error 'interp "VVQS: unbound identifier ~e" id)]
    [(FunAppC fun args)
     (define fun-def (lookup-fun fun funs))
     (define args-val (map (λ ([x : ExprC]) (interp x funs)) args))
     (define substituted-body (subst-args (FunC-args fun-def) (map parse args-val) (FunC-body fun-def)))
     (interp substituted-body funs)]))


;; Test cases for parsing
(define a1 (BinopC '+ (NumC 1) (NumC 2)))
(define a2 (BinopC '+ (NumC 3) a1))
(define a3 (BinopC '* a1 a2))
(define sub (BinopC '- (NumC 3) (NumC 2)))
(define div (BinopC '/ (NumC 4) (NumC 2)))
(define leq0-1 (leq0? (NumC 1) a1 a2))
(define leq0-2 (leq0? (NumC -1) a1 a2))
(check-equal? (parse '(+ 1 2)) a1)
(check-equal? (parse '(+ 3 (+ 1 2))) a2)
(check-equal? (parse '(* (+ 1 2) (+ 3 (+ 1 2)))) a3)
(check-equal? (parse '(* 3 (- 1 2))) (BinopC '* (NumC 3) (BinopC '- (NumC 1) (NumC 2))))
(check-equal? (parse '(- 3 2)) sub)
(check-equal? (parse '(/ 4 2)) div)
(check-exn #rx"expression" (lambda () (parse '{+ 4})))
(check-exn #rx"operator" (lambda () (parse '{& 4 5})))
(check-exn #rx"VVQS: illegal expression" (lambda () (parse 'def)))
(check-equal? (parse '(leq0? 1 then (+ 1 2) else (+ 3 (+ 1 2)))) leq0-1)
(check-equal? (parse '(leq0? 1 then 2 else 3)) (leq0? (NumC 1) (NumC 2) (NumC 3)))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse '(f 5)) (FunAppC 'f (cons (NumC 5) '())))

;; Test cases for function parsing
(check-equal? (parse-fundef '{def {add x} = {+ x 1}}) (FunC 'add '(x) (BinopC '+ (IdC 'x) (NumC 1))))
(check-equal? (parse-fundef '{def {mul x} = {* x 2}}) (FunC 'mul '(x) (BinopC '* (IdC 'x) (NumC 2))))
(check-exn #rx"illegal function" (lambda () (parse-fundef '(def (def x) = (+ x 1)))))

;; Test cases for program parsing and interpretation
(define test-prog1
  '{{def {add x} = {+ x 1}}
    {def {mul x} = {* x 2}}
    {def {main init} = {add {mul init}}}})

(define test-prog2
  '{{def {sub x} = {- x 1}}
    {def {div x} = {/ x 2}}
    {def {main init} = {sub {div init}}}})

(check-equal? (top-interp test-prog1) 1) ; (add (mul 0)) = (add 0) = 1
(check-equal? (top-interp test-prog2) -1) ; (sub ((div 0)) = (sub 0) = -1

;; Test cases for multiple func args
(define multiple-args-test
  '((def (main) = (g 2 3 4))
    (def (g x y z) = (+ (* x y) z))))

(check-equal? (top-interp multiple-args-test) 10)

;; Test cases for conditional expressions
(define test-prog3
'{{def {negate x} = {leq0? x then {- 0 x} else {* -1 x}}}
{def {main init} = {negate {+ init 1}}}})

(check-equal? (top-interp test-prog3) -1) ; (negate (+ 0 1)) = (negate 1) = -1

;; Test case that reaches the "empty? funs" branch in lookup-fun
(define test-prog-no-main
'{{def {add x} = {+ x 1}}
{def {mul x} = {* x 2}}})

(check-exn #rx"VVQS: function not found" (lambda () (top-interp test-prog-no-main)))

;; Test case that reaches the "else (lookup-fun name (cdr funs))" branch in lookup-fun
(define test-prog-multiple-funs
'{{def {add x} = {+ x 1}}
{def {mul x} = {* x 2}}
{def {main init} = {mul init}}})

(check-equal? (top-interp test-prog-multiple-funs) 0) ; (mul 0) = 0

;; Test case to reach unbound identifier
(define test-prog-unbound-id
'{{def {main init} = unbound-id}})

(check-exn #rx"unbound identifier" (lambda () (top-interp test-prog-unbound-id)))

;; Single interp test cases (no functions)
(check-equal? (interp a1 '()) 3)
(check-equal? (interp a2 '()) 6)
(check-equal? (interp a3 '()) 18)
(check-equal? (interp sub '()) 1)
(check-equal? (interp div '()) 2)
(check-equal? (interp (NumC 0) '()) 0)
(check-exn #rx"zero" (λ () (interp (BinopC '/ (NumC 5) (NumC 0)) '() )))
(check-equal? (interp leq0-1 '()) 6)
(check-equal? (interp leq0-2 '()) 3)
