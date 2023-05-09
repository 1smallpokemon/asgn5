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
          (bind 'error (PrimV 'error 1))))
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

;;updated interp function to handle VVQS5, supports enviorments
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
     (define arg-values (map (λ (arg) (interp (cast arg ExprC) env)) args))
     (match func-val
       [(CloV params body closure-env)
        (if (= (length params) (length arg-values))
            (let ([extended-env (append (map (λ (param arg) (bind (cast param Symbol)
                                                                  (cast arg ValV))) params arg-values) closure-env)])
              (interp body extended-env))
            (error 'interp (format "VVQS: Wrong number of arguments in application")))]
       [(PrimV name arity)
        (if (= arity (length arg-values))
            (apply-prim func-val arg-values env)
            (error 'interp (format "VVQS: Wrong number of arguments for primitive ~a" name)))]
       [else (error 'interp "VVQS: Attempted to apply non-function value")])]))

;; Apply a primitive operation based on its name
(: apply-prim (PrimV (Listof ValV) Env -> ValV))
(define (apply-prim prim-val args env)
  (match prim-val
    [(PrimV name arity)
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
       ['error (error (format "user-error ~a" (serialize (first args))))])]))


(: serialize (ValV -> String))
(define (serialize val)
  (match val
    [(CloV params body env)
     "#<procedure>"
     #;(format "<closure: params: ~a, body: ~a>" params body)] 
    [(PrimV name arity)
     "#<primop>" 
     #;(format "<primitive: ~a, arity: ~a>" name arity)] 
    [(StrV s) s]
    [(NumV n) (number->string n)]
    [(BoolV b) (if b "true" "false")]))


;;-------------------------------TEST CASES-----------------------------------------
;; parser tests
(define concreteLam '({x y} => {+ 3 {+ x y}}))
(check-equal? (parse concreteLam)
              (LamC (list 'x 'y)
                    (AppC (IdC '+) (list (NumC 3)
                                         (AppC (IdC '+)
                                               (list (IdC 'x) (IdC 'y)))))))

(define ifTest '(("nice" if {<= x 4} else "lame") where {[x := 5]}))
(check-equal? (parse ifTest)
              (AppC (LamC '(x)
                          (IfC (StrC "nice")
                               (AppC (IdC '<=) (list (IdC 'x) (NumC 4)))
                               (StrC "lame")))
                    (list (NumC 5))))

;; parser errors
(check-exn #rx"expression" (λ () (parse '(if 4))))
(check-exn #rx"Duplicate" (λ () (parse '({x x} => {+ x x}))))
(check-exn #rx"Duplicate" (λ () (parse '({+ yer yump} where {[yer := 4] [yer := 6]}))))


;; interp tests
(define lamApp '(({x y} => {+ 3 {- x y}}) 1 2))
(check-equal? (top-interp lamApp) "2")
(check-equal? (top-interp ifTest) "lame")
(check-equal? (top-interp '(69 if {<= 5 7} else 96)) "69")

;; interp errors
(check-exn #rx"zero" (λ () (top-interp '(/ 5 (* 5 0)))))
(check-exn #rx"boolean" (λ () (top-interp '(1 if 2 else 3))))
(check-exn #rx"arguments" (λ () (top-interp '{{{x y} => {+ x 1}} 1 2 3})))
(check-exn #rx"primitive" (λ () (top-interp '(+ 1 2 3))))
(check-exn #rx"not found" (λ () (top-interp '((+ x y) 1 2 3))))
(check-exn #rx"non-function" (λ () (top-interp '(3))))


(check-exn #rx"lookup" (λ () (top-interp '({+ x y} where {[x := 5]}))))

;; Test cases for NumC
(check-equal? (top-interp '42) "42")

;; Test cases for StrC
(check-equal? (top-interp "hello") "hello")

;; Test cases for IfC
(check-equal? (top-interp '(1 if true else 2)) "1")
(check-equal? (top-interp '(1 if false else 2)) "2")

;; Test cases for LamC and AppC
(check-equal? (top-interp '(((x y) => (+ x y)) 2 3)) "5")
(check-equal? (top-interp '(((x) => (+ x 1)) 4)) "5")

;; Test cases for IdC
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp 'false) "false")
;
;; Test cases for primitive operations
(check-equal? (top-interp '(+ 2 3)) "5")
(check-equal? (top-interp '(- 7 3)) "4")
(check-equal? (top-interp '(* 4 3)) "12")
(check-equal? (top-interp '(/ 8 2)) "4")
(check-equal? (top-interp '(<= 2 3)) "true")
(check-equal? (top-interp '(<= 3 3)) "true")
(check-equal? (top-interp '(<= 4 3)) "false")
(check-equal? (top-interp '(equal? "hello" "hello")) "true")
(check-equal? (top-interp '(equal? "hello" "world")) "false")

;; Test cases for where
(check-equal? (top-interp '((+ x y) where ((x := 2) (y := 3)))) "5")
(check-equal? (top-interp '((+ x y) where ((x := 4) (y := (* 4 2))))) "12")


(check-equal? (top-interp '((((f) => ((x y) => (f x y))) ((a b) => (+ a b))) 1 2)) "3")

;; serializing PrimV and CloV
(check-equal? (serialize (PrimV '+ 4)) "#<primop>")
(check-equal? (serialize (CloV '(a b c) (IdC 'x) '())) "#<procedure>")
;(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env])#:transparent)

;; Error cases
(check-exn exn:fail?
  (lambda () (top-interp '(+ 2))))
(check-exn exn:fail?
  (lambda () (top-interp '(+ 2 3 4))))
(check-exn exn:fail?
  (lambda () (top-interp '((x y) => (+ x y 2) 3))))
(check-exn exn:fail?
  (lambda () (top-interp '((+ x y) where ((x := 2) (y := 3) (x := 4))))))

(check-exn #rx"real" (λ () (top-interp '(+ true (- false (* "fail" (/ 4 "zero")))))))
(check-exn #rx"real" (λ () (top-interp '(+ true (- false (* "fail" 4))))))
(check-exn #rx"real" (λ () (top-interp '(+ true (- false 3)))))
(check-exn #rx"real" (λ () (top-interp '(+ true 2))))
(check-exn #rx"real" (λ () (top-interp '(<= true 2))))
;(check-exn #rx"")

(define test-equal-closure
   '{equal? {{x} => {+ x 1}} {{y} => {+ y 1}}})

(check-equal? (top-interp test-equal-closure) "false")

(define test-wrong-args
   '{+ 5 10 15})

(check-exn #rx"VVQS: Wrong number of arguments for +" (λ () (top-interp test-wrong-args)))
(check-exn #rx"user-error" (λ () (top-interp '(error "hi"))))