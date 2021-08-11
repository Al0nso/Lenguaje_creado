#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWAE
;; (define (lookup name ds)
(define (lookup name ds)
  (cond
    [(equal? ds (mtSub)) (error "lookup: Hay un identificador libre:" name)]
    [(equal? name (aSub-name ds)) (aSub-value ds) ]
    [(id? (aSub-name ds))
     (cond
       [ (equal? (id-i (aSub-name ds)) name) (aSub-value ds)])]
    [else (lookup name (aSub-ds ds) )]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWAE DefrdSub-> CFWAE-Value
(define (interp expr ds)
  (type-case CFWBAE expr
    [id (i) (lookup i ds)]
    [bool (b) (boolV b)]
    [num (n) (numV n) ]
    [op (f args)  (let  ([i (apply f (for/list ([i args]) (numV-n (interp i ds))))])
                    (cond 
                          [(number? i) (numV i)]
                          [(boolean? i) (boolV i)]))]
    [iF (condicion then else) (cond
                                 [(boolV? (interp condicion ds))
                                  (cond
                                   [(equal? (interp condicion ds) (boolV true)) (interp then ds)]
                                   [else (interp else ds)])]
                                 [else (error "interp: parametro del iF no es un boolean: "  (interp condicion ds) ) ])]
    [fun (params body) (closure params  body ds)]
    [app (fun args) (cond
                      [(id? fun) (let ([i (lookup (id-i fun) ds)])
                                          (interp (closure-body i) (agrega_argumentos (closure-param i) (for/list ([ j args]) (interp j (closure-env i))) (closure-env i))))]
                      [else (interp (fun-body fun) (agrega_argumentos (fun-params fun) (for/list ([ j args]) (interp j ds)) ds))])]))

(define (agrega_argumentos params args ds)
  (cond
    [(equal? (length params) 1) (aSub (first params)  (first args) ds)]
    [else (agrega_argumentos (cdr params) (cdr args) (aSub (first params) (first args) ds))]))

