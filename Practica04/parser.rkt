#lang plai
(require (file "./grammars.rkt"))


;; Funcion auxiliar para encontrar repetidos
(define (repdos ls)
  (cond
    [(empty? ls) #f]
    [(member (car ls) (cdr ls)) #t]
    [else (repdos (cdr ls))]))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWAE
;; parse: s-expression -> CFWAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (car sexp)
       [(+) (op + (for/list ([i (cdr sexp)]) (parse i)))]
       [(-) (op - (for/list ([i (cdr sexp)]) (parse i)))]
       [(*) (op * (for/list ([i (cdr sexp)]) (parse i)))]
       [(/) (op / (for/list ([i (cdr sexp)]) (parse i)))]
       [(modulo) (op modulo (list (parse (second sexp)) (parse (third sexp))))]
       [(exp) (op exp (list (parse (second sexp)) (parse (third sexp))))]
       [(add1) (op add1 (list (parse (second sexp))))]
       [(sub1) (op sub1 (list (parse (second sexp))))]
       [(if0) (if0 (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)) ) ]
       [(with) (with* (for/list ([i (second sexp)]) (binding (first i) (parse (second i))))
                     (parse (third sexp)))]
       [(with*) (with* (for/list ([i (second sexp)]) (binding (first i) (parse (second i))))
                     (parse (third sexp)))] 
       [(fun) ((if (repdos (second sexp))
                   (error "parser: parámetro definido dos veces")
                   (fun ((for/list ([i (second sexp)]) (parse i)) (parse (third sexp))) )))]
       [(app) (app (parse (second sexp) (for/list ([i (third sexp)]) (parse i)) ))])]))