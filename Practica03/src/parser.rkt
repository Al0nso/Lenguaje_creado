#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta WAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> WAE
;; parse: s-expression -> WAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (cond 
       [(equal? (car sexp) '+) (op + (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '-) (op - (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '*) (op * (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '/) (op / (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) 'modulo) (op modulo (list (parse (second sexp)) (parse (third sexp))))]
       [(equal? (car sexp) 'expt) (op expt (list (parse (second sexp)) (parse (third sexp))))]
       [(equal? (car sexp) 'add1) (op add1 (list (parse (second sexp))))]
       [(equal? (car sexp) 'sub1) (op sub1 (list (parse (second sexp))))]
       [(equal? (car sexp) 'with) (with (for/list ([i (second sexp)]) (binding (first i) (parse (second i))) ) (parse (third sexp)))] 
       [(equal? (car sexp) 'with*) (with* (for/list ([i (second sexp)]) (binding (first i) (parse (second i))) ) (parse (third sexp)))])]))
