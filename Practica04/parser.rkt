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
     (cond 
       [(equal? (car sexp) '+) (op + (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '-) (op - (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '*) (op * (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '/) (op / (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) 'modulo) (op modulo (list (parse (second sexp)) (parse (third sexp))))]
       [(equal? (car sexp) 'exp) (op exp (list (parse (second sexp)) (parse (third sexp))))]
       [(equal? (car sexp) 'add1) (op add1 (list (parse (second sexp))))]
       [(equal? (car sexp) 'sub1) (op sub1 (list (parse (second sexp))))]
       [(equal? (car sexp) 'if0) (if0 (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)) ) ]
       [(equal? (car sexp) 'with) (parse (list (list 'fun (for/list ([i (second sexp)]) (car i)) (third sexp)) (for/list ([i (second sexp)]) (second i))))] 
       [(equal? (car sexp) 'with*) (parse (list (list 'fun (for/list ([i (second sexp)]) (car i)) (third sexp)) (for/list ([i (second sexp)]) (second i))))]  
       [(equal? (car sexp) 'fun) (cond
                [(not (repdos (second sexp))) (fun (second sexp) (parse (third sexp)))]
                [ else (error "parser: parámetro definido dos veces")])]
       [(equal? (car (first sexp)) 'fun) (app (parse (car sexp)) (for/list ([i (second sexp)]) (parse i)) )])]))