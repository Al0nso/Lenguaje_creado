#lang plai
(require (file "./grammars.rkt"))

;;Version 1.2

;; Funcion auxiliar para encontrar repetidos
(define (repdos ls)
  (cond
    [(empty? ls) #f]
    [(member (car ls) (cdr ls)) #t]
    [else (repdos (cdr ls))]))

;;
;;Funcion not propia
(define (noT e)
  (cond
    [(equal? e (boolS true)) (boolS false)]
    [(equal? e false) (boolS true)]
    [else e]))


;;Funcion and propia
(define (anD ls)
  (cond
    [(equal? ls '() ) (boolS true)]
    [(equal? (car ls) (boolS false)) (boolS false)]
    [(equal? (car ls) (boolS true)) (anD (cdr ls))]
    [else ls]))


;;Funcion or propia
(define (oR ls)
  (cond
    [(equal? ls '() ) (boolS true)]
    [(equal? (car ls) (boolS true)) (boolS true)]
    [(equal? (car ls) (boolS false)) (oR (cdr ls))]
    [else ls]))


;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(list? sexp)
     (cond 
       [(equal? (car sexp) '+) (opS + (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '-) (opS - (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '*) (opS * (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '/) (opS / (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) 'modulo) (opS modulo (list (parse (second sexp)) (parse (third sexp))))]
       [(equal? (car sexp) 'exp) (opS exp (list (parse (second sexp)) (parse (third sexp))))]
       [(equal? (car sexp) 'add1) (opS add1 (list (parse (second sexp))))]
       [(equal? (car sexp) 'sub1) (opS sub1 (list (parse (second sexp))))]
       [(equal? (car sexp) '<) (opS < (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '<=) (opS <= (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '=) (opS = (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) '>) (opS > (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) 'not) (noT (parse (second sexp)))]
       [(equal? (car sexp) 'and) (anD (for/list ([i (second sexp)]) (parse i)))]
       [(equal? (car sexp) 'or) (oR (for/list ([i (cdr sexp)]) (parse i)))]
       [(equal? (car sexp) 'zero?) (opS zero? (list (parse (second sexp))))]
       [(equal? (car sexp) 'with) (parse (list (list 'fun (for/list ([i (second sexp)]) (car i)) (third sexp)) (for/list ([i (second sexp)]) (second i))))] 
       [(equal? (car sexp) 'with*) (cond
                                     [(equal? (length (second sexp)) 1) (parse (list 'with (second sexp) (third sexp))) ]
                                     [else (parse (list 'with (list (car (second sexp))) (list 'with* (cdr (second sexp))  (third sexp)) )) ])]  
       [(equal? (car sexp) 'if)
        (cond
          [(equal? (length sexp) 4)(iFS (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)) )]
          [else (error "parser: Faltan argumentos para el iFS")])]
       [(equal? (car sexp) 'fun) (cond
                [(not (repdos (second sexp))) (funS (second sexp) (parse (third sexp)))]
                [ else (error "parser: parámetro definido dos veces")])]
       [(list? (first sexp))
        (cond
          [(equal? (car (first sexp)) 'fun) (cond
                                              [(equal? (length (second (car sexp))) (length (second sexp))) (appS (parse (car sexp)) (for/list ([i (second sexp)]) (parse i)) )]
                                              [else (error "parser: La cardinalidad de los argumentos difiere de la aridad de la función" (second sexp))])]
          [else (error "parser: expresión inválida")])]
       [(equal? (car sexp) 'cond)
        (cond
          [(equal? (car (car (drop sexp (- (length sexp) 1)))) 'else) (condS (for/list ([i (cdr sexp)]) (parse-cond i)))]
          [else (error "parser: Falta expresión else")])]
       [(symbol? (first sexp)) (appS (parse (first sexp)) (for/list ([ i (second sexp)]) (parse i)) ) ]
       [else  (error "parser: Similitud no encontrada" sexp)])]))

;; Toma una lista de parejas de condiciones y genera la sintáxis abstracta
;; de una condicional en CFWBAE
;; parse-cond: A -> SCFWBAE
;; parse-cond: s-expression -> SCFWBAE
(define (parse-cond cond-expr)
  (cond
    [(equal? (car cond-expr) 'else) (else-cond (parse (second cond-expr))) ]
    [else (condition (parse (first cond-expr)) (parse (second cond-expr)))]))