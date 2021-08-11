#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
;; (define (desugar sexpr))
(define (desugar sexpr)
  (cond
    [(idS? sexpr) (id (idS-i sexpr))]
    [(numS? sexpr) (num (numS-n sexpr))]
    [(boolS? sexpr) (bool (boolS-b sexpr))]
    [(iFS? sexpr) (iF (desugar (iFS-condicion sexpr)) (desugar (iFS-then sexpr)) (desugar (iFS-else sexpr)))]
    [(opS? sexpr) (op (opS-f sexpr) (for/list ([i (opS-args sexpr)]) (desugar i)) )]
    [(funS? sexpr) (fun (funS-params sexpr) (desugar (funS-body sexpr)) )]
    [(appS? sexpr) (app (desugar (appS-fun sexpr)) (for/list ([i (appS-args sexpr)]) (desugar i)))]
    [(condS? sexpr) (iF (desugar (condition-test-expr (car (condS-cases sexpr)))) (desugar (condition-then-expr (car (condS-cases sexpr))))
                                                                   (cond
                                                                     [(else-cond? (car (cdr (condS-cases sexpr)))) (desugar (else-cond-else-expr (car (cdr (condS-cases sexpr)))))]
                                                                     [else (desugar (condS (cdr (condS-cases sexpr))))]) )]))
