#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; Recibe una extensión (expr) del lenguaje WAE,
;; un id (sub-id) y otra expresión (value).
;; Sustituye el valor sub-id por value, en expr.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sud-id value)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id) val expr)]
    [num (n) expr]
    [(list? expr)
     (case (car expr)
       [(+)(op + (for/list ([i (cdr expr)] (subst i sub-id value))))]
       [(-)(op - (for/list ([i (cdr expr)] (subst i sub-id value))))]
       [(*)(op * (for/list ([i (cdr expr)] (subst i sub-id value))))]
       [(/)(op / (for/list ([i (cdr expr)] (subst i sub-id value))))]
       [(add1)(op add1 (list (subst (second expr) sub-id value) (subst (third expr) sub-id value)))]
       [(sub1)(op sub1 (list (subst (second expr) sub-id value) (subst (third expr) sub-id value)))]
       [(modulo)(op modulo (list (subst (second expr) sub-id value) (subst (third expr) sub-id value)))]
       [(expt)(op expt (list (subst (second expr) sub-id value) (subst (third expr) sub-id value)))]
       [(with)
           (if (find (second expr) sub-id)
                (with (for/list ([s (in-list (second expr))] (binding (first s) (subst (second s) sub-id value))))
                       (third expr))
                (with (for/list ([s (in-list (second expr))] (binding (first s) (subst (second s) sub-id value))))
                      (subst (third expr) sub-id value)))]
       [(with*)
           (if (find (second expr) sub-id)
                (with* (for/list ([s (in-list (second expr))] (binding (first s) (subst (second s) sub-id value))))
                       (third expr))
                (with* (for/list ([s (in-list (second expr))] (binding (first s) (subst (second s) sub-id value))))
                      (subst (third expr) sub-id value)))])]))

;;Función auxiliar para comprobar que el id no se encuentre la lista de id's del with*
;;list symbol -> bool
(define (find l id)
  (for/or ([s (in-list l)])
      (symbol=? (first s) id))) 


;; Toma un árbol de sintáxis abstracta del lenguaje WAE
;; y lo interpreta, devolviendo el valor numérico correspondiente
;; interp: WAE -> number
(define (interp expr) ...)
