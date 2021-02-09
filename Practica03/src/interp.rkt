#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;;Función auxiliar para sustituir un conjunto de id por un conjunto de valores
;;subst-rec WAE->list(id)->list(num)->WAE
(define (subst-rec expr li lv)
  (if (equal? (length li) 1)
      (subst expr (first li) (first lv))
      (subst-rec (subst expr (first li) (first lv)) (cdr li) (cdr lv))))

;; Recibe una extensión (expr) del lenguaje WAE,
;; un id (sub-id) y otra expresión (value).
;; Sustituye el valor sub-id por value, en expr.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sud-id value)
  (type-case WAE expr
    [id (i) (if (symbol=? i sud-id) value expr)]
    [num (n) expr]
    [op (f args) (op f (for/list ([i args]) (subst i sud-id value)))]
    [with (bindings body)
          (if (find bindings sud-id)
              (with (for/list ([i bindings]) (binding (binding-id i)(subst (binding-value i) sud-id value))) body)
              (with (for/list ([i bindings]) (binding (binding-id i)(subst (binding-value i) sud-id value))) (subst body sud-id value)))]
    [with* (bindings body)
          (parse (interp expr))]))

;;Función auxiliar para comprobar que el id no se encuentre la lista de id's del with
;;list symbol -> bool
(define (find l sub-id)
  (for/or ([s l])
      (equal? (binding-id s) sub-id))) 


;; Toma un árbol de sintáxis abstracta del lenguaje WAE
;; y lo interpreta, devolviendo el valor numérico correspondiente
;; interp: WAE -> number
(define (interp expr)
  (type-case WAE expr
    [id (i) (error "interp: Variable libre:" i)]
    [num (n) n]
    [op (f args)  (apply f (for/list ([i args]) (interp i)))]
    [with (bindings body) (interp (subst-rec body (for/list ([i bindings]) (binding-id i)) (for/list ([i bindings]) (binding-value i))))]
    [with* (bindings body) (interp (transf-with bindings body))]))

;;Función auxiliar que transforma el with* en with
;;bindings WAE -> WAE
(define (transf-with bi bo)
  (if (equal? (length bi) 1)
      (with bi bo)
      (with (list (first bi)) (transf-with (cdr bi) bo) )) )

(define (prueba e)
  (interp (parse e)))
