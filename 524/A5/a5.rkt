#lang racket

(require "a3.rkt")

;; program     := exprList
;; exprList    := expr optExprList
;; optExprList := ɛ | exprList
;; expr        := atom | invocation
;; atom        := NAME | STRING | number
;; number      := INT | FLOAT
;; invocation  := OPAREN exprList CPAREN

(define (eval source)
  ;; program     := exprList
  (let ([input (parse source)])
    (last (eval-exprList (second input)))))

(define (eval-exprList exprList-expr)
  ;; exprList    := expr optExprList
  (let* ([label (first exprList-expr)]
         [expr (eval-expr (second exprList-expr))]
         [opt (third exprList-expr)])
    (eval-optExprList expr opt)))

(define (eval-optExprList expr opt)
  ;; optExprList := ɛ | exprList
  (if (= 1 (length opt))
      (list expr)
      (cons expr (eval-exprList (second opt)))))

(define (eval-number number-expr)
  ;; number      := INT | FLOAT
  (let* ([child (second number-expr)]
         [child-label (first child)])
   (second child)))

(define (eval-atom atom-expr)
  ;; atom        := NAME | STRING | number
  (let* ([child (second atom-expr)]
         [child-label (first child)])
    (cond
      [(equal? child-label 'number) (eval-number child)]
      [(equal? child-label 'NAME) (eval-name child)]
      [else (second child)])))

(define (eval-name name-expr)
  (cond
    [(equal? (second name-expr) '+) +]
    [(equal? (second name-expr) '-) -]
    [(equal? (second name-expr) '*) *]
    [(equal? (second name-expr) '/) /]
    [(equal? (second name-expr) '=) =]
    [(equal? (second name-expr) '<) <]
    [(equal? (second name-expr) 'not) not]
    [(equal? (second name-expr) 'string=?) string=?]
    [(equal? (second name-expr) 'string<?) string<?]
    [(equal? (second name-expr) 'string-append) string-append]
    [(equal? (second name-expr) 'and) and-l]
    [(equal? (second name-expr) 'or) or-l]
    [else (error (~a "variable " (second name-expr) " not found"))]

                                    ))

(define or-l (lambda x 
    (if (null? x)
        #f
        (if (car x) (apply or-l (cdr x)) #t))))

(define and-l (lambda x 
    (if (null? x)
        #t
        (if (car x) (apply and-l (cdr x)) #f))))

(define (eval-expr expr-expr)
  ;; expr        := atom | invocation
  (let* ([child (second expr-expr)]
         [child-label (first child)])
    (cond
      [(equal? child-label 'atom) (eval-atom child)]
      [else (eval-invocation child)])))

(define (eval-invocation invocation-expr)
  ;; invocation  := OPAREN exprList CPAREN
  (let* ([label (first invocation-expr)]
         [oparen (second invocation-expr)]
         [exprList (third invocation-expr)]
         [cparen (fourth invocation-expr)]
         [proc (second exprList)])
    (apply (first (eval-exprList exprList)) (rest (eval-exprList exprList)))))

         