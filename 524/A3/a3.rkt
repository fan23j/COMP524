#lang racket

(require (only-in (file "lex.rkt") lex))

;; consume terminals

(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
     (error (~a "expected token of type " type " but actual token was " token)))
    (tokens (rest (tokens)))  ; update tokens: remove first token
    token))

(define (check type)
  (if (empty? (tokens))
      #f
      (equal? type (first (first (tokens))))))

(define tokens (make-parameter '()))

;; program     := exprList
;; exprList    := expr optExprList
;; optExprList := ɛ | exprList
;; expr        := atom | invocation
;; atom        := NAME | STRING | number
;; number      := INT | FLOAT
;; invocation  := OPAREN exprList CPAREN

(define (parse code)
  (parameterize ([tokens (lex code)])
    (parse-program)))

(define (parse-program)
  (list 'program
        (parse-exprList)))

(define (parse-exprList)
  (list 'exprList
        (parse-expr)
        (parse-optExprList)))

(define (parse-optExprList)
  (if (terminate?)
      (list 'optExprList)
      (list 'optExprList (parse-exprList))))

(define (terminate?)
  (or (empty? (tokens))
      (check 'CPAREN)))

(define (parse-expr)
  (if (check 'OPAREN)
      (list 'expr (parse-invocation))
      (list 'expr (parse-atom))))

(define (parse-atom)
  (if (number?)
      (list 'atom (parse-number))
      (if (check 'NAME)
          (list 'atom (consume 'NAME))
          (list 'atom (consume 'STRING)))))

(define (number?)
  (or (check 'INT)
      (check 'FLOAT)))

(define (parse-number)
        (if (check 'INT)
          (list 'number (consume 'INT))
          (list 'number (consume 'FLOAT))))

(define (parse-invocation)
  (list 'invocation
        (consume 'OPAREN)
        (parse-exprList)
        (consume 'CPAREN)))