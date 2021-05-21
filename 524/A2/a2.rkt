
#lang racket

;; The `[data #f]` is a default value for the 2nd positional argument.
;; That way, this function can take 1 arg or 2 args.
(define (token type [data #f])
  (list type data))

;;;; Token creator functions
;;;;
;;;; Given a string matching a regexp, return a token for that input or #f for
;;;; no token.

(define (skip-match str) #f)

(define (punctuation-token str)
  (token
    (case str
      [("=") 'EQUALS]
      [("+") 'PLUS]
      [("/") 'DIVIDE]
      [(";") 'SEMICOLON]
      [("(") 'OPAREN]
      [(")") 'CPAREN]
      [("{") 'OBRACE]
      [("}") 'CBRACE]
      [(".") 'PERIOD]
      [(",") 'COMMA])))

(define (int-token str)
  (token 'INT (string->number str)))

(define (float-token str)
  (token 'FLOAT (string->number str)))

(define (format-string input)
  (filter (lambda (y) (not(eq? y null))) input))

(define (string-token str)
  (token 'STRING (substring str 1 (- (string-length str) 1))))
  
(define (name-or-keyword-token str)
  (case str
    [("def" "fun" "if" "not" "and" "or")
     (token (string->symbol (string-upcase (string-trim str))))]
    [else (token 'NAME (string->symbol str))]))

(define (invalid-token str)
  (token 'INVALID str))
 
;;;; Lexing rules table
;;;;
;;;; Each item in the table is a 2-tuple (i.e. list of 2 elements):
;;;; 1. a regexp to detect a token at the beginning of a string
;;;; 2. a function (from above) to take the matched string and create a token

(define re-table
  (list
    (list #rx"^[ \r\n\t]+" skip-match) ; whitespace
    (list #rx"^//[^\n]+(\n|$)" skip-match) ; // comments
    (list #rx"^[/\\*].+\\*/(?=[\r\n\t (){},;.]|$)" skip-match) ; /* comments
    (list #rx"^[-]?[0-9]+\\.[0-9]+(?=[\r\n\t (){},;.]|$)" float-token) ; float
    (list #rx"^[-]?[0-9]+(?=[\r\n\t (){},;]|$)" int-token) ; int
    (list #rx"^[;(){},\\.]" punctuation-token) ; punctuation
    (list #rx"^\"[^\"]*\"(?=[\r\n\t (){},;.]|$)" string-token) ; string
    (list #rx"^[^0-9\"\r\n\t(){},;.][^ \"\r\n\t(){},;\\.]*(?=[\r\n\t (){},;.]|$)" name-or-keyword-token)  ; name or keyword
    (list #rx"^[0-9][^[0-9]]+" invalid-token) ; invalid token
    (list #rx"^[^\"].*\"*.*" invalid-token) ; invalid token
    (list #rx"^([^[\"]]|[;(){},\\.]).*[;(){},\\.]+.*" invalid-token) ; invalid-token
    (list #rx"^\".*\"+.*\"$" invalid-token) ; invalid-token
    ))

(define (get-matches input)
  (map (lambda (entry) (regexp-match (first entry) input))
       re-table))

(define (get-token index str)
  (case index
    [(0 1 2) (list)]
    [(3) (float-token str)]
    [(4) (int-token str)]
    [(5) (punctuation-token str)]
    [(6) (string-token str)]
    [(7) (name-or-keyword-token str)]
    [(8 9 10 11) (invalid-token str)]
    ))


(define (index-of-item items [size (sub1 (length items))])
  (letrec ([index-helper
            (lambda (n items)
              (if (empty? items)
                  null
                  (if (not (equal? #f (first items)))
                      n
                      ;(if (= n size)
                          ;8
                          (index-helper (add1 n) (rest items)))))]);)
    (index-helper 0 items)))

(define (lex-helper str)
  (if (= (string-length str) 0)
      (list)
          (cons
           (get-token
            (index-of-item (get-matches str))
            (list-ref (list-ref (get-matches str) (index-of-item (get-matches str))) 0 ))
           (lex-helper (string-trim str (list-ref (list-ref (get-matches str) (index-of-item (get-matches str))) 0) #:right? #f)))))

(define (delete-null input)
  (filter (lambda (y) (not(eq? y null))) input))

(define (lex str)
  (delete-null (lex-helper str)))