#lang racket

(define (tokn type val)
  (cons type (cons val '())))

(define (lex-s s)
  (cond [(null? s) '()]
        [(eq? (car s) #\() (cons (tokn 'lp 'lp) (lex-s (cdr s)))]
        [(eq? (car s) #\)) (cons (tokn 'rp 'rp) (lex-s (cdr s)))]
        [(eq? (car s) #\space) (lex-s (cdr s))]
        [(ident-start? (car s))
         (lex-ident (cons (car s) '()) (cdr s))]
        [(char-numeric? (car s))
         (lex-num (cons (car s) '()) (cdr s))]))

(define (lex-num acc s)
  (cond [(char-numeric? (car s)) (lex-num (cons (car s) acc) (cdr s))]
        [else (cons (tokn 'num (string->number (apply string (reverse acc))))
                    (lex-s s))]))

(define (ident-start? c)
  (or (char-alphabetic? c) (char-punctuation? c) (char-symbolic? c)))

(define (ident-in? c)
  (or (ident-start? c) (char-numeric? c)))

(define (lex-ident acc s)
  (cond [(ident-in? (car s)) (lex-ident (cons (car s) acc) (cdr s))]
        [else (cons (tokn 'ident (string->symbol (apply string (reverse acc))))
                    (lex-s s))]))

(define (reverse l)
  (letrec ([R (lambda (l col)
                        (cond [(null? l) col]
                              [else (R (cdr l) (cons (car l) col))]))])
    (R l '())))

(define (string->charlist s)
  (letrec ([S (lambda (col idx)
                (cond [(= idx -1) col]
                      [else (S (cons (string-ref s idx) col) (sub1 idx))]))])
    (S '() (sub1 (string-length s)))))

(define (lex s)
  (lex-s (string->charlist s)))

