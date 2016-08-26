#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l)) (lat? (cdr l))]
        [else #f]))

(define (rember a lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) a) (cdr lat)]
        [else (cons (car lat) (rember a (cdr lat)))]))

(define (firsts l)
  (cond [(null? l) '()]
        [else (cons (car (car l))
                    (firsts (cdr l)))]))

(define (insertR new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) (cons old
                                   (cons new
                                         (cdr lat)))]
        [else (cons (car lat) (insertR new old (cdr lat)))]))

(define (o+ n m)
  (cond [(zero? m) n]
        [else (add1 (o+ n (sub1 m)))]))

(define (numbered? aexp)
  (cond [(atom? aexp) (number? aexp)]
        [(eq? (car (cdr aexp)) '+) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
        [(eq? (car (cdr aexp)) '*) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
        [(eq? (car (cdr aexp)) '**) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]))

(define (** n m)
  (cond [(= m 1) n]
        [else (* n (** n (sub1 m)))]))

;; (define (value aexp)
;;   (cond [(atom? aexp) aexp]
;;         [(eq? (car (cdr aexp)) '+) (+ (value (car aexp)) (value (car (cdr (cdr aexp)))))]
;;         [(eq? (car (cdr aexp)) '*) (* (value (car aexp)) (value (car (cdr (cdr aexp)))))]
;;         [(eq? (car (cdr aexp)) '**) (** (value (car aexp)) (value (car (cdr (cdr aexp)))))]))

(define (member? a lat)
  (cond [(null? lat) #f]
        [else (cond [(equal? (car lat) a) #t]
                    [else (member? a (cdr lat))])]))

(define (set? lat)
  (cond [(null? lat) #t]
        [else (cond [(member? (car lat) (cdr lat)) #f]
                    [else (set? (cdr lat))])]))

(define (remove a lat)
  (cond [(null? lat) '()]
        [(equal? (car lat) a) (remove a (cdr lat))]
        [else (cons (car lat) (remove a (cdr lat)))]))

(define (makeset lat)
  (cond [(null? lat) '()]
        [else (cons (car lat)
                    (makeset (remove (car lat) (cdr lat))))]))

(define (multirember&co a lat col)
  (cond [(null? lat) (col '() '())]
        [(eq? (car lat) a)
         (multirember&co a
                         (cdr lat)
                         (lambda (newlat seen)
                           (col newlat (cons (car lat) seen))))]
        [else
         (multirember&co a
                         (cdr lat)
                         (lambda (newlat seen)
                           (col (cons (car lat) newlat)
                                seen)))]))

(define (evens-only*&co l col)
  (cond [(null? l) (col '() 1 0)]
        [(atom? (car l))
         (cond [(even? (car l))
                (evens-only*&co (cdr l)
                                (lambda (newl p s)
                                  (col (cons (car l) newl) (* p (car l)) s)))]
               [else
                (evens-only*&co (cdr l)
                                (lambda (newl p s)
                                  (col newl p (+ s (car l)))))])]
        [else (evens-only*&co (car l)
                              (lambda (al ap as)
                                (evens-only*&co (cdr l)
                                                (lambda (dl dp ds)
                                                  (col (cons al dl)
                                                       (* ap dp)
                                                       (+ as ds))))))]))

(define (pick idx lat)
  (cond [(= idx 1) (car lat)]
        [else (pick (sub1 idx) (cdr lat))]))

(define (keep-looking a sorn lat)
  (cond [(number? sorn) (keep-looking a (pick sorn lat) lat)]
        [else (eq? sorn a)]))

(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

(define (lookup-in-entry-help name names values entry-f)
  (cond [(null? names) (entry-f name)]
        [(eq? (car names) name) (car values)]
        [else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)]))

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

(define (lookup-in-table name table table-f)
  (cond [(null? table) (table-f name)]
        [else (lookup-in-entry name
                               (car table)
                               (lambda (name)
                                 (lookup-in-table name
                                                  (cdr table)
                                                  table-f)))]))

(define (build a b)
  (cons a (cons b '())))

(define (contains a lat)
  (cond [(null? lat) #f]
        [(eq? (car lat) a) #t]
        [else (contains a (cdr lat))]))

(define (atom-to-action e)
  (cond [(number? e) *const]
        [(contains e '(#t #f cons car cdr null? eq? atom? zero? add1 sub1 number?)) *const]
        [else *identifier]))

(define (list-to-action e)
  (cond [(atom? (car e))
         (cond [(eq? (car e) 'quote) *quote]
               [(eq? (car e) 'lambda) *lambda]
               [(eq? (car e) 'cond) *cond]
               [else *application])]
         [else *application]))

(define (expression-to-action e)
  (cond [(atom? e) (atom-to-action e)]
        [else (list-to-action e)]))

(define (value e)
  (meaning e initial-table))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*const e table)
  (cond [(number? e) e]
        [(eq? e #t) #t]
        [(eq? e #f) #f]
        [else (build 'primitive e)]))

(define text-of second)

(define (*quote e table)
  (text-of e))

(define (initial-table name)
  (car '()))

(define (*identifier e table)
  (lookup-in-table e table initial-table))

(define (*lambda e table)
  (build 'non-primitive
         (cons table (cdr e))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define (evcon lines table)
  (cond [(else? (question-of (car lines)) (meaning (answer-of (car lines))) table)]
        [(meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table)]
        [else (evcon (cdr lines) table)]))

(define (else? x)
  (cond [(atom? x) (eq? x (quote else))]
        [else #f]))

(define question-of first)

(define answer-of second)

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define cond-lines-of cdr)

(define (evlis args table)
  (cond [(null? args) '()]
        [else (cons (meaning (car args) table)
                    (evlis (cdr args) table))]))

(define (*application e table)
  (my-apply (meaning (function-of e) table)
         (evlis (arguments-of e) table)))

(define function-of car)

(define arguments-of cdr)

(define (primitive? l)
  (eq? (first l) 'primitive))

(define (non-primitive? l)
  (eq? (first l) 'non-primitive))

(define (my-apply fun vals)
  (cond [(primitive? fun) (apply-primitive (second fun) vals)]
        [(non-primitive? fun) (apply-closure (second fun) vals)]))

(define (apply-primitive name vals)
  (cond [(eq? name 'cons) (cons (first vals) (second vals))]
        [(eq? name 'car) (car (first vals))]
        [(eq? name 'cdr) (cdr (first vals))]
        [(eq? name 'null?) (null? (first vals))]
        [(eq? name 'eq?) (eq? (first vals) (second vals))]
        [(eq? name 'atom?) (:atom? (first vals) (second vals))]
        [(eq? name 'zero?) (zero? (first vals))]
        [(eq? name 'add1) (add1 (first vals))]
        [(eq? name 'sub1) (sub1 (first vals))]
        [(eq? name 'number?) (number? first vals)]))

(define (:atom? x)
  (cond [(atom? x) #t]
        [(null? x) #f]
        [(primitive? x) #t]
        [(non-primitive? x) #f]
        [else #f]))

(define extend-table cons)

(define new-entry build)

(define (apply-closure closure vals)
  (meaning (body-of closure)
           (extend-table (new-entry (formals-of closure) vals)
                         (table-of closure))))
