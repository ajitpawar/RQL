#|==============================================
     Assignment 1 - RQL

     Taher Jafferjee, g4jaffer
     Ajit Pawar, g4pawar

===============================================|#

#lang racket

; needed for eval to work correctly
(define ns (make-base-namespace))

; Alias
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

(provide attributes
         tuples
         size
         SELECT)



;========================================
; Part 0: Semantic aliases
;=======================================
#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)
  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  (first table))

#|
(tuples table)
  table: a valid table
  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  (if (empty? (first table))
      '()
      (rest table)))

#|
(size table)
  table: a valid table
  Returns the number of tuples in 'table'.
|#
(define (size table)
  (length (tuples table)))



;=======================================
; Cartesian
;=======================================
(define (cartesian-product* next-list so-far)
  (foldl append '()
         (map (lambda (e)
                (map (lambda (x) (cons e x)) so-far))
              next-list)))

;(cartesian-product* (reverse '(a b)) '((10 11) (21 23) (37)))


(define curry
    (lambda (f . c) (lambda x (apply f (append c x)))))

(define stitch
    (lambda (tuples element)
        (map (curry cons element) tuples)))

(define flatten
    (curry apply append))

(define cartesian
    (lambda (l1 l2)
        (flatten (map (curry stitch l2) l1))))


; lists is a list of tables
; (first lists) gives you the whole first table
(define cartesian-lists
    (lambda (lists)
        (foldr cartesian '(()) lists)))

(define cartesian-map
    (lambda (f lists)
        (map (curry apply f) (cartesian-lists lists))))



;=======================================
; Union + Rename tables
;=======================================
; Union of two lists
(define (union ls1 ls2)
  (let loop ([ls1 ls1])
    (cond
      [(null? ls1) ls2]
      ;[(member (first ls1) ls2) (loop (rest ls1))]
      [else
       (cons (first ls1) (loop (rest ls1)))])))


; For each row of the table, merge multiple columns into single list
; In other words, consolite list of lists into single list (for each row of table)
(define (merge-columns table)
  (if (empty? table)
      '()
      (cons (foldl
              (lambda (x y) (union x y))
              '()
              (reverse (first table))) (merge-columns (rest table)))))


; Union all tables listed in tablenames by cartesian
; NOTE: tablenames is a list of tables. (first tablenames) gives you first entire table
(define (union-all-tables tablenames)
  (let ([mega-table (cartesian-map list tablenames)])
    (merge-columns mega-table)))



; list of all attribs from all tables in tables
(define (make-attrib-list tables)
  (flatten (map (lambda (table) (if (empty? table)
                       '()
                       (first table))) tables)))

; returns true if item appears in lst
(define (appears lst item)
  (> (length (filter (lambda (x) (equal? x item)) lst)) 0))

(define (remove-dup lst)
  (foldl (lambda (x y) (if (member x (remove x y)) (remove x y) y)) lst lst))

(define (remove-unique lst)
  (foldl (lambda (x y) (if (not (member x (remove x y))) (remove x y) y)) lst lst))


(define (get-pos lst item [index 0])
  (cond [(empty? lst) -1]
        [(equal? (first lst) item) index]
        [else (get-pos (rest lst) item (+ index 1))]))


(define (get-table-alt-name tables tname renames)
  (let ([pos (get-pos tables tname)])
    (list-ref renames pos)))


(define (rename-attrib-list tables renames dup-list)
  (map (lambda (t) (if (empty? (first t))   ; if no attribs
                           (append '())
                           (map (lambda (attr) (if (appears dup-list attr)
                                                (append (string-append
                                                         (get-table-alt-name tables t renames)
                                                         "." attr))
                                                (append attr))) (first t) ))) tables))


(define (make-final-table tables renames)
    (let* ([attrib-list (make-attrib-list tables)] ; orignal attribs
        [dup-list (remove-dup (remove-unique  attrib-list))]    ; duplicate attribs
        [attrib-list-new (if (empty? dup-list)
                             attrib-list
                             (flatten (rename-attrib-list tables renames dup-list)))] ; renamed attribs
        [mega-table (union-all-tables (map (lambda (x) (rest x)) tables))]) ; merged table

      (cons attrib-list-new mega-table)))



;=======================================
; SELECT helper
;=======================================
; Given a row and field-name, return the index of that field-name
; Return -1 otherwise
(define (get-col-index field lst [index 0])
  (cond
    ((null? lst) -1)
    ((equal? (first lst) field) index)
    (else (get-col-index field (rest lst) (+ index 1)))))


; Generate list of indices
(define (generate-col-indices columns table)
  (filter (lambda (x) (>= x 0)) (map (lambda (field) (get-col-index field (first table))) columns)))


; Given a row and an index, return the field there
(define (select-from-row row col-index)
  (map (lambda (index) (list-ref row index)) col-index))



;=======================================
; SELECT
;=======================================
; select from single table
(define (select-single columns table)
  (let* ([cols (if (equal? * columns)
                   (make-attrib-list (list table))
                   columns)]
         [col-index (generate-col-indices cols table)])

  (map (lambda (row) (select-from-row row col-index)) table)))  ; get specified attrs row-by-row



; select *
(define (select-star tables [renames '()])
  (if (equal? 1 (length tables))
              (first tables)
              (make-final-table tables renames)))


; select from multiple tables
(define (select-multiple columns tables renames)
  (let* ([final-table (make-final-table tables renames)])
    (select-single columns final-table)))



;=======================================
; ORDER BY
;=======================================

(define (order-by attr table)
  (let* ([index (get-col-index attr (first table))])
    (if (< index 0)
        table
        (cons (first table)
              (sort (rest table) (lambda (x y) (> (list-ref x index) (list-ref y index))))))))


(define (order-by-f attr table f)
  (let* ([index (get-col-index attr (first table))])
    (if (< index 0)
        table
        (cons (first table) (sort (rest table) (lambda (x y)
                             (> (f (list-ref x index))
                                (f (list-ref y index)))))))))



#|
(define (order-by expr table)
  (filter-table (lambda (tuple)
                  (eval (rec-eval expr tuple) ns)) table))
|#

;=======================================
; WHERE
;=======================================
(define (rec-eval expr tuple)
  (cond [(empty? expr) expr]
        [(list? (first expr)) (cons (rec-eval (first expr) tuple) (rec-eval (rest expr) tuple))]
        [else (cons ((first expr) tuple) (rec-eval (rest expr) tuple))]))



(define (where expr table)
  (filter-table (lambda (tuple)
                  (eval (rec-eval expr tuple) ns)) table))




;=======================================
; Macro helpers (Part 4)
;=======================================
#|
A function that takes:
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (get-attr attrib-list attr tuple)
  (if (empty? tuple)
      '()
      (list-ref tuple (get-pos attrib-list attr))))



#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table
  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#
(define (filter-table f table)
  (if (empty? (tuples table))
      '()
      (cons (first table) (filter f (rest table)))))


#|
A function 'replace-attr' that takes:
  - x
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#
(define (replace-attr x attrib-list)
 (lambda (tuple) (if (appears attrib-list x)
                     (list-ref tuple (get-pos attrib-list x))
                     x)))



; What should this macro do?
; Returns list of functions
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     (list (replace expr table) ...)]

    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     (replace-attr atom (first table))]

    ))




;=======================================
; Macros
;=======================================

(define-syntax SELECT
  (syntax-rules (* FROM WHERE ORDER BY)

    ; SELECT columns
    [(SELECT <cols> FROM <table>)
                          (select-single <cols> <table>)]
    [(SELECT <cols> FROM [<t1> <n1>] [<t2> <n2>] ...)
                          (select-multiple <cols> (list <t1> <t2> ...) (list <n1> <n2> ...))]


    ; ORDER BY
    [(SELECT <cols> FROM <table> ORDER BY (<f> <attr>))
                          (select-single <cols> (order-by-f <attr> (SELECT * FROM <table>) <f>))]
    [(SELECT <cols> FROM [<t1> <n1>] [<t2> <n2>] ... ORDER BY (<f> <attr>))
                          (select-single <cols> (order-by-f <attr> (make-final-table (list <t1> <t2> ...) (list <n1> <n2> ...)) <f>))]

    [(SELECT <cols> FROM <table> ORDER BY <attr>)
                          (select-single <cols> (order-by <attr> (SELECT * FROM <table>)))]
    [(SELECT <cols> FROM [<t1> <n1>] [<t2> <n2>] ... ORDER BY <attr>)
                          (select-single <cols>
                             (order-by <attr>
                                (make-final-table (list <t1> <t2> ...) (list <n1> <n2> ...))))]


    ; WHERE
    [(SELECT <cols> FROM <table> WHERE <pred> ORDER BY (<f> <attr>))
                          (select-single <cols> (order-by-f <attr> (where (replace <pred> <table>) <table>) <f>))]
    [(SELECT <cols> FROM <table> WHERE <pred> ORDER BY <attr>)
                          (select-single <cols> (order-by <attr> (where (replace <pred> <table>) <table>) ))]
    [(SELECT <cols> FROM <table> WHERE <pred>)
                         (select-single <cols> (where (replace <pred> <table>) <table>))]

    [(SELECT <cols> FROM [<t1> <n1>] [<t2> <n2>] ... WHERE <pred> ORDER BY (<f> <attr>))
                       (let* ([final-table (make-final-table (list <t1> <t2> ...) (list <n1> <n2> ...))])
                          (select-single <cols> (order-by-f <attr> (where (replace <pred> final-table) final-table) <f>)))]
    [(SELECT <cols> FROM [<t1> <n1>] [<t2> <n2>] ... WHERE <pred> ORDER BY <attr>)
                       (let* ([final-table (make-final-table (list <t1> <t2> ...) (list <n1> <n2> ...))])
                          (select-single <cols> (order-by <attr> (where (replace <pred> final-table) final-table) )))]
    [(SELECT <cols> FROM [<t1> <n1>] [<t2> <n2>] ... WHERE <pred>)
                       (let* ([final-table (make-final-table (list <t1> <t2> ...) (list <n1> <n2> ...))])
                         (select-single <cols> (where (replace <pred> final-table) final-table)))]
    ))

#|
 (SELECT *
   FROM Person
   WHERE (equal? "LikesChocolate" #t)
   ORDER BY (+ (string-length "Name") (string-length "Age")))
 |#