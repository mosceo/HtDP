;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname db) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)


;===========================================================
; 23.7 Project: Database
;===========================================================

(define-struct db [schema content])
; A DB is a structure:
;   (make-db Schema Content)
 
; A Schema is a [List-of Spec]

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)

; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch


(define db0 (make-db (list (list "Name" string?)        ; schema
                           (list "Age" integer?)
                           (list "Present" boolean?))
                     (list (list "Alice" 35 #true)      ; content
                           (list "Bob"   25 #false)
                           (list "Carol" 30 #true))))

(define db0-proj (make-db (list (list "Name" string?)        ; schema
                                (list "Present" boolean?))
                          (list (list "Alice" #true)      ; content
                                (list "Bob"   #false)
                                (list "Carol" #true))))

(define db1-bad (make-db (list (list "Name" string?)
                               (list "Age" integer?)
                               (list "Present" boolean?))
                         (list (list "Alice" 35 #true)
                               (list "Bob"   25 #false)
                               (list "Carol" 30 12))))


; DB -> Boolean
(check-expect (integrity-check db0) #true)
(check-expect (integrity-check db1-bad) #false)

(define (integrity-check db)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define width   (length schema))
          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row) width)
                 (andmap (lambda (s c) [(second s) c])
                         schema
                         row))))
    (andmap row-integrity-check content)))


;----------
; Project ;
;----------

; DB [List-of Label] -> DB
(check-expect (db-content (project db0 (list "Name" "Present")))
              (db-content db0-proj))

(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row
          ; retain those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask))
          (define mask (map keep? schema)))
    (make-db (filter keep? schema)
             (map row-project content))))


;----------
; Ex. 408 ;
;----------

; DB [List-of Label] [Row -> Boolean] -> DB
; Consumes a database, a list of labels, and a predicate on rows.
; The result is a list of rows that satisfy the given predicate,
; projected down to the given set of labels. 

(define (select db labels pred?)
  (local ((define db-rows-filtered
            (make-db (db-schema db)
                     (filter pred? (db-content db)))))
    (project db-rows-filtered labels)))


;----------
; Ex. 409 ;
;----------

; DB [List-of Label] -> DB
; The function consumes a database db and list lol of Labels.
; It produces a database like db but with its columns reordered according to lol.

(define (reorder db lol)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define labels  (map first schema))
          (define index   (map (lambda (l) (list-index labels l)) lol))
          
          ; Row -> Row
          ; retain those columns whose name is in labels
          (define (row-rearrange row)
            (list-pluck row index)))
          ; -- IN --
          (make-db (list-pluck schema index)
                   (map row-rearrange content))))


; [List-of Any] Any -> Number
; find the index of an element in a list
(check-expect (list-index (list 1 2 3) 1) 0)
(check-expect (list-index (list 'a 'b 'c) 'c) 2)

(define (list-index l e)
  (cond [(equal? (first l) e) 0]
        [else (+ (list-index (rest l) e) 1)]))


; [List-of Any] [List-of Number] -> [List-of Any]
; form a new list from a list of indexes and an original list
(check-expect (list-pluck (list 1 2 3 'a 'b 'c) (list)) (list))
(check-expect (list-pluck (list 1 2 3 'a 'b 'c) (list 1)) (list 2))
(check-expect (list-pluck (list 1 2 3 'a 'b 'c) (list 4 2 5 0)) (list 'b 3 'c 1))

(define (list-pluck l index)
  (for/list [(i index)]
    (list-ref l i)))

; testing by fiddling
;(reorder db0 (list "Age" "Name"))


;----------
; Ex. 411 ;
;----------
(check-expect (select-rows
               (make-db 'no-use (list (list "a" 10) (list "b" 20) (list "a" 30)))
               "a")
              (list (list "a" 10) (list "a" 30)))
(check-expect (select-rows
               (make-db 'no-use (list (list "a" 10) (list "b" 20) (list "a" 30)))
               "c")
              (list))
              
(define (select-rows db val)
  (filter (lambda (row) (equal? (first row) val))
          (db-content db)))


; [List-of [List-of Any]] -> [List-of [List-of Any]]
; given a list of lists, remove the first element in each list
(check-expect (list-remove-first '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))

(define (list-remove-first l)
  (map rest l))


; [List-of [List-of Any]] -> [List-of [List-of Any]]
; given a list of lists, remove the first element in each list
(check-expect (list-remove-last '((1 2 3) (4 5 6) (7 8 9))) '((1 2) (4 5) (7 8)))

(define (list-remove-last l)
  (map remove-last l))


; [List-of Any] -> [List-of Any]
; remove the last element in a list
(check-expect (remove-last '(1 2 3)) '(1 2))

(define (remove-last l)
  (cond [(empty? l) '()]
        [(empty? (rest l)) '()]
        [else (cons (first l) (remove-last (rest l)))]))


; [List-of Any] -> Any
; get the last element from a list
(check-expect (get-last '(1)) 1)
(check-expect (get-last '(1 2 3)) 3)

(define (get-last l)
  (list-ref l (- (length l) 1)))


; [List-of Any] [List-of [List-of Any]] -> [List-of [List-of Any]]
(check-expect (list-join '(1 2) '((3 4) (5 6)))
              '((1 2 3 4) (1 2 5 6)))

(define (list-join pre ll)
  (for/list ([l ll])
    (append pre l)))


; DB DB -> DB
; join two tables by last field of db1 and first field of db2 
(define (join db1 db2)
  (local ((define content1 (db-content db1))
          (define content2 (db-content db2))
          (define new-schema (append (remove-last  (db-schema db1))
                                     (rest         (db-schema db2))))
          (define (process-row row)
            (list-join (remove-last row)
                       (list-remove-first (select-rows db2 (get-last row)))))
          (define new-content
            (foldr (lambda (row res) (append (process-row row) res))
                   '() content1)))
    ; -- IN --
    (make-db new-schema
             new-content)))


;-------------------------
; Testing (join db1 db2) ;
;-------------------------

(define db10 (make-db (list (list "Field1" string?)
                            (list "Field2" integer?)
                            (list "Field3" boolean?))
                      (list (list "A" 10 #true)
                            (list "B" 20 #false)
                            (list "C" 30 #true))))

(define db11 (make-db (list (list "Field3" boolean?)
                            (list "Field4" string?))
                      (list (list #true "t" "true")
                            (list #true "T" "TRUE")
                            (list #false "f" "false")
                            (list #false "F" "FALSE"))))

(define db12 (make-db (list (list "Field1" string?)
                            (list "Field2" integer?)
                            (list "Field4" string?))
                      (list (list "A" 10 "t" "true")
                            (list "A" 10 "T" "TRUE")
                            (list "B" 20 "f" "false")
                            (list "B" 20 "F" "FALSE")
                            (list "C" 30 "t" "true")
                            (list "C" 30 "T" "TRUE"))))

(check-expect (db-content (join db10 db11)) (db-content db12)) 
