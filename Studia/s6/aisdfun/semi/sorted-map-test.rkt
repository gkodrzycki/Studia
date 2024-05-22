#lang racket

; A testing script for the sorted-map library.

; This script uses a combination of test cases,
; exhaustive testing and random testing to find
; bugs.

(require test-engine/racket-tests)

(require "sorted-map.rkt")


; Compare red-black maps to sorted alists for correctness.
(define (sorted-alist-insert compare alist key value)
  
  (define/match (ins alist)
    ['()      `((,key . ,value))]
    [`(,(and n (cons k v)) . ,rest)
     ; =>
     (switch-compare (compare key k)
       [<   (cons (cons key value) alist)]
       [=   (cons (cons key value) rest)]
       [>   (cons n (ins rest))])])
  
  (ins alist))

(define (sorted-alist-delete compare alist key)
  
  (define/match (del alist)
    ['()      '()]
    [`(,(and n (cons k v)) . ,rest)
     ; =>
     (switch-compare (compare key k)
       [<   rest]
       [=   rest]
       [>   (cons n (del rest))])])
  
  (del alist))
             

(define (int-compare a b) (- a b))

(define test-empty (sorted-map-empty int-compare))

(define test-tree (sorted-map-insert* test-empty '(3 1 5 2 8 7 6) '(a b c d e f g)))

(define sub-test-tree (sorted-map-insert* test-empty '(1 3 5) '(b a c)))

(define del-test-tree (sorted-map-delete test-tree 8))

(check-expect (sorted-map-get (L int-compare) 2) #f)

(check-expect (sorted-map-get test-tree 3) 'a)
(check-expect (sorted-map-get test-tree 1) 'b)
(check-expect (sorted-map-get test-tree 5) 'c)
(check-expect (sorted-map-get test-tree 2) 'd)
(check-expect (sorted-map-get test-tree 8) 'e)

(check-expect (sorted-map-get del-test-tree 8) #f)
(check-expect (sorted-map-get del-test-tree 3) 'a)
(check-expect (sorted-map-get del-test-tree 1) 'b)
(check-expect (sorted-map-get del-test-tree 5) 'c)
(check-expect (sorted-map-get del-test-tree 2) 'd)

(check-expect (sorted-map-get (sorted-map-delete test-tree 3) 8) 'e)
(check-expect (sorted-map-get (sorted-map-delete test-tree 3) 3) #f)
(check-expect (sorted-map-get (sorted-map-delete test-tree 1) 5) 'c)
(check-expect (sorted-map-get (sorted-map-delete test-tree 8) 2) 'd)
(check-expect (sorted-map-get (sorted-map-delete test-tree 5) 1) 'b)
(check-expect (sorted-map-get (sorted-map-delete test-tree 6) 7) 'f)
(check-expect (sorted-map-get (sorted-map-delete test-tree 7) 6) 'g)

(check-expect (sorted-map-submap? sub-test-tree test-tree) #t)

(check-expect (sorted-map-size test-tree) 7)



; Constructs all permutations of a list.
; Source: http://www.acooke.org/cute/PermuteFuc0.html
(define (permute lst)
  
  (define (flat-map f lst) (flat-map-acc f lst '()))
  (define (flat-map-acc f lst acc)
    (if (null? lst)
        acc
        (flat-map-acc f (cdr lst) (append acc (f (car lst))))))
  
  ; This does not attempt to keep any kind of ordering.
  (define (except-for x lst) (except-acc x lst '()))
  (define (except-acc x lst acc)
    (if (null? lst)
        acc
        (let ((head (car lst))
              (tail (cdr lst)))
          (if (equal? x head)
              (append acc tail)
              (except-acc x tail (cons head acc))))))
  
  (case (length lst)
    ((0) '())
    ((1) (list lst))
    (else (flat-map (lambda (head)
                      (map (lambda (perm) (cons head perm))
                           (permute (except-for head lst))))
                    lst))))


; Builds a map in the specified order, then deletes in the specified order.
; Checks for red-black violations and inconsistencies the entire way.
(define (build-kill build-order delete-order smap alist)
  (cond
    [(and (null? build-order) (null? delete-order))
     ; =>
     #t]
    
    [(null? build-order)
     ; =>
     (let* ((d         (car delete-order))
            (smap*     (sorted-map-delete smap d))
            (alist*    (sorted-alist-delete int-compare alist d))
            (legal?    (sorted-map-is-legal? smap*))
            (smap*list (sorted-map-to-alist smap*)))
       (when (not legal?) 
         (display "failed on: ")
         (display smap)
         (newline)
         (display "deleting: ")
         (display d)
         (newline)
         (error "Implementation error: illegal red-black map"))
       (when (not (equal? smap*list alist*))
         (error "Implementation error: inconsistency discovered"))
       (build-kill '() (cdr delete-order) smap* alist*))]
    
    [else
     ; =>
     (build-kill (cdr build-order) delete-order
                 (sorted-map-insert smap (car build-order) #t) 
                 (sorted-alist-insert int-compare alist (car build-order) #t))]))


(define (test-all lst)
  (let ((permutations (permute lst)))
    (for-each (lambda (perm1)
      (for-each (lambda (perm2)
        (build-kill perm1 perm2 test-empty '()))
                permutations))
              permutations)))


;; Exhaustive testing

; Tested up to '(1 2 3 4 5 6 7 8)
(check-expect (test-all '(1 2 3 4 5)) (void))



;; Randomized testing

(define (build-random-tree size)
  (cond
    [(= 0 size) (sorted-map-empty int-compare)]
    [else
     (let* ((el (inexact->exact (round (* size size (random))))))
       (sorted-map-insert (build-random-tree (- size 1))
                          el #t))]))

    

(define (test-random-delete size)
  
  (define rand-tree (build-random-tree size))
  
  (define rand-alist (sorted-map-to-alist rand-tree))

  (define rand-index (random-in 0 (- (sorted-map-size rand-tree) 1)))
  
  (define pair (list-ref rand-alist rand-index))
  
  (define key (car pair))
  
  (define new-tree (sorted-map-delete rand-tree key))
  
  (define new-list (sorted-alist-delete int-compare rand-alist key))
  
  (define new-tree-as-list (sorted-map-to-alist new-tree))
  
  (define (fail message)
    (display (format "old list: ~s~nold map: ~s~nnew map:~s~ndelete: ~s~n"
                     new-list
                     (sorted-map-to-tree rand-tree)
                     (sorted-map-to-tree new-tree)
                     key))
    (error message))
    
  (when (not (sorted-map-is-legal? new-tree))
    (fail "test-random-delete failed: illegal map"))
  
  (when (not (equal? new-list new-tree-as-list))
    (fail "test-random-delete failed: delete failed")))
    

(define (random-in lo hi)
  (let ((r (+ lo (inexact->exact (round (* (random) (- hi lo)))))))
    (cond
      [(< r lo)  lo]
      [(> r hi)  hi]
      [else      r])))

(define random-iterations 2000)
(define random-size-max 50)

(for ([i (in-range 1 random-iterations)])
  (test-random-delete (random-in 1 random-size-max)))



(test)


(define (sorted-map-to-sexp t)
  (match t
    [(T _ c l k v r)
     `(,c ,(sorted-map-to-sexp l)
          ,k
          ,v
          ,(sorted-map-to-sexp r))]
    
    [(L _) 'L]
    
    [(BBL _) 'BBL]))
           

(define t (sorted-map-insert* test-empty '(25 75 100 122) '(#f #f #f #f)))

(define t2 (sorted-map-delete t '25))

(sorted-map-to-sexp t)

(sorted-map-to-sexp t2)
