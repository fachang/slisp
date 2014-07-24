#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-tree entry) (list entry nil nil))
(define (entry tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))
(define (set-entry! tree entry) (set-car! tree entry))
(define (set-left! tree subtree) (set-car! (cdr tree) subtree))
(define (set-right! tree subtree) (set-car! (cddr tree) subtree))

(define (make-bst) (cons '*bst* nil))

(define (insert-bst! key new-entry bst)
  (define (iter parent child action)
    (cond ((null? child)
           (action parent (make-tree new-entry)))
          ((= (key (entry child)) (key new-entry))
           (set-entry! child new-entry))
          ((< (key (entry child)) (key new-entry))
           (iter child (right child) set-right!))
          (else
           (iter child (left child) set-left!))))
  (iter bst (cdr bst) set-cdr!))

(define (lookup-bst key target-key bst)
  (define (iter tree)
    (cond ((null? tree) false)
          ((= (key (entry tree)) target-key)
           (entry tree))
          ((< (key (entry tree)) target-key)
           (iter (right tree)))
          (else
           (iter (left tree)))))
  (iter (cdr bst)))

(define (make-table)
  (let ((table (cons '*table* (cons (make-bst) (make-bst)))))
    (define (lookup target-keys)
      (define (iter keys table)
        (let ((key (car keys))
              (rest-keys (cdr keys)))
          (if (null? rest-keys)
              (let ((record (lookup-bst car key (cadr table))))
                (if record
                    (cdr record)
                    false))
              (let ((subtable (lookup-bst car key (cddr table))))
                (if subtable
                    (iter rest-keys subtable)
                    false)))))
      (iter target-keys table))

    (define (insert! keys value)
      (define (iter keys table)
        (let ((key (car keys))
              (rest-keys (cdr keys)))
          (if (null? rest-keys)
              (insert-bst! car (cons key value) (cadr table))
              (let ((subtable (lookup-bst car key (cddr table))))
                (if subtable
                    (iter rest-keys subtable)
                    (let ((new-table
                           (cons key (cons (make-bst) (make-bst)))))
                      (insert-bst! car new-table (cddr table))
                      (iter rest-keys new-table)))))))
      (iter keys table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;test
(define t (make-table))

((t 'insert!) '(90 91) 'john)
((t 'insert!) '(90 91 92) 'david)
((t 'insert!) '(87 88 95) 'marry)
((t 'insert!) '(78 95 86) 'henry)

((t 'lookup) '(90 91))
((t 'lookup) '(90 91 92))
((t 'lookup) '(87 88 95))
((t 'lookup) '(78 95 86))

((t 'lookup) '(90))
((t 'lookup) '(78 95 80))
((t 'lookup) '(90 91 93))
