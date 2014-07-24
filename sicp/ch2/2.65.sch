#!/usr/bin/env racket
#lang planet neil/sicp

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x nil nil))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list tree)
  (define (iter tree result)
    (if (null? tree)
        result
        (iter (left-branch tree)
              (cons (entry tree)
                    (iter (right-branch tree) result)))))

  (iter tree nil))

(define (list->tree elements)
  (define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

  (car (partial-tree elements (length elements))))

;exercise 2.65
(define (union-set set1 set2)
  ;exercise 2.62
  (define (union-ordered set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((= (car set1) (car set2))
           (cons (car set1)
                 (union-ordered (cdr set1) (cdr set2))))
          ((< (car set1) (car set2))
           (cons (car set1)
                 (union-ordered (cdr set1) set2)))
          (else (cons (car set2)
                      (union-ordered set1 (cdr set2))))))

  (list->tree (union-ordered (tree->list set1)
                             (tree->list set2))))

(define (intersection-set set1 set2)
  ;exercise 2.62
  (define (intersection-ordered set1 set2)
    (cond ((or (null? set1) (null? set2)) nil)
          ((= (car set1) (car set2))
           (cons (car set1)
                 (intersection-ordered (cdr set1) (cdr set2))))
          ((< (car set1) (car set2))
           (intersection-ordered (cdr set1) set2))
          (else
           (intersection-ordered set1 (cdr set2)))))

  (list->tree (intersection-ordered (tree->list set1)
                                    (tree->list set2))))

;test
(define (test-intersection set1 set2)
  (display
   (tree->list (intersection-set set1 set2)))
  (newline))

(define (test-union set1 set2)
  (display
   (tree->list (union-set set1 set2)))
  (newline))

(define x (list->tree '(1 3 5 7 9)))
(define y (list->tree '(4 5 7 9 10)))
(define z (list->tree '(0 1 9 10 20)))

(test-intersection x y)
(test-intersection x z)
(test-intersection y z)

(test-union x y)
(test-union x z)
(test-union y z)
