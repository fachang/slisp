#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;part a
(define (left-branch m) (car m))

(define (right-branch m) (cadr m))

(define (branch-length b) (car b))

(define (branch-structure b) (cadr b))

;part b
(define (total-weight m)
  (if (not (pair? m))
      m
      (+ (total-weight (branch-structure (left-branch m)))
         (total-weight (branch-structure (right-branch m))))))

;test
(define x
  (make-mobile (make-branch 1.0 100.0)
               (make-branch 2.0 (make-mobile (make-branch 2.0 30.0)
                                             (make-branch 3.0 20.0)))))

(total-weight x)    ;150

;part c
(define (balanced? m)
    (if (not (pair? m))
        true
        (let ((lb (left-branch m))
              (rb (right-branch m))
              (lm (branch-structure (left-branch m)))
              (rm (branch-structure (right-branch m))))
          (and (= (* (branch-length lb) (total-weight lm))
                  (* (branch-length rb) (total-weight rm)))
               (balanced? lm)
               (balanced? rm)))))

;test
(define y
  (make-mobile (make-branch 5.0 (make-mobile (make-branch 1.0 20.0)
                                             (make-branch 2.0 10.0)))
               (make-branch 3.0 (make-mobile (make-branch 2.0 30.0)
                                             (make-branch 3.0 20.0)))))

(define z
  (make-mobile (make-branch 5.0 (make-mobile (make-branch 1.0 20.0)
                                             (make-branch 1.0 10.0)))
               (make-branch 3.0 (make-mobile (make-branch 2.0 30.0)
                                             (make-branch 3.0 20.0)))))

(balanced? y)    ;true
(balanced? z)    ;false

;part d
(define (make-mobile2 left right)
  (cons left right))

(define (make-branch2 left right)
  (cons left right))

;the only thing need to change is the selector. as below:

(define (left-branch2 m) (car m))

(define (right-branch2 m) (cdr m))

(define (branch-length2 b) (car b))

(define (branch-structure2 b) (cdr b))
