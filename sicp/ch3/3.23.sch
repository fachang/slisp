#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-deque) (cons nil nil))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty queue" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty queue" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-entry (cons item (cons nil (front-ptr deque)))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-entry)
           (set-rear-ptr! deque new-entry)
           deque)
          (else
           (set-car! (cdr (front-ptr deque)) new-entry)
           (set-front-ptr! deque new-entry)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-entry (cons item (cons (rear-ptr deque) nil))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-entry)
           (set-rear-ptr! deque new-entry)
           deque)
          (else
           (set-cdr! (cdr (rear-ptr deque)) new-entry)
           (set-rear-ptr! deque new-entry)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty queue" deque))
        (else
         (set-front-ptr! deque (cddr (front-ptr deque)))
         (or (empty-deque? deque) (set-car! (cdr (front-ptr deque)) nil))
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty queue" deque))
        (else
         (set-rear-ptr! deque (cadr (rear-ptr deque)))
         (if (null? (rear-ptr deque))
             (set-front-ptr! deque nil)
             (set-cdr! (cdr (rear-ptr deque)) nil))
         deque)))

(define (print-deque deque)
  (define (iter front)
    (if (null? front)
        (display ")")
        (begin (display (car front))
               (or (null? (cddr front)) (display " "))
               (iter (cddr front)))))
  (display "(")
  (iter (front-ptr deque))
  (newline))

;test
(define dq (make-deque))

(print-deque (front-insert-deque! dq 2))
(print-deque (front-insert-deque! dq 1))
(print-deque (rear-insert-deque! dq 3))
(print-deque (rear-insert-deque! dq 4))

(print-deque (front-delete-deque! dq))
(print-deque (rear-delete-deque! dq))

(print-deque (front-insert-deque! dq 1))
(print-deque (rear-insert-deque! dq 4))

(print-deque (rear-delete-deque! dq))
(print-deque (rear-delete-deque! dq))
(print-deque (front-delete-deque! dq))
(print-deque (front-delete-deque! dq))

(print-deque (rear-insert-deque! dq 2))
(print-deque (front-insert-deque! dq 1))

(print-deque (rear-delete-deque! dq))
(print-deque (rear-delete-deque! dq))
