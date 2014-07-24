#!/usr/bin/env racket
#lang planet neil/sicp

;Generated DOT file of the process tree
;Usage: tree.rkt | dot -T png -o tree.png

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree-graph elements)
  (define (display-node name n)
    (display "  ")
    (display name)
    (display "[label=\"")
    (display n)
    (display "\"];")
    (newline))

  (define (display-edge a b)
    (display "  ")
    (display a)
    (display " -> ")
    (display b)
    (display ";")
    (newline))

  (define (left node)
    (string-append node "l"))

  (define (right node)
    (string-append node "r"))

  (define (nth-of-list n list)
    (if (= n 0)
        (car list)
        (nth-of-list (- n 1) (cdr list))))

  (define (partial-tree-graph elts n node)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (display-node node (nth-of-list left-size elts))
          (cond ((not (= n 1))(display-edge node (left node))))
          (let ((left-result (partial-tree-graph elts left-size (left node))))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (cond ((not (= n 1))(display-edge node (right node))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree-graph (cdr non-left-elts)
                                                      right-size
                                                      (right node))))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry left-tree right-tree)
                        remaining-elts))))))))
  (display "digraph {")
  (newline)
  (car (partial-tree-graph elements (length elements) "s"))
  (display "}")
  (newline))

(list->tree-graph '(1 3 5 7 9 11))
