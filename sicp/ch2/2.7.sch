#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))
