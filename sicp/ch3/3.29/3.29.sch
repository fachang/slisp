#!/usr/bin/env racket
#lang planet neil/sicp

(#%require "sim.sch" "cell.sch")

;test
(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))

(probe 'output output)

(or-gate input-1 input-2 output)

(set-signal! input-1 0)
(set-signal! input-2 1)
(propagate)

(set-signal! input-1 0)
(set-signal! input-2 0)
(propagate)
