#!/usr/bin/env racket
#lang planet neil/sicp

(#%require "load-test1.sch")
(#%provide (all-defined)
           (all-from "load-test1.sch"))

(define (func2)
  (newline)
  (display "func2 from file2"))
