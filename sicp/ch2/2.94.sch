#!/usr/bin/env racket
#lang planet neil/sicp

(define (repeat f n)
  (cond ((= n 1) (lambda (x) (f x)))
        (else (lambda (x) (f ((repeat f (- n 1)) x))))))

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (car seq) (accumulate f init (cdr seq)))))

(define (make-table)
  (define table nil)

  (define (get index1 index2)
    (define (iter-index1 table)
      (cond ((null? table) false)
            ((equal? (caar table) index1) (iter-index2 (cdar table)))
            (else (iter-index1 (cdr table)))))
    (define (iter-index2 op-list)
      (cond ((null? op-list) false)
            ((equal? (caar op-list) index2) (cadar op-list))
            (else (iter-index2 (cdr op-list)))))
    (iter-index1 table))

  (define (put! index1 index2 item)
    (define (iter-index1 table)
      (cond ((null? table) (list (cons index1 (iter-index2 nil))))
            ((equal? (caar table) index1)
             (cons (cons index1 (iter-index2 (cdar table))) (cdr table)))
            (else (cons (car table) (iter-index1 (cdr table))))))
    (define (iter-index2 op-list)
      (cond ((null? op-list) (list (list index2 item)))
            ((equal? (caar op-list) index2)
             (cons (list index2 item) (cdr op-list)))
            (else (cons (car op-list) (iter-index2 (cdr op-list))))))
    (set! table (iter-index1 table)))

  (lambda (op)
    (cond ((eq? op 'get) get)
          ((eq? op 'put!) put!)
          (else (error "Unknow operation -- TABLE" op)))))

(define op-table (make-table))

(define (get op type)
  ((op-table 'get) op type))

(define (put op type item)
  ((op-table 'put!) op type item))

;exercise 2.78
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'real)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((real? datum) 'real)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((real? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;exercise 2.84
(define types-tower '(polynomial complex real rational integer))

(define (apply-generic op . args)
  (define (type-level type)
    (let ((rest (memq type types-tower)))
      (if rest
          (length rest)
          (error "Invalid type -- TYPE-LEVEL"))))

  (define (convert-to-highest args)
    (let ((types (map type-tag args)))
      (let ((levels (map type-level types)))
        (let ((highest-lv (accumulate max 0 levels)))
          (map (lambda (lv x)
                 (if (= lv highest-lv)
                     x
                     ((repeat raise (- highest-lv lv)) x)))
               levels args)))))

  (define (drop-result x)
    (if (or (eq? op 'add)
            (eq? op 'sub)
            (eq? op 'mul)
            (eq? op 'div))
        (drop x)
        x))

  (let ((types (map type-tag args)))
    (let ((proc (get op types)))
      (if proc
          (drop-result (apply proc (map contents args)))
          (let ((converted (convert-to-highest args)))
                (let ((proc (get op (map type-tag converted))))
                  (if proc
                      (drop-result (apply proc (map contents converted)))
                      (error
                       "No method for these types -- APPLY-GENERIC"
                       (list op types '-> op (map type-tag converted))))))))))

;exercise 2.83
;exercise 2.85
;exercise 2.88
;exercise 2.94
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (greatest-common-divisor x y)
  (apply-generic 'greatest-common-divisor x y))
(define (neg x) (apply-generic 'neg x))
(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

(define (drop x)
  (let ((project (get 'project (list (type-tag x)))))
    (if (not project)
        x
        (let ((projected (project (contents x))))
          (if (equ? x projected)
              (drop projected)
              x)))))

;exercise 2.83
;exercise 2.85
;exercise 2.88
;exercise 2.94
(define (install-integer-package)
  (define (raise x)
    (make-rational (tag x) (tag 1)))

  (define (tag x)
    (attach-tag 'integer x))

  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (make-rational (tag x) (tag y))))
  (put 'greatest-common-divisor '(integer integer)
       (lambda (x y) (tag (gcd x y))))
  (put 'neg '(integer)
       (lambda (x) (tag (- x))))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'equ? '(integer integer) =)
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'raise '(integer) raise)
  'done)

(define (install-real-package)
  (define (raise x)
    (make-complex-from-real-imag x 0))
  (define (project x)
    (define (tag x) (attach-tag 'integer x))
    (make-rational (tag (round x)) (tag 1)))

  (define (tag x)
    (attach-tag 'real x))

  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(real)
       (lambda (x) (tag (- x))))
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'equ? '(real real) =)
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'raise '(real) raise)
  (put 'project '(real) project)
  'done)

;exercise 2.83
;exercise 2.85
;exercise 2.88
;exercise 2.93
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d) (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (neg-rat x)
    (make-rat (neg (numer x))
              (denom x)))
  (define (=zero?-rat x)
    (=zero? (numer x)))
  (define (equ?-rat x y)
    (equ? (mul (numer x) (denom y))
          (mul (denom x) (numer y))))
  (define (to-single-number x)
    (let ((n (numer x))
          (d (denom x)))
      (if (or (eq? (type-tag n) 'polynomial)
              (eq? (type-tag d) 'polynomial))
          0
          (/ (contents n) (contents d)))))
  (define (raise x)
    (make-real (to-single-number x)))
  (define (project x)
    (make-integer (round (to-single-number x))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'neg '(rational)
       (lambda (x) (tag (neg-rat x))))
  (put '=zero? '(rational) =zero?-rat)
  (put 'equ? '(rational rational) equ?-rat)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational) raise)
  (put 'project '(rational) project)
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (define (square x) (* x x))
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (if (and (= (imag-part z) 0)
             (= (real-part z) 0))
        0
        (atan (imag-part z) (real-part z))))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (define (square x) (* x x))
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

;exercise 2.85
;exercise 2.88
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (neg-complex z)
    (make-from-real-imag (- (real-part z))
                         (- (imag-part z))))
  (define (=zero?-complex z)
    (= (magnitude z) 0))
  (define (equ?-complex z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (raise z)
    (make-polynomial 'x (list 'sparse (list 0 (tag z)))))
  (define (project z)
    (make-real (real-part z)))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'neg '(complex)
       (lambda (z) (tag (neg-complex z))))
  (put '=zero? '(complex) =zero?-complex)
  (put 'equ? '(complex complex) equ?-complex)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'raise '(complex) raise)
  (put 'project '(complex) project)
  'done)

;exercise 2.90
(define (install-sparse-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-termlist) nil)
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (tag-term x) (attach-tag 'term x))
  (define (tag-list x) (attach-tag 'sparse x))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  (put 'first-term '(sparse) (lambda (x) (tag-term (first-term x))))
  (put 'rest-terms '(sparse) (lambda (x) (tag-list (rest-terms x))))
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'adjoin-term '(term sparse) (lambda (x y) (tag-list (adjoin-term x y))))
  (put 'make 'term (lambda (x y) (tag-term (make-term x y))))
  (put 'the-empty-termlist 'sparse (lambda () (tag-list (the-empty-termlist))))
  'done)

(define (install-dense-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-termlist) nil)
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (define (rest-terms term-list)
    (let ((rest (cdr term-list)))
      (cond ((null? rest) nil)
            ((equal? (car rest) 0) (rest-terms rest))
            (else rest))))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (adjoin-term term term-list)
    (define (pad-0 term-list n)
      (if (= n 0)
          term-list
          (cons 0 (pad-0 term-list (- n 1)))))
    (if (=zero? (coeff term))
        term-list
        (cons (coeff term)
              (pad-0 term-list
                     (- (order term) (length term-list))))))

  (define (tag-term x) (attach-tag 'term x))
  (define (tag-list x) (attach-tag 'dense x))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  (put 'first-term '(dense) (lambda (x) (tag-term (first-term x))))
  (put 'rest-terms '(dense) (lambda (x) (tag-list (rest-terms x))))
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put 'adjoin-term '(term dense) (lambda (x y) (tag-list (adjoin-term x y))))
  (put 'make 'term (lambda (x y) (tag-term (make-term x y))))
  (put 'the-empty-termlist 'dense (lambda () (tag-list (the-empty-termlist))))
  'done)

;exercise 2.87
;exercise 2.88
;exercise 2.90
;exercise 2.91
;exercise 2.94
(define (install-ploy-package)
  ;;internal procedures
  ;;representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;;representation of terms and term lists
  (define (order t) (apply-generic 'order t))
  (define (coeff t) (apply-generic 'coeff t))
  (define (first-term t-list) (apply-generic 'first-term t-list))
  (define (rest-terms t-list) (apply-generic 'rest-terms t-list))
  (define (empty-termlist? t-list) (apply-generic 'empty-termlist? t-list))
  (define (adjoin-term t t-list) (apply-generic 'adjoin-term t t-list))
  (define (make-term order coeff) ((get 'make 'term) order coeff))
  (define (the-empty-termlist-sparse) ((get 'the-empty-termlist 'sparse)))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist-sparse)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist-sparse)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list L1 L1)
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist-sparse) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms
                        (add-terms L1
                                   (neg-terms
                                    (mul-term-by-all-terms
                                     (make-term new-o new-c) L2)))
                        L2)))
                  (list (adjoin-term
                         (make-term new-o new-c)
                         (car rest-of-result))
                        (cadr rest-of-result))))))))
  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))
  (define (gcd-terms L1 L2)
    (if (empty-termlist? L2)
        L1
        (gcd-terms L2 (remainder-terms L1 L2))))
  (define (neg-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist-sparse)
        (let ((t (first-term L)))
          (adjoin-term
           (make-term (order t) (neg (coeff t)))
           (neg-terms (rest-terms L))))))
  (define (equ?-terms L1 L2)
    (cond ((and (empty-termlist? L1) (empty-termlist? L2))
           true)
          ((empty-termlist? L1) false)
          ((empty-termlist? L2) false)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (if (not (and (= (order t1) (order t2))
                           (equ? (coeff t1) (coeff t2))))
                 false
                 (equ?-terms (rest-terms L1) (rest-terms L2)))))))

  (define (major-variable p1 p2)
    (define (highest-order term-list)
      (if (empty-termlist? term-list)
          0
          (order (first-term term-list))))
    (let ((v1 (variable p1)) (ho1 (highest-order (term-list p1)))
          (v2 (variable p2)) (ho2 (highest-order (term-list p2))))
      (cond ((and (> ho1 0) (> ho2 0) (not (eq? v1 v2))) false)
            ((and (> ho1 0) (= ho2 0)) v1)
            ((and (= ho1 0) (> ho2 0)) v2)
            (else v1))))
  (define (add-poly p1 p2)
    (let ((v (major-variable p1 p2)))
      (if v
          (make-poly v
                     (add-terms (term-list p1)
                                (term-list p2)))
          (error "Polys not in same var -- ADD-POLY"
                 (list p1 p2)))))
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  (define (mul-poly p1 p2)
    (let ((v (major-variable p1 p2)))
      (if v
          (make-poly v
                     (mul-terms (term-list p1)
                                (term-list p2)))
          (error "Polys not in same var -- MUL-POLY"
                 (list p1 p2)))))
  (define (div-poly p1 p2)
    (let ((v (major-variable p1 p2)))
      (if v
          (let ((result
                 (div-terms (term-list p1)
                            (term-list p2))))
            (list (make-polynomial v (car result))
                  (make-polynomial v (cadr result))))
          (error "Polys not in same var -- DIV-POLY"
                 (list p1 p2)))))
  (define (gcd-poly p1 p2)
    (let ((v (major-variable p1 p2)))
      (if v
          (make-polynomial v (gcd-terms (term-list p1)
                                        (term-list p2)))
          (error "Polys not in same var -- GCD-POLY"
                 (list p1 p2)))))
  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))
  (define (=zero?-poly p)
    (empty-termlist? (term-list p)))
  (define (equ?-poly p1 p2)
    (and (major-variable p1 p2)
         (equ?-terms (term-list p1)
                     (term-list p2))))

  ;;interfacr to rest of system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (div-poly p1 p2)))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (gcd-poly p1 p2)))
  (put 'neg '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'equ? '(polynomial polynomial) equ?-poly)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-sparse-package)
(install-dense-package)
(install-ploy-package)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-real n)
  ((get 'make 'real) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;test
(define (test f . args)
  (display (apply f args))
  (newline))

(define x (make-integer 10))
(define y (make-integer 10))

(define p1 (make-polynomial 'x '(sparse (4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '(sparse (3 1) (1 -1))))

(define p3 (make-polynomial 'x '(dense 1 -1 -2 2 0)))
(define p4 (make-polynomial 'x '(dense 1 0 -1 0)))

(test greatest-common-divisor x y)
(test greatest-common-divisor p1 p2)
(test greatest-common-divisor p3 p4)
