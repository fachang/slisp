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
(define types-tower '(complex real rational integer))

(define (apply-generic op . args)
  (define (type-level type)
    (let ((rest (memq type types-tower)))
      (if rest
          (length rest)
          (error "Invalid type -- TYPE-LEVEL" type))))

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
                       (list op types)))))))))

;exercise 2.83
;exercise 2.85
;exercise 2.86
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (sine x) (apply-generic 'sine x))
(define (cose x) (apply-generic 'cose x))
(define (atang x y) (apply-generic 'atang x y))
(define (squrt x) (apply-generic 'squrt x))
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
;exercise 2.86
(define (install-integer-package)
  (define (raise x)
    (make-rational x 1))

  (define (tag x)
    (attach-tag 'integer x))

  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (make-rational x y)))
  (put 'sine '(integer)
       (lambda (x) (make-real (sin x))))
  (put 'cose '(integer)
       (lambda (x) (make-real (cos x))))
  (put 'atang '(integer integer)
       (lambda (x y) (make-real (atan x y))))
  (put 'squrt '(integer)
       (lambda (x) (make-real (sqrt x))))
  (put 'equ? '(integer integer) =)
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'raise '(integer) raise)
  'done)

(define (install-real-package)
  (define (raise x)
    (make-complex-from-real-imag x 0))
  (define (project x)
    (make-rational (round x) 1))

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
  (put 'sine '(real)
       (lambda (x) (tag (sin x))))
  (put 'cose '(real)
       (lambda (x) (tag (cos x))))
  (put 'atang '(real real)
       (lambda (x y) (tag (atan x y))))
  (put 'squrt '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'equ? '(real real) =)
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'raise '(real) raise)
  (put 'project '(real) project)
  'done)

;exercise 2.83
;exercise 2.85
;exercise 2.86
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (sine-rat x)
    (make-real (sin (/ (numer x) (denom x)))))
  (define (cose-rat x)
    (make-real (cos (/ (numer x) (denom x)))))
  (define (atang-rat x y)
    (make-real (atan (/ (numer x) (denom x))
                     (/ (numer y) (denom y)))))
  (define (squrt-rat x)
    (make-real (sqrt (/ (numer x) (denom x)))))
  (define (equ?-rat x y)
    (= (/ (numer x) (denom x))
       (/ (numer y) (denom y))))
  (define (raise x)
    (make-real (/ (numer x) (denom x))))
  (define (project x)
    (make-integer (round (/ (numer x) (denom x)))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'sine '(rational) sine-rat)
  (put 'cose '(rational) cose-rat)
  (put 'atang '(rational rational) atang-rat)
  (put 'squrt '(rational) squrt-rat)
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

;exercise 2.86
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (define (square x) (mul x x))
    (squrt (add (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
    (atang (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cose a)) (mul r (sine a))))

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
    (mul (magnitude z) (cose (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (define (square x) (mul x x))
    (cons (squrt (add (square x) (square y)))
          (atang y x)))

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
;exercise 2.86
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (equ?-complex z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
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
  (put 'equ? '(complex complex) equ?-complex)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'project '(complex) project)
  'done)

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

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

;test
(define (test f . args)
  (display (apply f args))
  (newline))

(define int (make-integer 5))
(define rat (make-rational 27 4))
(define real (make-real 13.7))
(define comp (make-complex-from-real-imag 5.6 10.3))
(define comp1 (make-complex-from-real-imag rat int))
(define comp2 (make-complex-from-real-imag rat real))

(test add comp1 comp2)
(test sub comp1 comp2)
(test mul comp1 comp2)
(test div comp1 comp2)
