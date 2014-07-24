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
;exercise 2.88
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
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
  (define (neg-rat x)
    (make-rat (- (numer x))
              (denom x)))
  (define (=zero?-rat x)
    (= (numer x) 0))
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
    (atan (imag-part z) (real-part z)))
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
  ;this function is changed becuase the representation of polynomial is changed.
  (define (raise z)
    (make-polynomial 'x (list (tag z))))
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

;exercise 2.87
;exercise 2.88
;exercise 2.89
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
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (neg-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t (first-term L)))
          (adjoin-term
           (make-term (order t) (neg (coeff t)))
           (neg-terms (rest-terms L))))))

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
  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))
  (define (=zero?-poly p)
    (empty-termlist? (term-list p)))

  ;;interfacr to rest of system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'neg '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
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

(define int (make-integer 100))
(define rat (make-rational 30 4))
(define real (make-real 12.5))
(define comp (make-complex-from-real-imag 7.5 10.5))
(define p1 (make-polynomial 'y (list 1.0 1.0)))
(define p2 (make-polynomial 'x (list int rat 0 real)))
(define p3 (make-polynomial 'x (list comp p1 0 10.5)))

(test sub p1 p1)
(test sub p2 p3)
(test sub p3 p2)
