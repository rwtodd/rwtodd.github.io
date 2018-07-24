---
layout: post
title: "Logic Tables"
date: 2017-12-27 21:00
categories: lisp logic
---
I've been reading some logic, and thought it might be
fun to auto-generate some truth tables.

So the idea is to use a logical expression as input, and get a
truth table as output.  We'll rely heavily on the lisp compiler
itself to evaluate the terms.

Input might be: `(truth-table '(and a b (or c d)))`

Lisp gives us AND, OR, and NOT... we need IMPLIES and EQUIV. Note that
I'll allow chains of implications or equivalences (so that `(implies a b c)`
means "(implies (a implies b) c)."

```common-lisp
(defun implies (p &rest ps)
  (if (null ps)
      p
      (apply #'implies (if p (car ps) t)
                       (cdr ps))))

(defun equiv (p &rest ps)
  (cond ((null ps) p)
    (t (apply #'equiv (eql p (car ps)) (cdr ps)))))
```

Now, to test arbitrary logical sentences, I'll need to pull out a list of
the free variables.  This way, I can build a lambda function out
of it and `eval` it.  To get the free variables, I'll flatten the
sentence and then filter out anything with a function definition:

```common-lisp
(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun free-vars (expr)
  "get the free variables out of EXPR in alphabetical order"
  (let ((vars nil))
    (mapc #'(lambda (x) (pushnew x vars))
          (remove-if #'(lambda (x) (or (not (symbolp x)) (fboundp x)))
                     (flatten expr)))
    (sort vars #'string-lessp :key #'symbol-name)))

(free-vars '(and a (or b c)))
;; => (A B C)
```

Now we know how many free vars there are, and we're going to need
to be able to generate all the combinations of true/false for those
inputs.  This will help us build the truth table and some other
fun functions.  Since the numbrer of combinations grows very quickly,
let's generate them lazily:

```common-lisp
(ql:quickload "rt-lazygen")
(use-package "RT-LAZYGEN")

(defun generate-tf (n)
  "lazily generate t/nil inputs for a logical sentence of arity N"
  (lg-map #'(lambda (x)
              (let ((result nil))
                (dotimes (idx n result)
                  (push (logbitp idx x) result))))
          (lg-range 0 (expt 2 n))))

(lg-to-list (generate-tf 3))
;; => ((T T T) (NIL T T) (T NIL T) (NIL NIL T)
;;     (T T NIL) (NIL T NIL) (T NIL NIL) (NIL NIL NIL))
```

Now that I can generate all the applicable inputs to a logical
sentence with `n` free variables, let's just run all the cases:

```common-lisp
(defun run-cases (vars expr)
  (let ((func (eval `(lambda ,vars ,expr))))
    (lg-map #'(lambda (args)
                (cons args (apply func args)))
            (generate-tf (length vars)))))
```

With the ability to run all the cases, we can easily test
if a sentence is a tautology:

```common-lisp
(defun tautologyp (expr)
  "determine if every combination of inputs results in T"
  (lg-every #'cdr (run-cases (free-vars expr) expr)))

;; We can test it a few times...
(tautologyp '(or a (not a)))
;; => T

(tautologyp '(equiv
                (implies (not q) (not p))
                (implies p q)))
;; => T

(tautologyp '(equiv (implies p q) (implies q p)))
;; => NIL
```

We can also easily look for values that make a sentence true.

```common-lisp
(defun true-for (expr)
  "Identify an input for EXPR that makes it true, by brute force"
  (let ((vars (free-vars expr)))
    (multiple-value-bind (foundp case)
        (lg-some #'cdr (run-cases vars expr))
      (if foundp
          (mapcar #'list vars (car case))
          nil))))

(true-for '(equiv (implies a b c) (and (not b) c) (or c d)))
;; => ((A NIL) (B NIL) (C NIL) (D T))

;; also works when a sentence is always false:
(true-for '(equiv a (not a)))
;; => NIL
```

Now the only thing left to do is produce a formatted table of the
RUN-CASES results.  For this, we mostly lean on FORMAT.

```common-lisp
(defun truth-table (expr)
  "generate a truth table for EXPR"
  (flet ((to-tf (x) (if x "T" "F")))
    (let* ((vars (free-vars expr))
           (fmt-string  "~{ ~a~^ |~} || ~a~%")
           (heading (format nil fmt-string vars expr)))
      (format t "~a~a~%"
              heading
              (map 'string #'(lambda (ch) (if (char= ch #\|) ch #\-)) heading))
      (lg-for-each #'(lambda (x)
                       (format t fmt-string
                               (mapcar #'to-tf (car x))
                               (to-tf (cdr x))))
                   (run-cases vars expr)))))


;; two examples... one is a tautologyy, and one is not:

(truth-table '(equiv (and a (or b c)) (or (and a b) (and a c))))
;; =>
;;  A | B | C || (EQUIV (AND A (OR B C)) (OR (AND A B) (AND A C)))
;; ---|---|---||---------------------------------------------------
;;  F | F | F || T
;;  F | F | T || T
;;  F | T | F || T
;;  F | T | T || T
;;  T | F | F || T
;;  T | F | T || T
;;  T | T | F || T
;;  T | T | T || T
 
(truth-table '(implies (implies (and b c) a)
                       (and (implies c a) (implies b a))))
;; =>
;;  A | B | C || (IMPLIES (IMPLIES (AND B C) A) (AND (IMPLIES C A) (IMPLIES B A)))
;; ---|---|---||-------------------------------------------------------------------
;;  F | F | F || T
;;  F | F | T || F
;;  F | T | F || F
;;  F | T | T || T
;;  T | F | F || T
;;  T | F | T || T
;;  T | T | F || T
;;  T | T | T || T
```

Not bad! There are times when leveraging the host language's reader
and interpreter pay off, and this is one of them!  This kind of thing
isn't hard in C or Rust (for example), but it is enough of a hassle
that you wouldn't just code it up in a few minutes for laughs like
you can with lisp.

