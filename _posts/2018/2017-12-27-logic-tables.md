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
means "((a implies b) implies c)."

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

Now, to test arbitrary logical sentences, I'll need to pull out all of
the free variables.  This way, I can just build a lambda function out
of it and `eval` it.

```common-lisp
(defun free-vars (expr)
  "get the free variables out of EXPR"
  (labels ((find-nested (test obj)
         (if (atom obj)
         (and (funcall test obj) (list obj))
         (reduce #'union
             (mapcar #'(lambda (o) (find-nested test o))
                 obj))))))
  (find-nested #'(lambda (o) (and (symbolp o)
                  (not (null o))
                  (not (fboundp o))))
           expr))

;; (free-vars '(and a (or b c)))
;; => (C B A)
```

Now that we can tell how many free vars there are, we need
to produce t/nil values to push into those vars to cover all
the cases.

```common-lisp
(defun generate-tf (n)
  "generate t/nil inputs for a truth function"
  (if (= n 0)
      nil
      (do ((x '(nil) (mapcan #'(lambda (expr)
                                 (list (cons t expr) (cons nil expr)))
                             x))
           (n n (1- n)))
          ((= n 0) x))))

;; (generate-tf 3)
;; => ((T T T) (NIL T T) (T NIL T) (NIL NIL T)
;;     (T T NIL) (NIL T NIL) (T NIL NIL) (NIL NIL NIL))
```

Now that I can generate all the applicable inputs to a logical
sentence with `n` free variables, let's just run all the cases:

```common-lisp
(defun run-cases (expr)
  "EVAL a logical sentence EXPR against all combinations"
  (let* ((vars (free-vars expr))
         (func (eval `(lambda ,vars ,expr))))
    (mapcar #'(lambda (args)
                 (cons args (apply func args)))
            (generate-tf (length vars)))))
```

With the ability to run all the cases, we can easily test
if a sentence is a tautology:

```common-lisp
(defun tautologyp (expr)
  "determine if every combination of inputs results in T"
  (every #'cdr (run-cases expr)))

;; We can test it a few times...
;; (tautologyp '(or a (not a)))
;; => T

;; (tautologyp '(equiv
;;                (implies (not q) (not p))
;;                (implies p q)))
;; => T

;; (tautologyp '(equiv (implies p q) (implies q p)))
;; => NIL
```

Now the only thing left to do is produce a formatted table of the
RUN-CASES results.  I will do that tomorrow.

