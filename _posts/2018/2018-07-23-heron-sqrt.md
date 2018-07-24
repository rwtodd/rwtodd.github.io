---
layout: post
title: "Heron's SQRT"
date: 2018-07-23 21:00
categories: lisp 
---

While doing other things I had a [lecture by Sussman][1] playing on Youtube,
and his discussion of Heron's algorithm for square roots caught my ear.
I couldn't resist coding it up to play with it for a second (a form of
procrastination).

I used my `rt-lazygen` lazy lists.  `Heron` is an infinite stream of
guesses, each getting closer to the desired square root:

```common-lisp
(ql:quickload "rt-lazygen")
(use-package "RT-LAZYGEN")

(defun heron (num init)
  "A stream of improved guesses of sqrt(NUM), starting with INIT"
  (lg-iterate #'(lambda (g) (/ (+ g (/ num g)) 2.0d0)) 
              init))
```

So, for instance, we can take the first 20 guesses at sqrt(40149),
starting from the horrible guess: 1.0...

```common-lisp
(lg--> (heron 40149 1) (lg-take 20) (lg-to-list))
;; => (1 20075.0d0 10038.4999750934d0 5021.249738510471d0 2514.622778350662d0
;;  1265.2944950999367d0 648.5127239886488d0 355.21103606229434d0
;;  234.1198094296821d0 202.80446451480697d0 200.3867395660892d0
;;  200.37215428479686d0 200.37215375395854d0 200.37215375395854d0
;;  200.37215375395854d0 200.37215375395854d0 200.37215375395854d0
;;  200.37215375395854d0 200.37215375395854d0 200.37215375395854d0)
```

... and see how close each of those was:

```common-lisp
(mapcar #'(lambda (x) (abs (- 40149 (* x x)))) *)
;; => (40148 4.02965476d8 1.007313327499502d8 2.517279993649147d7 6283178.717400002d0
;;  1560821.159330204d0 380419.7531751773d0 86025.88014044857d0
;;  14663.085167390665d0 980.6508271376006d0 5.8453939276587334d0
;;  2.127304396708496d-4 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)
```

So even after a single refinement, the square of our guess was within 4 
of our target.

Finally, we can define a helper function to refine our guess until a tolerance
is met, and print the answer and the number of steps:

```common-lisp
(defun do-heron (num &optional (guess 1) (tolerance 0.01d0))
  (let ((gs (heron num guess)))
    (do ((guess (funcall gs) (funcall gs))
         (count 1 (1+ count)))
     ((< (abs (- num (* guess guess))) tolerance) (values guess count)))))

;; try it...
(do-heron 40149)
;; => 200.37215428479686d0
;; => 12
```

Question: how fast does the number of required guesses grow compared to the size of the number?

```common-lisp
(loop :for x = 2 :then (* x x) 
      :for result = (multiple-value-bind (_ c) (do-heron x) c) 
      :collect (list x result) 
      :while (< result 20))
;; => ((2 3) (4 4) (16 6) (256 8) (65536 13) (4294967296 21))
```

Looks roughly logarithmic compared to NUM...

```common-lisp
(mapcar #'log '(16 256 65536 4294967296))
;; => (2.7725887 5.5451775 11.090355 22.18071)
```
Yep, similar!  I like how easy it is to just play
around with these things in lisp.

[1]: https://www.youtube.com/watch?v=fAY0_pesZ6s&index=34&list=WL
