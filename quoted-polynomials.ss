
;; A simple test:
;;
;; $$x + y = y + x = 1x^1 + \left(1y^1\right)x^0.$$
(add '(polynomial x . ((1 1))) '(polynomial y . ((1 1))))
=> (add '(polynomial y . ((1 1))) '(polynomial x . ((1 1))))
=> '(polynomial x . ((1 1) (0 (polynomial y . ((1 1))))))

;; A more complicated test:
;;
;; $$\begin{aligned}
;; &\phantom{=} (yx^3 + 2)(y + x^2 + 1) \\
;; &= \left(1y^1\right)x^5 + \left(1y^2 + 1y^1\right)x^3 + 2x^2
;; + \left(2y^1 + 2y^0\right)x^0.
;; \end{aligned}$$
(mul '(polynomial x . ((3 (polynomial y . ((1 1)))) (0 2)))
     '(polynomial y . ((1 1) (0 (polynomial x . ((2 1) (0 1)))))))
=> '(polynomial x . ((5 (polynomial y . ((1 1))))
                     (3 (polynomial y . ((2 1) (1 1))))
                     (2 2)
                     (0 (polynomial y . ((1 2) (0 2))))))
