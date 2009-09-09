;; cancel-sum List -> List
;; returns the given list with an added element symetricto the sum of the others
(define (cancel-sum list-of-integers)
  (append list-of-integers (list (* -1 (foldl + 0 list-of-integers)))))

(define (add-to-right elem lst)
  (if (empty? lst) 
      (list elem)
      (cons (car lst) (add-to-right elem (rest lst)))))

(add-to-right  '(d e f) '(a b c))


(define-struct ice-cube (mass speed))

;; momenta : Number, Number -> Number
;; computes the momentum of a solid with the given mass and speed.
(define (momenta mass speed)
  (* mass speed))

;; momentum : ice-cube ice-cube -> Number
;; (define (mementum iceA iceB))
;; ...(ice-cube-mass  iceA) ...(ice-cube-mass  iceB)
;; ...(ice-cube-speed iceA) ...(ice-cube-speed iceB)
;; Returns the sum of the momenta of the given ice-cubes
(define (momentum iceA iceB)
  (+ (momenta (ice-cube-mass iceA) (ice-cube-speed iceA))
     (momenta (ice-cube-mass iceB) (ice-cube-speed iceB))))

;; collision : ice-cube, ice-cube -> ice-cube
;; (define (mementum cubeA cubeB))
;; ...(ice-cube-mass  cubeA) ...(ice-cube-mass  cubeB)
;; ...(ice-cube-speed cubeA) ...(ice-cube-speed cubeB)
;; returns the ice-cube formed with the collision of the ice-cubes
;; passed as parameters.
(define (collision cubeA cubeB)
  (let ([M (+ (ice-cube-mass cubeA) (ice-cube-mass cubeB))])
  (make-ice-cube M (/ (momentum cubeA cubeB) M))))
      
;; tests
(define iceA (make-ice-cube 3  1))
(define iceB (make-ice-cube 1 -1))

(momenta (ice-cube-mass iceA) (ice-cube-speed iceA))
(momenta (ice-cube-mass iceB) (ice-cube-speed iceB))

(momentum iceA iceB)

(collision iceA iceB)
(make-ice-cube 4 0.5)
(equal? (collision iceA iceB) (make-ice-cube 4 0.5))
