#lang scheme
;; Section 14

;;;;
;;;; Problem 1
;;;;

(define-struct child (father mother name date eyes))
;; a child is a structure
;; where father and mother can be empty or a child,
;; and name and eyes are symbols,
;; and date is a number.


;; Template:
;; (define (fun-for-ft-node ft-node))
;; ... (child-father ft-node)
;; ... (child-mother ft-node)
;; ... (child-name ft-node)
;; ... (child-date ft-node)
;; ... (child-eyes ft-node)

;; get-atr child f -> list
;; adds the atr field of every node to a list
(define (get-atr atr node)
  (if (empty? node)
      empty
      (let* ([m (child-mother node)]
             [p (child-father node)]
             [a (atr node)]
             [f (cond [(empty? a) append] [else cons])])
        (f a (append (get-atr atr m) (get-atr atr p))))))

;; get-mothers : node -> list
;; list of mothers on the family tree
(define (get-mothers node)
  (map child-name (get-atr child-mother ft1)))

;; tests:
(define (fat father mother name date eyes)
         (make-child father mother name date eyes))

(define (mot father mother name date eyes)
         (make-child father mother name date eyes))

(define ft1 (make-child 
             (fat (fat empty empty 'avo 12 'red) (mot empty empty 'avo' 13 'purp) 'pa 7 'blue) 
             (mot empty (mot empty (mot empty empty 'bisa 23 'pink) 'voutra 13 'pink) 'ma '6 'grey)
             'jo 2 'black))

(get-mothers ft1)

;;;;
;;;; Problem 2
;;;;

;;