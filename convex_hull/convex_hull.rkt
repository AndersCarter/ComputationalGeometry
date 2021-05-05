#lang racket

(require test-engine/racket-tests)
(require 2htdp/image)

; Constants
(define SCREEN-SIZE 500)
(define SCREEN-BOUND (/ SCREEN-SIZE 10)) ; Defines how many pixels away from the border of the screen points have to be
(define POINT-COUNT 50)

; Posn

; A two dimensional point
(define-struct posn (x y) #:transparent)

; int int -> posn
; Creates a random posn between 'min' and 'max'.
(define (random-posn min max)
  (define scale (- max min))
  (posn (+ (random scale) min)
        (+ (random scale) min)))

; [posn] -> [posn]
; Sorts the posns by their x component
(check-expect (posn-sort-byx (list (posn 1 2) (posn 3 2) (posn 2 7))) (list (posn 1 2) (posn 2 7) (posn 3 2)))
(define (posn-sort-byx posns)
  (sort posns (lambda (p q) (< (posn-x p) (posn-x q)))))

; Convex Hull Function

; [posn] -> [posn]
; Calculates the convex hull on the given list of posns
(define (convex-hull pnts)
  (define sorted (posn-sort-byx pnts))
  (define upper (half-hull sorted))
  (define lower (half-hull (reverse sorted)))
  (append upper lower))

; [posn] -> [posn]
; Determines the points within the hull half. Assumes 'pnts' is sorted by
; the x component. Returns a sorted list of posns that represent the upper
; hull
(define (half-hull pnts)
  (half-hull-recur pnts empty))

(define (half-hull-recur pnts hull)
  (cond [(empty? pnts) hull]
        [(< (length hull) 2) (half-hull-recur (rest pnts) (cons (first pnts) hull))]
        [else
         (define pnt (first pnts))
         (if (is-right? (second hull) (first hull) pnt) ; Check if the line created by the last two points in the hull and the new point make a right turn.
             (half-hull-recur (rest pnts) (cons pnt hull)) ; Adds to hull
             (half-hull-recur pnts (rest hull))) ; Remove the last hull point and check again.
         ])
  )

; Draw Functions

; posn posn image -> image
; Draws a line between the two points and places it on the image
(define (draw-line p q scene)
  (add-line scene (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

; posn image -> image
; Draws 'pnt' on 'scene'
(define (draw-point pnt scene)
  (place-image (circle 2 "solid" "blue") (posn-x pnt) (posn-y pnt) scene))

; [posn] -> image
; Draws the points on the background-scene
(define (draw-points pnts background-scene)
  (for/fold ([scene background-scene])
            ([pnt pnts])
    (draw-point pnt scene)))

; [posns] [posns] image -> image
; Draws the convex hull on the 'scene'
(define (draw-convexhull pnts hull scene)
  (define point-scene (draw-points pnts scene))
  (if (< (length hull) 2)
      point-scene
      (for/fold ([acc point-scene])
                ([pnt (drop-last hull)]
                 [next-pnt (rest hull)])
        (draw-line pnt next-pnt acc)))
  )

; Helper Functions

; list -> list
; Removes the last item in the list
(define (drop-last arr)
  (reverse (rest (reverse arr))))

; list -> list
; Removes the second element from the list
(check-expect (drop-second (list 1 2 3 4)) (list 1 3 4))
(check-expect (drop-second (list 1 2)) (list 1))
(check-error (drop-second (list 1)))

(define (drop-second L)
  (cons (first L) (rest (rest L))))

; posn posn posn -> bool
; Determines if 'pnt' lies to the right of the line created by 'start' and 'stop'.
(check-expect (is-right? (posn 1 1) (posn 1 2) (posn 2 2)) #t)
(check-expect (is-right? (posn 1 1) (posn 1 2) (posn 1 3)) #f)
(check-expect (is-right? (posn 1 1) (posn 1 2) (posn 0 1)) #f)
(define (is-right? start stop pnt)
  (define d (- (* (- (posn-x pnt) (posn-x start))
                  (- (posn-y stop) (posn-y start)))
               (* (- (posn-y pnt) (posn-y start))
                  (- (posn-x stop) (posn-x start)))))
  
  (> d 0))

; list int -> list
; Removes the first 'n' elements from the list
(check-expect (pop-n (list 1 2 3 4) 2) (list 3 4))
(check-expect (pop-n (list 1 2) 2) empty)
(check-expect (pop-n (list 1 2 3 4) 7) empty)

(define (pop-n L n)
  (cond [(>= n (length L)) empty]
        [else (for/fold ([acc L])
                        ([i n])
                (rest acc))]))

; Execution
(test)

(define pnts (build-list POINT-COUNT (lambda (x) (random-posn SCREEN-BOUND (- SCREEN-SIZE SCREEN-BOUND))))) ; Random points
(draw-convexhull pnts (convex-hull pnts) (empty-scene SCREEN-SIZE SCREEN-SIZE))
