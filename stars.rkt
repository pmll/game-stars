#lang racket

(require racket/draw)

; config
(define width 600)
(define height 700)
(define min-diam 1)
(define max-diam 3)
(define min-intensity 0.1)
(define max-intensity 0.5)
(define cycle-length 6)
(define stars-qty 200)
(define file-name "stars")

; colours
(define blue (make-object color% 157 180 255))
(define white (make-object color% 255 249 249))
(define red (make-object color% 255 187 123))
(define yellow (make-object color% 255 245 236))
(define colours (list blue white red yellow))

; x, y, diam, colour, cycle stage
(define (make-star)
  (define diam (+ min-diam (random (+ (- max-diam min-diam) 1))))
  (list (random (- width diam))
        (random (- height diam))
        diam
        (list-ref colours (random (length colours)))
        (random cycle-length)))

(define (make-stars qty)
  (if (zero? qty)
      '()
      (cons (make-star) (make-stars (- qty 1)))))

(define halfway-cycle (* cycle-length 0.5))

(define intensity-step (/ (- max-intensity min-intensity) halfway-cycle))
 
(define (cycle->intensity cycle)
  (if (< cycle halfway-cycle)
      (+ min-intensity (* cycle intensity-step))
      (+ min-intensity (* (- cycle-length cycle) intensity-step))))
      
(define (make-frame stars frame-no)
  (define stars-bitmap (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap stars-bitmap]))
  (define (plot-star star)
    (send dc set-brush (cadddr star) 'solid)
    (send dc set-pen white 0 'transparent)
    (send dc set-alpha (cycle->intensity (remainder (+ (list-ref star 4) frame-no) cycle-length)))
    (send dc draw-ellipse (car star) (cadr star) (caddr star) (caddr star)))
  (for-each plot-star stars)
  (send stars-bitmap save-file (format "~a-~a.png" file-name frame-no) 'png))

(define (make-frames stars)
  (for-each (lambda (frame-no) (make-frame stars frame-no)) (range cycle-length)))

(make-frames (make-stars stars-qty))