#lang racket

(require
  2htdp/image
  2htdp/universe
  "constants.rkt"
  "entities.rkt"
  "functions.rkt")

(define HEAD-IMG (bitmap "art/snake_head.png"))
(define SEG-IMG (bitmap "art/snake_body.png"))
(define GOO-IMG (bitmap "art/goo.png"))
(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-DOWN-IMG (flip-vertical HEAD-UP-IMG))

(define (text/menlo s color size) (text/font s color size "Menlo" 'script 'normal 'normal #f))

(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))

(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (my-map goo-loc goos))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

(define (img-list+scene posns img scene)
  (my-foldr (lambda (p s) (img+scene p img s)) scene posns))

(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir)    HEAD-UP-IMG]
                   [(string=? "down" dir)  HEAD-DOWN-IMG]
                   [(string=? "left" dir)  HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (render-end w)
  (overlay (text/menlo "Game Over" ENDGAME-TEXT-SIZE "red")
           (render-pit w)))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos  (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

(define (init-game)
  (pit (snake "right" (list (posn 1 1)))
                 (build-list MAX-GOO (lambda (x) (fresh-goo)))))

(define (start-new)
  (big-bang (init-game)
    (on-tick next-pit TICK-RATE)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when dead? render-end)))
