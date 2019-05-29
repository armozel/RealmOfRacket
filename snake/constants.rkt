#lang racket

(provide (all-defined-out))

(define SEG-SIZE 15)
(define SIZE 30)
(define MAX-GOO 5)
(define EXPIRATION-TIME 150)
(define TICK-RATE 1/10)
(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))
(define ENDGAME-TEXT-SIZE 32)