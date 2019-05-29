#lang racket

(provide (all-defined-out))

(struct pit (snake goos) #:transparent)

(struct snake (dir segs) #:transparent)

(struct posn (x y) #:transparent)

(struct goo (loc expire) #:transparent)
