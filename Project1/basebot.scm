;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname basebot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
; ;(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* 1/2 a (square t)) (* v t) u )))

;; you need to complete this procedure, then show some test cases

;(position 0 0 0 0)
;(position 0 0 20 0)
; (position 0 5 10 10)
; (position 2 2 2 2)
; (position 5 5 5 5)

;; Problem 2


;; define root finding procedure
(define (quadratic-formula a b c)
  
  (if (< (- (square b) (* 4 (* a c))) 0 )
      #f
      (- (square b) (* 4 (* a c)))
    ))

(define (root1 a b c)
  (if (eq? (quadratic-formula a b c) #f)
      (display "no soultion")
      (/ (+ (* -1 b) (sqrt (quadratic-formula a b c))) (* 2 a))
      ))

(define (root2 a b c)
  (if (eq? (quadratic-formula a b c) #f)
      (display "no soultion")
      (/ (- (* -1 b) (sqrt (quadratic-formula a b c))) (* 2 a))
      ))

;;(root1 5 3 6)
;;(root1 1 5 4)
;;(root2 1 5 4)

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (if (< (root1 (* 0.5 gravity) vertical-velocity elevation) (root2 (* 0.5 gravity) vertical-velocity elevation))
        (root2 (* 0.5 gravity) vertical-velocity elevation)
        (root1 (* 0.5 gravity) vertical-velocity elevation)
           )))
;;(time-to-impact 50 -10)

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))
    ))

;;(time-to-height 10 20 21)

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

;; calculate the x velocity
(define (calc-Vx velocity angel)
  (* velocity (cos (degree2radian angel))))

;; calculate the y velocity
(define (calc-Vy velocity angel)
  (* velocity (sin (degree2radian angel))))

;;(calc-Vx 25 53)

;; calculate the time it takes to get to the max point
(define (calc-time-max velocity-initial)
  (/ velocity-initial gravity))

;;calculate the max height
(define (total-height velocity angel elevation)
  (+ elevation (-
   (* (calc-Vy velocity angel) (calc-time-max (calc-Vy velocity angel)))
   (* 0.5 (* gravity (square (calc-time-max (calc-Vy velocity angel))))))))

;; calculate the total time for the ball to fall on the ground
(define (total-time velocity angel elevation)
  (+ (calc-time-max (calc-Vy velocity angel))
     (sqrt (/ (* 2 (total-height velocity angel elevation)) gravity))))

(define travel-distance-simple
(lambda (elevation velocity angle)
  (* (calc-Vx velocity angle) (total-time velocity angle elevation))))

(travel-distance-simple 60 25 53)

;; total time is 6s
;; maximum height 80m
;; horizontal motion 90m


;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
(travel-distance-simple 1 45 0) ;;i20.328927815368154 m
(total-time 45 0 1) ;; 0.45175395145262565 sec

;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
(travel-distance-simple 1 45 90) ;; 2.536544885275964e-014 m
(total-time 45 90 1) ;;9.20584217798685 sec

;; at an angle of (/ pi 4) radians or 45 degrees
(travel-distance-simple 1 45 45) ;;207.62785983753528 m
(total-time 45 45 1) ;;6.525114117972054 sec

;; what is the distance traveled in each case?
;; record both in meters and in feet


