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

;;(travel-distance-simple 60 25 53)

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
;;(travel-distance-simple 1 45 0) ;;i20.328927815368154 m
;;(total-time 45 0 1) ;; 0.45175395145262565 sec

;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;;(travel-distance-simple 1 45 90) ;; 2.536544885275964e-014 m
;;(total-time 45 90 1) ;;9.20584217798685 sec

;; at an angle of (/ pi 4) radians or 45 degrees
;;(travel-distance-simple 1 45 44) ;;207.62785983753528 m
;;(total-time 45 45 1) ;;6.525114117972054 sec

;; what is the distance traveled in each case?
;; record both in meters and in feet

;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 1)
(define distance 0)


;; check the angel if it's bigger than 90
;; if it's not increase and check distance
;; compare new distance with old distance

(define (recur-func velocity elevation angle prev-distance best-angle)
  (if (< 90 angle)
      best-angle
      (if (< (travel-distance-simple elevation velocity angle) prev-distance)
          (recur-func velocity elevation (+ angle alpha-increment) prev-distance best-angle)
          (recur-func velocity elevation (+ angle alpha-increment) (travel-distance-simple elevation velocity angle) angle))))
          

  
(define find-best-angle
  (lambda (velocity elevation)
        (recur-func velocity elevation 0 0 0)
    ))

;;(find-best-angle 25 1) ;;207.53716710226922

;; find best angle
;;apparently the best angel is 45

;; try for other velocities
;; for bigger velocities the angel doesn't change much

;; try for other heights
;; as the height grows the angel grows smaller

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define integrate
  (lambda (x0 y0 u0 v0 t0 dt g m beta)
    (let ((dx (* u0 dt))
          (dy (* v0 dt))
          (du (* -1 (* beta (* u0 (* dt (* (/ 1 m) (sqrt(+ (square u0) (square v0)))))))))
          (dv (* -1 (* dt (+ (* beta (* v0 (/ 1 m) (sqrt(+ (square u0) (square v0))))) g)))))
      (if (< y0 0)
          x0
          (integrate (+ x0 dx) (+ y0 dy) (+ u0 du) (+ v0 dv) (+ t0 dt) dt g m beta)
          ))))

(define travel-distance
  (lambda (elevation velocity angle)
    (let ((v0 (calc-Vy velocity angle))
          (u0 (calc-Vx velocity angle))
          (y0 elevation)
          (x0 0)
          (t0 0)
          (dt 0.01)
          (g gravity))
          (integrate x0 y0 u0 v0 t0 dt g mass beta))))

;;(travel-distance 0 3 80) ;; 92.50801605244197 m
;;(travel-distance 1 40 45) ;; 82.78053770266135 m
          


;; RUN SOME TEST CASES

;; what about Denver?

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance


;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

(define find-time
  (lambda (velocity distance elevation)
    (let ((starting-angle -90)
          (best-angle 0)
          (old-time 0))
      (recur-distance velocity distance elevation starting-angle best-angle old-time))))

(define recur-distance 
  (lambda (velocity distance elevation starting-angle best-angle old-time)
    (if (< 90 starting-angle)
        old-time
        (if (almost-equal distance (travel-distance elevation velocity starting-angle))
            (if (> (travel-time elevation velocity starting-angle) old-time)
                (recur-distance velocity distance elevation (+ 1 starting-angle) starting-angle (travel-time elevation velocity starting-angle))
                (recur-distance velocity distance elevation (+ 1 starting-angle) best-angle old-time))
        (recur-distance velocity distance elevation (+ 1 starting-angle) best-angle old-time)))))

(define almost-equal
  (lambda (distance target-distance)
    (< (abs (- distance target-distance)) 1)))
        


(define integrate-time
  (lambda (x0 y0 u0 v0 t0 dt g m beta)
    (let ((dx (* u0 dt))
          (dy (* v0 dt))
          (du (* -1 (* beta (* u0 (* dt (* (/ 1 m) (sqrt(+ (square u0) (square v0)))))))))
          (dv (* -1 (* dt (+ (* beta (* v0 (/ 1 m) (sqrt(+ (square u0) (square v0))))) g)))))
      (if (< y0 0)
          t0
          (integrate-time (+ x0 dx) (+ y0 dy) (+ u0 du) (+ v0 dv) (+ t0 dt) dt g m beta)
          ))))

(define travel-time
  (lambda (elevation velocity angle)
    (let ((v0 (calc-Vy velocity angle))
          (u0 (calc-Vx velocity angle))
          (y0 elevation)
          (x0 0)
          (t0 0)
          (dt 0.01)
          (g gravity))
          (integrate-time x0 y0 u0 v0 t0 dt g mass beta))))


;;(find-time 35 90 1) ;; the weakoutfielder can't reach 90 meters
;;(find-time 45 60 1) ;; a strong outfilder can reach 60 meters in 1.84 seconds
;;(find-time 45 90 1) ;; It will take 5.35 seconds to reach 90 meters


;; Problem 8

(define travel-distance-bounce
  (lambda (velocity angle elevation bounces)
    (let ((distance-without-bounce (travel-distance elevation velocity angle)))
      (calc-bounce (/ velocity 2) angle elevation bounces distance-without-bounce))))
          


(define calc-bounce
  (lambda (velocity angle elevation bounces old-distance)
    (let ((bounce-distance (travel-distance 0 velocity angle)))
    (if (> bounces 0)
        (calc-bounce (/ velocity 2) angle 0 (- bounces 1) (+ old-distance bounce-distance))
        old-distance))))
;;(travel-distance-bounce 55 80 1 4) ;; 61.733650076285336 meter
;;(travel-distance-bounce 45 45 1 1) ;; 130.61071759996312 meter
;;(travel-distance-bounce 45 45 1 2) ;; 142.54687837034763 meter
;;(travel-distance-bounce 45 45 1 3) ;; 145.7535162820798 meter

(define integrate-bounce
  (lambda (x0 y0 u0 v0 t0 dt g m beta)
    (let ((dx (* u0 dt))
          (dy (* v0 dt))
          (du (* -1 (* beta (* u0 (* dt (* (/ 1 m) (sqrt(+ (square u0) (square v0)))))))))
          (dv (* -1 (* dt (+ (* beta (* v0 (/ 1 m) (sqrt(+ (square u0) (square v0))))) g)))))
      (if (< y0 0)
          (list x0 (+ u0 du) (+ v0 dv))
          (integrate-bounce (+ x0 dx) (+ y0 dy) (+ u0 du) (+ v0 dv) (+ t0 dt) dt g m beta)
          ))))

(define travel-distance-bounce2
  (lambda (elevation velocity angle)
    (let ((v0 (calc-Vy velocity angle))
          (u0 (calc-Vx velocity angle))
          (y0 elevation)
          (x0 0)
          (t0 0)
          (dt 0.01)
          (g gravity))
          (integrate-bounce x0 y0 u0 v0 t0 dt g mass beta))))

(define calc-velocity
  (lambda (u0 v0)
    (sqrt(+ (square u0) (square v0)))))

(define get-bounces
  (lambda (info-list angle bounces old-distance)
    (let ((distance (+ old-distance (car info-list)))
          (velocity (calc-velocity (car (cdr info-list)) (car (cdr (cdr info-list))))))
      (if (< bounces 1)
          distance
          (get-bounces (travel-distance-bounce2 0 velocity angle) angle (- bounces 1) distance)))))
          
          
             
    

(define calc-bounce2
 (lambda (elevation velocity angle bounces)
   (let ((info-list (travel-distance-bounce2 elevation velocity angle)))
     (get-bounces info-list angle bounces 0))))
         


;; comparing with the previous problem we see that the output makes sense
;;(calc-bounce2 1 45 45 1) ;;133.90337383615076 meter
;;(calc-bounce2 1 45 45 2) ;;160.4762106488039 meter
