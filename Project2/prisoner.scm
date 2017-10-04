(require racket/list)
;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play-loop-iter strat0 strat1 count history0 history1 limit)
  (cond ((= count limit) (print-out-results history0 history1 limit))
        (else (let ((result0 (strat0 history0 history1))
                    (result1 (strat1 history1 history0)))
                (play-loop-iter strat0 strat1 (+ count 1)
                                (extend-history result0 history0)
                                (extend-history result1 history1)
                                limit)))))
(define (play-loop strat0 strat1)
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (begin (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (car (cdr scores)) number-of-games)))
    (newline))))

(define (get-scores history0 history1)
  (get-scores-helper history0 history1 0 0))
(define (get-scores-helper history0 history1 score0 score1)
  (cond ((empty-history? history0)
         (list score0 score1))
        (else (let ((game (make-play (most-recent-play history0)
                                     (most-recent-play history1))))
                (get-scores-helper (rest-of-plays history0)
                                   (rest-of-plays history1)
                                   (+ (get-player-points 0 game) score0)
                                   (+ (get-player-points 1 game) score1))))))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
    (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)

(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))
(define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))

(define (EGALITARIAN  my-history other-history)
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

(define a-play (make-play "c" "d"))

;; Problem 1

(define extract-entry
  (lambda (pair pairs-list)
   (if (empty? pairs-list) empty
    (if (equal? pair (car (car pairs-list)))
           (list (car (car pairs-list)) (car (cdr (car pairs-list))))
        (extract-entry pair (cdr pairs-list))))))



;;Problem 2

;;(play-loop NASTY PATSY) ;;Nasty always win against pats Player 1 Score:  5 ;;Player 2 Score:  0
;;(play-loop PATSY PATSY) ;; both Patsys do good Player 1 Score:  3 ;;Player 2 Score:  3
;;(play-loop EGALITARIAN PATSY) ;; Patsy does quite well against egalitarian Player 1 Score:  3 ;;Player 2 Score:  3
;;(play-loop EGALITARIAN EGALITARIAN) ;; they do well against each other Player 1 Score:  3 ;;Player 2 Score:  3
;;(play-loop EGALITARIAN SPASTIC) ;; spastic do much better Player 1 Score:  5/2 ;;Player 2 Score:  85/48
;;(play-loop NASTY SPASTIC) ;; Nasty is winning against spastic ;; Player 1 Score:  11/4 ;;Player 2 Score:  9/16
;;(play-loop PATSY SPASTIC) ;; Patsy doesn't do well against him Player 1 Score:  4/3 ;;Player 2 Score:  37/9
;;(play-loop EYE-FOR-EYE SPASTIC) ;; equal results Player 1 Score:  224/93 ;;Player 2 Score:  224/93
;;(play-loop EYE-FOR-EYE EYE-FOR-EYE) ;; equal scores Player 1 Score:  3 ;;Player 2 Score:  3



;;Problem 3
;; The new version is certainly much faster.
;; in the old version we are redoing the calculations everytime while we could have just added the results of the last game
;; and saved ourself the trouble of wasting the time on a calculations we have already done

;;Problem 4
(define (list-length lst)
  (cond ((null? lst) 0)
        (else (+ 1 (list-length (cdr lst))))))

(define (second-most-recent-play history)
  (car (cdr history)))

(define (EYE-FOR-TWO-EYES my-history other-history)
  (if (empty-history? my-history)
      "c"
      (if (> (list-length other-history) 2)
          (if (and (string=? (most-recent-play other-history) "d") (string=? (second-most-recent-play other-history) "d"))
              "d"
              "c")
           "c")))

;;(play-loop EYE-FOR-TWO-EYES EYE-FOR-EYE) ;; They both end up playing fair because eye for eye will copy the other player move and eye for two eyes will
;; never have two defection to start defecting. Player 1 Score:  3 ;;Player 2 Score:  3
;;(play-loop EYE-FOR-TWO-EYES EGALITARIAN) same results as the previous game
;;(play-loop EYE-FOR-TWO-EYES NASTY) ;; nasty win because the other player will cooperate twice before defecting for the rest of the game

;;Problem 5
(define (find-history-item history-list item)
  (if (= item 1)
      (car history-list)
      (find-history-item (cdr history-list) (- item 1))))

(define (compare-list-items history-list n)
  (if (>= 1 n)
      #t
      (if (and (string=? (find-history-item history-list (- n 1)) "c") (string=? (find-history-item history-list  n) "c"))
              (compare-list-items history-list (- n 1))
              #f)))
      
 
(define (EYE-FOR-n-EYES my-history other-history)
  (let ((n 5))
  (if (empty-history? my-history)
      "c"
      (if (>= (list-length other-history) n)
          (if (compare-list-items other-history n)
              "c"
              "d")
           "c"))))

;;(play-loop EYE-FOR-n-EYES NASTY) ;; It's take him a long time to realize whats wrong Player 1 Score:  18/19 ;;Player 2 Score:  23/19
;;(play-loop EYE-FOR-n-EYES EGALITARIAN) ;;shockingly it's a draw :) Player 1 Score:  3 ;;Player 2 Score:  3
;;(play-loop EYE-FOR-n-EYES SPASTIC) ;;apparently he likes randomness Player 1 Score:  293/101 ;;Player 2 Score:  78/101

;;Problem 6

;;define global state
(define rotating-state 0)
(define (increment-state)
  (set! rotating-state (+ rotating-state 1)))

(define (reset-state)
  (set! rotating-state 0))

(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (lambda(my-history other-history)
  (if (> freq0 rotating-state)
      (begin0
        (strat0 my-history other-history)
        (increment-state))
      (if (> (+ freq0 freq1) rotating-state)
          (begin0
            (strat1 my-history other-history)
            (increment-state))
          (begin0
            (strat0 my-history other-history)
            (reset-state))))))

;; (play-loop (make-rotating-strategy PATSY NASTY 10 24) SPASTIC) ;; Player 1 Score:  122/47 ;;Player 2 Score:  77/47


;; Problem 7 ;;

;;define global state
(define strategy-loop 0)
(define (increment-strategy)
  (set! strategy-loop (+ strategy-loop 1)))

(define (reset-strategy)
  (set! strategy-loop 0))

(define (make-higher-order-spastic strategy-list)
  (lambda
      (my-history other-history)
    (begin0
      ((list-ref strategy-list strategy-loop) my-history other-history)
      (if (>= strategy-loop (- (list-length strategy-list) 1))
          (reset-strategy)
          (increment-strategy)
          ))))

;;(play-loop (make-higher-order-spastic (list PATSY NASTY)) SPASTIC) ;; Player 1 Score:  203/101 ;; Player 2 Score:  238/101 
;;(play-loop (make-higher-order-spastic (list PATSY NASTY)) EYE-FOR-EYE) ;; Player 1 Score:  228/91 ;;Player 2 Score:  228/91

;; Problem 8 ;;
(define (gentle gentleness-factor strat)
  (lambda (my-history other-history)
    (if (< gentleness-factor (random))
        (strat my-history other-history)
        (PATSY my-history other-history))))

;; (play-loop (gentle 0.5 NASTY) SPASTIC) ;; very sneaky hehe ;;Player 1 Score:  235/106 ;;Player 2 Score:  125/53

(define (slightly-gentle-Nasty)
  (gentle 0.1 NASTY))

(define (slightly-gentle-Eye-for-Eye)
  (gentle 0.1 EYE-FOR-EYE))

;;(play-loop (slightly-gentle-Nasty) SPASTIC) ;; Player 1 Score:  273/94 ;;Player 2 Score:  34/47
;;(play-loop (slightly-gentle-Eye-for-Eye) SPASTIC) ;;Player 1 Score:  98/45 ;;Player 2 Score:  221/90


;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;

;;Problem 9

(define *game-association-list*3
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))

(define (play-loop-iter-3 strat0 strat1 strat2 count history0 history1 history2 limit)
  (cond ((= count limit) (print-out-results-3 history0 history1 history2 limit))
        (else (let ((result0 (strat0 history0 history1 history2))
                    (result1 (strat1 history1 history0 history2))
                    (result2 (strat2 history2 history1 history0)))
                (play-loop-iter-3 strat0 strat1 strat2 (+ count 1)
                                (extend-history result0 history0)
                                (extend-history result1 history1)
                                (extend-history result2 history2)
                                limit)))))
(define (play-loop-3 strat0 strat1 strat2)
  (play-loop-iter-3 strat0 strat1 strat2 0 the-empty-history the-empty-history the-empty-history
		  (+ 90 (random 21))))

(define (get-scores-3 history0 history1 history2)
  (get-scores-helper-3 history0 history1 history2 0 0 0))

(define (get-scores-helper-3 history0 history1 history2 score0 score1 score2)
  (cond ((empty-history? history0)
         (list score0 score1 score2))
        (else (let ((game (make-play (most-recent-play history0)
                                     (most-recent-play history1)
                                     (most-recent-play history2))))
                (get-scores-helper-3 (rest-of-plays history0)
                                   (rest-of-plays history1)
                                   (rest-of-plays history2)
                                   (+ (get-player-points-3 0 game) score0)
                                   (+ (get-player-points-3 1 game) score1)
                                   (+ (get-player-points-3 2 game) score2))))))
(define (get-point-list-3 game)
    (cadr (extract-entry game *game-association-list*3)))
(define (get-player-points-3 num game)
  (list-ref (get-point-list-3 game) num))

(define (print-out-results-3 history0 history1 history2 number-of-games)
  (let ((scores (get-scores-3 history0 history1 history2)))
    (begin (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (car (cdr scores)) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (car (cdr (cdr scores))) number-of-games))))))


(define (NASTY-3 history1 history2 history3)
  "d")

(define (PATSY-3 history1 history2 history3)
  "c")

(define (SPASTIC-3 history1 history2 history3)
  (if (= (random 2) 0)
      "c"
      "d"))

;;(play-loop-3 PATSY-3 PATSY-3 PATSY-3) ;; working perfectly :)
;;(play-loop-3 SPASTIC-3 PATSY-3 NASTY-3) ;; working perfectly :)

;;Problem 10

(define (tough-Eye-for-Eye history1 history2 history3)
  (if (empty-history? history1)
      "c"
      (if (or (string=? (most-recent-play history2) "d") (string=? (most-recent-play history3) "d"))
          "d"
          "c")))

;;(tough-Eye-for-Eye `("d") `("d") `("c")) tested- works perfectly as descriped

(define (soft-Eye-for-Eye history1 history2 history3)
  (if (empty-history? history1)
      "c"
      (if (and (string=? (most-recent-play history2) "d") (string=? (most-recent-play history3) "d"))
          "d"
          "c")))
;;(soft-Eye-for-Eye `("d") `("d") `("d")) tested- works perfectly as descriped

;;(play-loop-3 tough-Eye-for-Eye tough-Eye-for-Eye NASTY-3) ;; Player 1 Score:  100/99 Player 2 Score:  100/99 Player 3 Score:  103/99
;;(play-loop-3 soft-Eye-for-Eye soft-Eye-for-Eye NASTY-3) ;; Player 1 Score:  2 Player 2 Score:  2 Player 3 Score:  5
;;(play-loop-3 soft-Eye-for-Eye soft-Eye-for-Eye SPASTIC-3) ;; Player 1 Score:  37/12 Player 2 Score:  37/12 Player 3 Score:  107/24
;;From my obeservation NASTY-3 will beat any of them because he will start cheating and it will take them sometime to realise his style.
;; randomness is not the eye-for-eye favorite thing.

;;Problem 11

(define (make-combined-strategies strat1 strat2 combine)
  (lambda(history1 history2 history3)
    (let ((r1 (strat1 history1 history2))
          (r2 (strat2 history1 history3)))
      (combine r1 r2))))




;; Problem 12
;;helper function
(define (all-but-last l) (reverse (cdr (reverse l))))

(define  (make-history-summary-iter history0 history1 history2 list1 list2 list3)
  (let ((others-length (- (length history0) 1))
        (player0-length (- (length history0) 2)))
    (cond ((< player0-length 0) (list list1 list2 list3))
           ((and (string=? (list-ref history2 others-length) "d") (string=? (list-ref history1 others-length) "d"))
            (cond 
                   ((string=? (list-ref history0 player0-length) "c")
                      (make-history-summary-iter (all-but-last history0) (all-but-last history1) (all-but-last history2)
                                                 list1 list2 (list (+ 1 (car list3)) (cadr list3) (+ 1 (caddr list3)))))
                   ((string=? (list-ref history0 player0-length) "d")
                      (make-history-summary-iter (all-but-last history0) (all-but-last history1) (all-but-last history2)
                                                 list1 list2  (list (car list3) (+ 1 (cadr list3)) (+ 1 (caddr list3)))))))
           
           ((or (and (string=? (list-ref history2 others-length) "c") (string=? (list-ref history1 others-length) "d"))
               (and (string=? (list-ref history2 others-length) "d") (string=? (list-ref history1 others-length) "c")))
            (cond 
                  ((string=? (list-ref history0 player0-length) "c")
                     (make-history-summary-iter (all-but-last history0) (all-but-last history1) (all-but-last history2)
                                                list1 (list (+ 1 (car list2)) (cadr list2) (+ 1 (caddr list2))) list3))
                  ((string=? (list-ref history0 player0-length) "d")
                     (make-history-summary-iter (all-but-last history0) (all-but-last history1) (all-but-last history2)
                                                list1 (list (car list2) (+ 1 (cadr list2)) (+ 1 (caddr list2))) list3))))
           
           ((and (string=? (list-ref history2 others-length) "c") (string=? (list-ref history1 others-length) "c"))
            (cond 
                   ((string=? (list-ref history0 player0-length) "c")
                      (make-history-summary-iter (all-but-last history0) (all-but-last history1) (all-but-last history2)
                                                 (list (+ 1 (car list1)) (cadr list1) (+ 1 (caddr list1))) list2 list3))
           ((string=? (list-ref history0 player0-length) "d")
              (make-history-summary-iter (all-but-last history0) (all-but-last history1) (all-but-last history2)
                                         (list (car list1) (+ 1 (cadr list1)) (+ 1 (caddr list1))) list2 list3)))))))

(define (make-history-summary history1 history2 history3)
  (make-history-summary-iter history1 history2 history3 `(0 0 0) `(0 0 0) `(0 0 0)))
        

;;(define hist1 (list "c" "c" "d" "d" "c" "d" "c" "c"))
;;(define hist2 (list "c" "c" "c" "d" "d" "c" "d" "c"))
;;(define hist3 (list "c" "c" "d" "d" "d" "c" "c" "c"))
;;(make-history-summary hist1 hist2 hist3) ;; (list (list 3 0 3) (list 1 1 2) (list 0 2 2)) works as intended


;; Problem 13
(define summary (make-history-summary
(list "c" "c" "c" "c") ;hist-0
(list "d" "d" "d" "c") ;hist-1
(list "d" "d" "c" "c")))
;;(display summary)

(define (calc-prob num1 num2)
  (if (= num1 0)
      0
      (/ num1 num2)))
      
(define (get-probability-of-c summary-list)
  (let* ((first-list (list-ref summary-list 0))
        (second-list (list-ref summary-list 1))
        (third-list (list-ref summary-list 2))
        (first-prob (calc-prob (list-ref first-list 0) (list-ref first-list 2)))
        (second-prob (calc-prob (list-ref second-list 0) (list-ref second-list 2)))
        (third-prob (calc-prob (list-ref third-list 0) (list-ref third-list 2))))
  (list first-prob second-prob third-prob)))

;;(get-probability-of-c summary) ;; (list 1 1 1)

;;(define new-summary (make-history-summary
;;(list "c" "c" "c" "d" "c")
;;(list "d" "c" "d" "d" "c")
;;(list "d" "c" "c" "c" "c")))

;;(get-probability-of-c new-summary) ;; (list 0.5 1 '())

;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
(define (test-entry expected-values actual-values) 
   (cond ((null? expected-values) (null? actual-values)) 
         ((null? actual-values) #f) 
         (
              (= (car expected-values) (car actual-values)) 
          (test-entry (cdr expected-values) (cdr actual-values))) 
         (else #f))) 

(define (is-he-a-fool? hist0 hist1 hist2) 
   (test-entry (list 1 1 1) 
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt) 
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)  
                            (else 0)))
                   (get-probability-of-c (make-history-summary hist0 
                                                               hist1
                                                               hist2)))))

;;Problem 14
(define (is-he-soft hist0 hist1 hist2)
  (test-entry (list 1 1 0)
              (map (lambda (elt) 
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)  
                            (else 0)))
                   (get-probability-of-c (make-history-summary hist0 
                                                               hist1
                                                               hist2)))))

(is-he-soft (list "d" "c" "c" "c" "c") (list "d" "d" "d" "d" "c") (list "d" "d" "c" "c" "c"))
(define soft-summmary (make-history-summary
(list "d" "c" "c" "c" "c")
(list "d" "d" "d" "d" "c")
(list "d" "d" "c" "c" "c")))
(display soft-summmary)


(define (dont-tolerate-fools history1 history2 history3)
  (let ((total-games (length history1)))
    (if (< total-games 10)
        "c"
        (if (or (could-he-be-a-fool? history2 history1 history3)
                (could-he-be-a-fool? history3 history1 history2))
            "d"
            "c"))))

;;(play-loop-3 PATSY-3 PATSY-3 dont-tolerate-fools) Player 1 Score:  4 ;;Player 2 Score:  4 ;;Player 3 Score:  4


        
