#lang racket/gui
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)

(provide (all-defined-out))

(define fire (rs-read "sound/fire.wav")) ;The sound imported.
(define cs (rs-read "sound/crash.wav"))
(define intro (rs-read "sound/intro.wav"))
(define back (rs-read "sound/back.wav"))

(define winner (read-bitmap "images/winner.png"))
(define game-over-img (read-bitmap "images/gameover.jpg"))
(define missile4 (freeze (bitmap/file "images/missile4.png")))
(define missile3 (scale 0.3 (freeze (bitmap/file "images/boss-bul1.png"))))
(define boss (freeze (bitmap/file "images/boss.png")))
(define controls (read-bitmap "images/controls.png"))
(define health-img (freeze (bitmap/file "images/health.png"))) ;Imported Images.
(define missile1 (freeze (bitmap/file "images/missile1.png")))
(define missile2 (freeze (bitmap/file "images/missile2.png")))
(define background (freeze (bitmap/file "images/background.png")))
(define background1 (freeze (flip-vertical background)))
(define explosion (freeze  (bitmap/file "images/explosion.png")))
(define spaceship (freeze (bitmap/file "images/spaceship.png")))
(define l (list (freeze (bitmap/file "images/alien1.png")) (freeze (bitmap/file "images/alien2.png"))))
(define c1 (image->color-list explosion))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;definition of global variables
(define bosslives 20)
(define bullet-velocity 3)
(define beam-velocity 5)
(define score -50)
(define vir-score -50)
(define score-string (number->string vir-score))
(define back-posn (cons 350 -349))
(define posn (list (list (list 350 600))))
(define crash-effect '())
(define lifes 5)
(define l-c 41) ;l-c, l-c-1 and nocrash are limit for time after crash to a bullet.
(define l-c-1 40)
(define nocrash l-c)
(define speed 10) ; Movement speed of our spaceship.
(define t 60) ; Time limit.
(define x 0) ; Time state variable.
(define level-state 0)
(define pause-state 0)
(define trans-timer 0) ;Counter for transition between Levels.
(define trans-timer-limit 20)
(define fire-rate 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define health (place-image (rectangle 30 (*  lifes 140) 'solid 'red) ;Image of health bar.
                            15
                            (- 700 (* 70 lifes))
                            (rectangle 30 700 'solid 'grey)))

(define (level-finder) ;Function for finding level and string that would be shown in transition.
  (cond ((< score 120) (cons 1 "Level 1"))
        ((and (>= score 120) (< score 350)) (cons 2 "Level 2"))
        ((and (>= score 350) (< score 600)) (cons  3 "Level 3"))
        ((>= score 600) (cons 4 "Boss IS Here"))))

(define score-database (list -50 120 350 600 1150))

(define (move-spaceship w right up) ;For moving our Spaceship.
  (if (= level-state 0) w
      (if (or (> (+ (caaar w) right) 1250)  ;Limiting movement of spaceship inside the canvas only.
              (< (+ right (caaar w)) 35) 
              (> (+ up (cadaar w)) 665) 
              (< (+ up (cadaar w)) 35)) 
          w
          (cons (cons (list (+ (caaar w) right) (+ up (cadaar w))) (cdar w))
                (cdr w)))))

(define (scale-it t) ;Scaling the explosion image.
  (cons (+ 1 (/ t 30)) (- 255 (* 8 t))))

(define (op missile);;for the foldr of place-image "abstraction"
  (lambda (p q)
    (place-image missile
                 (car p)
                 (cadr p)
                 q)))

(define (op-lvl4 missile f);;for level 4 abstraction
  (lambda (p q)
    (let ((m (f p)))
      (place-image missile
                   (car m)
                   (cadr m)
                   q))))

(define (Making-Canvas w) ;W here is a List of list of positions.
  (define (func x y) ;foldr op for placing alien/enemies.
    (overlay (place-image (list-ref l (random 2))
                          (caar x)
                          (cadar x)
                          (foldr (op missile2) (empty-scene 1350 700 'transparent) (cdr x)))             
             y))
  (define (op-for-crash x y) ;Foldr op For placing crash-effect. 
    (let* ((m (scale-it (car x)))
           (color-op
            (lambda (x) (if (= 0 (color-alpha x)) (color 0 0 0 0)
                            (color (color-red x) (color-green  x) (color-blue x) (cdr m))))))
      (place-image (scale (car m)
                          (color-list->bitmap (map color-op c1) 75 75))
                        
                   (caadr x)
                   (car (cadadr x))
                   y)))

  (if (= 0 level-state) 
      (place-image  (text/font (cdr (level-finder))  ;For the transition.
                               36 'blue "Gill Sans" 'modern 'italic 'bold #t)
                    675
                    350
                    (empty-scene 1350 700 'grey))

      (place-image health-img ;Place the health.
                   1300
                   525
                   (place-image health ;placing Image Text.
                                1335
                                350
                                (place-image (text/font (string-append "Score :- " score-string) ; For displaying Score.
                                                        36 'blue "Gill Sans" 'modern 'italic 'bold #t)
                                             150
                                             30
                                             (overlay (foldr op-for-crash (empty-scene 1350 700 'transparent) crash-effect)
                                                      (if (= (car (level-finder)) 4)
                                                                          (overlay (place-image spaceship
                                                                                                (caaar w)
                                                                                                (cadr (caar w))
                                                                                                (foldr (op-lvl4 missile1 (λ(x) x)) 
                                                                                                       (empty-scene 1300 700 'transparent) 
                                                                                                       (cdar w)))
                                                                                   (overlay (foldr (op-lvl4 missile4 (λ(x) (car  x ))) 
                                                                                                   (empty-scene 1350 700 'transparent)
                                                                                                   (cadr w))
                                                                                            (foldr (op-lvl4 missile3 (λ(x) (car x)))
                                                                                                   (overlay/align  "middle" "top" boss
                                                                                                                   (place-image background
                                                                                                                675
                                                                                                                (cdr back-posn)
                                                                                                                (place-image background1
                                                                                                                             675
                                                                                                                             (car back-posn)
                                                                                                                             (empty-scene 1350 700 (color 0 0 102 150)))))
                                                                                                   (caddr w))))
                                                          ; Placing crash image.
                                                          (overlay (place-image spaceship ;Placing our spacedhip with its bulllet.
                                                                                (caaar w)
                                                                                (cadr (caar w))
                                                                                (foldr (op missile1) 
                                                                                       (empty-scene 1350 700 'transparent) 
                                                                                       (cdar w)))

                                                                   (foldr func ;foldr for placing alien and there spaceship.
                                                                          (place-image background ;null element is backgroung image moving downward.
                                                                                       675
                                                                                       (cdr back-posn)
                                                                                       (place-image background1 
                                                                                                    675
                                                                                                    (car back-posn)
                                                                                                    (empty-scene 1350 700 (color 0 0 102 150))))
                                                                          (cdr w))))))))))

(define (generate-bullets w) ;Appending all element of list w with a posn corresponding to bullets.
  (define (func x)
    (append x (list (list (caar x) (cadar x)))))
  (if (= (car (level-finder)) 4) 
      (append (list (car w)) (list (append (cadr w) new-bullets))  (cddr w))
      (cons (car w) (map func (cdr w)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;For level 4.
(define (mover x);;;;helper function of move in level 4
  (let((posn (car x))
       (vel (cadr x)))
    (cons  (cons (+ (car posn) (car vel)) (list (+ (cadr posn) (cadr vel)))) (list vel))))

(define vx (* bullet-velocity (sin (* 3.14 (/ 30 180)))))
(define vy (* bullet-velocity (cos (* (/ 30 180)))))

(define vel-list (list (list (- vx) vy)
                       (list 0 bullet-velocity);;;;;;for the velocity of boss-bullets in level 4
                       (list vx vy)))

(define new-bullets (append (map (λ(t) (cons '(470 100) (list t))) vel-list);;;;making the new bullet-list of level 4
                            (map (λ(t)(cons '(870 100) (list t))) vel-list)))

(define (generate-beam w)
  (let*((dx (- (caaar w) 670));;;;;;;;;;;generating the beam in level 4
        (dy (- (cadaar w) 180))
        (d (sqrt (+ (expt dx 2) (expt dy 2)))))
    (append (list (car w)) (list (cadr w)) (list (append (caddr w) (list (cons '(670 180)
                                                                               (list (list (* beam-velocity (/ dx d))
                                                                                           (* beam-velocity (/ dy d)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (delete w) ;for deleating bullets that had gone out of the canvas.
  (define (op x y)
    (cond ((> (cadar x) 700) y)
          (else (cons x y))))
  (define (op1 x y)
    (cond [( < (cadr x) 0) y]
          [else (cons x y)]))
  (if (and (not (= score 600)) (= (car (level-finder)) 4))
      (list (foldr op1 '() (car w)) (foldr op '() (cadr w)) (foldr op '() (caddr w)))
      (cons (car w) (foldr op '() (cdr w)))))

(define (close-enough? r); a and b are position i.e (list x y),
  (lambda (a b) (if (and (not (= score 600)) (= (car (level-finder)) 4))
                    (if (<= (+ (expt (- (caar a) (car b)) 2)
                           (expt (- (cadar a) (cadr b)) 2))
                        (* r r))
                    #t #f)
                    (if (<= (+ (expt (- (car a) (car b)) 2)
                           (expt (- (cadr a) (cadr b)) 2))
                        (* r r))
                    #t #f))))
(define p1 40)
(define p2 10)
(define close-alien-bullet? (close-enough? p1)) ;close-enough for alien and bullet.
(define close-bullet-bullet? (close-enough? p2))
(define close-alien-alien? (close-enough? 35))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;for level4

(define (close-bullet-boss? bullet-posn)
  (let((x (car bullet-posn))
       (y (cadr bullet-posn)))
    (or (and (>= x 435) (<= x 965) (>= y 0) (<= y 35))
        (and (>= x 435) (<= x 565) (>= y 35) (<= y 100))
        (and (>= x 602) (<= x 797) (>= y 35) (<= y 100))
        (and (>= x 834) (<= x 964) (>= y 35) (<= y 100))
        (and (>= x 654) (<= x 744) (>= y 100) (<= y 180)))))

(define (crash-boss w)
  (define (op x y)
    
    (if (close-bullet-boss? x) (begin (play cs)
                                      (set! bosslives (- bosslives 1))
                                      (set! score (+ score 20))
                                      (set! vir-score (+ vir-score 20))
                                      (set! score-string (number->string vir-score))
                                      (set! crash-effect (append crash-effect (list (list 0 (list (car x) (cdr x))))))
                                      y)
        (cons x y))) 
  (cons (cons (caar w) (foldr op '() (cdar w))) (cdr w)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (crash l1 l2) ;Check and remove elements if they are crashing in two list.
  (define (bullet-helper l a) ;Foldr op for removing alien enemies if crashing.
    (cond ((null? l) '())
          ((close-alien-bullet? (car l) a)
           (begin (play cs)
                  (set! score (+ score 20))
                  (set! vir-score (+ vir-score 20))
                  (set! score-string (number->string vir-score))
                  (set! crash-effect (append crash-effect (list (list 0 (list (car a) (cdr a))))))
                  (set! l2 (cons (cons (- (caar l2) 1500) (cdar l2)) (cdr l2)))
                  (cdr l)))
          (else (cons (car l) (bullet-helper (cdr l) a)))))

  (define (crash-bullet lb1 lb2) ;Foldr op for removing crashing bullets.
    (define (func x l)
      (define (helper x l lis)
        (cond ((null? l) (cons #f lis))
              ((close-bullet-bullet? x (car l)) (cons #t (append lis (cdr l))))
              (else (helper x (cdr l) (append lis (list (car l)))))))
      
      (let* ((pqr (helper x l '())))
        (begin (set! lb2 (cdr pqr)) (car pqr))))
    (define (op p q)
      (if (func p lb2) q (cons p q)))
    (cons (foldr op '() lb1) lb2))

  (let* ((op1 (cons (car l1) (bullet-helper (cdr l1) (car l2)))))
    (begin (set! l1 op1)
           (cons (cons (car l1)
                       (car (crash-bullet (cdr l1) (cdr l2))))
                 (cons (car l2) (cdr (crash-bullet (cdr l1) (cdr l2))))))))

(define speed-bullet 10)
(define speed-alien 5) ;speed of enemies.

(define (crash-alien w) ;for crashing between our spaceship and enemies and there bullets.
  (define (helper a l)
    (define (op x)
      (let ((k (crash (car w) x)))
        (begin (set! w (cons (car k) (cdr w))) (cdr k))))
    (map op l))
  (let ((help (helper (car w) (cdr w))))
    (begin (set! w (cons (car w) help)) w)))

(define (move-general l f w) ;A general function for movement of aliens and bullets.
  (lambda(function)          ;Dependent on a function for path of alien.
    (cond ((eq? f "down") (if (> (caar l) 0)
                              (cons (list (+ (caar l) (car function))
                                          (+ (cadar l) (cdr function)))
                                    (map (λ(t) (list (car t) (+ (cadr t) speed-bullet))) (cdr l)))
                              (cons (list  (caar l)  (+ (cadar l) 5))
                                    (map (λ(t) (list (car t) (+ (cadr t) speed-bullet))) (cdr l)))))
                            
          ((eq? f "up") (cons (car l)
                              (map (λ(t) (list (car t) (- (cadr t) speed-bullet))) (cdr l)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define move-level1 ;movement of alien for level1 (Straight path).
  (lambda (l f w)
    ((move-general l f w) (cons 0 speed-alien))))

(define move-level2 ;movement of alien for level2. (following our sppaceship).
  (lambda (l f w)
    (let ((d (sqrt (+ (expt (- (caaar w) (caar l)) 2) (expt (- (cadaar w) (cadar l)) 2)))))
      ((move-general l f w) (if (eq? f "up") 
                                (cons 0 0)
                                (cons (* speed-alien (/ (- (caaar w) (caar l)) d))  
                                      (* speed-alien (/ (- (cadaar w) (cadar l)) d))))))))

(define move-level3 ;movement of alien for level3. (sinosoidial path).
  (lambda (l f w)
    ((move-general l f w) (cons (* 300 speed-alien 0.01 (cos (/ (cadar l) 100))) speed-alien))))

(define move-level4
  (lambda (l f w)
    (cond[(eq? f "up") (cons (car l)
                             (map (λ(t) (list (car t) (- (cadr t) speed-bullet))) (cdr l)))]
         [(equal? f "down") (map (λ(t) (mover t)) l)])))

(define move-list (list move-level1 move-level2 move-level3 move-level4)) ;list of movement function.
(define move (car move-list))  ;Final movement function for using.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define background-speed 2) ;movement speed of background.
(define (for-background-move) ;For moving background.
  (begin (set! back-posn (cons (+ background-speed (car back-posn)) (+ background-speed (cdr back-posn))))
         (cond ((>= (car back-posn) 1050) (set! back-posn (cons (- (cdr back-posn) 699) (cdr back-posn))))
               ((>= (cdr back-posn) 1050) (set! back-posn (cons (car back-posn) (- (car back-posn) 699)))))))

(define crash-limit 30)
(define (for-crash-effect) ;for upadating values in crash-effect.
  (begin (set! crash-effect (map (lambda (x) (cons (+ 1 (car x)) (cdr x))) crash-effect)) ;updating counter of each element in crash-effect.
         (cond ((null? crash-effect) '()) 
               ((> (caar crash-effect) crash-limit) (set! crash-effect (cdr crash-effect))))));Removing elemets whose counter goes out of crash-limit.

(define (crash-spaceship spaceship-posn w1) ;function for decreasing our health.
  (define (op x y)
    (and (foldr op1 #t x) y))
  (define (op2 x y)
    (and (not (close-alien-alien? (car x) spaceship-posn)) y))
  (define (op1 x y)
    (and (not (close-alien-bullet? x spaceship-posn)) y))
  (define (op4 radius life-decrement)
    (λ(a b)(if ((close-enough? radius) a spaceship-posn)
               (begin (play cs)
                      (set! lifes (- lifes life-decrement))
                      (let ((p (if (<= lifes 0) 0 lifes)))
                        (set! health (place-image (rectangle 30 (*  p 140) 'solid 'red)
                                                15
                                                (- 700 (* 70 p))
                                                (rectangle 30 700 'solid 'grey))))
                      (set! crash-effect (append crash-effect (list (list 0 (list (car spaceship-posn) (cdr spaceship-posn))))))
                      b)
               (cons a b))))
  (if (and (not (= score 600)) (= (car (level-finder)) 4))
        (list  (car w1)  (foldr (op4 40 1) '() (cadr w1)) (foldr (op4 56 3) '() (caddr w1)))
      (if (< nocrash l-c-1) #t (and (foldr op2 #t w1) (foldr op #t w1)))))

(define (move-bullets w) ;overall on-tick function.
  (if (= pause-state 0)  ; for any pause between game.
      (if (= 0 level-state) ;will show the transition if condition is satisfied.
          (begin (set! trans-timer (+ 1 trans-timer))
                 (let ((k (car (level-finder))))
                   (if (>= trans-timer trans-timer-limit) ;For stopping the transition.
                       (begin (set! trans-timer 0)
                              (set! w posn)
                              (set! crash-effect '())
                              (set! speed-alien (cond ((= 3 k) 3)
                                                      (else 5)))
                              (set! fire-rate (cond ((= 1 k) 3)
                                                    ((= 2 k) 4)
                                                    ((= 3 k) 6)
                                                    (else 3)))
                              (cond ((= (car (level-finder)) 4) (set! w (list (list (list 350 600)) '() '())))
                                    (else (set! posn (list (list (list 350 600))))))
                              (set! move (list-ref move-list (- k 1)))
                              (set! score (+ score 50))
                              (set! vir-score (+ vir-score 50))
                              (set! score-string (number->string vir-score))
                              (set! level-state 1))
                       w)) w)
      
          (if (= (car (level-finder)) 4)
              (begin (set! w (crash-boss w))
                     (for-crash-effect)
                     (set! w (delete w))
                     (set! w (cond [(= x t) (begin (set! x 0) (generate-beam w))]
                                   [(= (modulo x (quotient t fire-rate)) 0) (generate-bullets w)]
                                   [else w]))
                     (set! x (+ x 1))
                     (set! w  (list (move (car w) "up" w) (move (cadr w) "down" w) (move (caddr w) "down" w)))
                     (set! w (crash-spaceship (caar w) w))
                     w)
              
              (begin
                     (set! w (crash-alien w)) ;on tick fucntion applcable in between level.
                     (for-background-move)
                     (for-crash-effect)
                     (set! w (delete w))
                     (set! w (cond [(= x t) (begin (set! x 0) (append (generate-bullets w) (list (list (list (+ 300 (random 670)) -30)))))]
                                   [(= (modulo x (quotient t fire-rate)) 0) (generate-bullets w)]
                                   [else w]))
                     (set! x (+ x 1))
                     (if (> nocrash l-c-1) (set! nocrash l-c)
                         (set! nocrash (+ 1 nocrash)))
                     (set! w (cons  (move (car w) "up" w) (map (λ(t) (move t "down" w)) (cdr w))))
                     (let ((death (not (crash-spaceship (caar w) (cdr w)))))
                       (cond (death (begin (cond ((> nocrash l-c-1) (begin (set! nocrash 0) (set! lifes (- lifes 1))
                                                                           (set! health (place-image (rectangle 30 (*  lifes 140) 'solid 'red)
                                                                                                     15
                                                                                                     (- 700 (* 70 lifes))
                                                                                                     (rectangle 30 700 'solid 'grey))))))
                                           (play  cs)
                                           (set! crash-effect (append crash-effect (list (list 0 (list (caaar w) (cdaar w))))))))))
                     (cond ((or (= score -50) (= score 120) (= score 350) (= score 600) (= score 1150))
                            (begin (play intro) (set! level-state 0))))
                     w))) w))
  
(define (change w a-key) ;Movement of our spaceship - Keyboard input.
  (cond 
    [(or (key=? a-key "left") (key=? a-key "a"))  (move-spaceship w (- speed) 0)]
    [(or (key=? a-key "right") (key=? a-key "d")) (move-spaceship w speed 0)]
    [(or (key=? a-key "up")  (key=? a-key "w"))  (move-spaceship w 0 (- speed))]
    [(or (key=? a-key "down") (key=? a-key "s")) (move-spaceship w 0 speed)]
    [(key=? a-key "escape") (set! pause-state 1) (send dialog show #t) w]
    [else w]))
  
(define (singleton? l)
  (if (= (length l) 1) #t #f))
(define fire-time-delay 120)

(define (create-bullet w x y event) ;for creating our bullet on click.
  (if (= 0 level-state)
      w
      (begin (cond [(singleton? (car w)) w]
                   ((< (cadr (cadar w)) -10) (set! w (cons (cons (caar w) (cddar w)) (cdr w))))
                   (else  w))
             (cond ((mouse=? event "button-down")
                    (if (and (> (- (cadaar w) (cadr (list-ref (car w) (- (length (car w)) 1)))) 0)
                             (< (- (cadaar w) (cadr (list-ref (car w) (- (length (car w)) 1)))) fire-time-delay))
                        w
                        (begin (play fire) (cons (append (car w) (list (list (caaar w) (- (cadaar w) 30)))) (cdr w)))))
                   (else w)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define font1 (make-object font% 20 		 	 	 	 
                'roman	 	 	 
                'normal	 	 	 
                'bold	 	 	 	 
                #f	 	 	 	 
                'default	 	 	 	 
                #t	 	 	 
                'aligned))

(define win (new frame% 
                       [label "Congratulations"]
                       [width 300]
                       [height 300]
                       ))

(new message% [parent win] [label winner])
(new button% [parent win]
     [label "Back"]
     [min-height  20]
     [font font1]
     [callback (lambda (button event)
                 (send win show #f))])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exit-when w) ;Closing game.
  (cond ((and (<= lifes 0) (= (car (level-finder)) 4)) (begin (stop)
                                                         (define game-over (new frame% 
                                                                                [label "Game Over"]
                                                                                [width 300]
                                                                                [height 300]
                                                                                ))

                                                         (new message% [parent game-over] [label game-over-img])
                                                         (new message% [parent game-over] 
                                                              [label (string-append "Your Score :- " (number->string vir-score))]
                                                              [font font1])
                                                         (new button% [parent game-over]
                                                              [label "Back"]
                                                              [min-height  20]
                                                              [font font1]
                                                              [callback (lambda (button event)
                                                                          (send game-over show #f))])
                                                         (send game-over show #t) (set! bosslives 20) (<= lifes 0)))
        ((<= bosslives 0) (begin (stop)
                                 (send win show #t)
                                 (set! bosslives 20) #t))
        ((and (= nocrash 10) (= 0 lifes)) #t)
        (else #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;for pausing game.
(define dialog (new dialog%	 ;dialog box after pausing.
                    [label "What would you like to do?"]	 
                    [parent #f]
                    [width 500]
                    [height 60]
                    [x 400]
                    [y 350]
                    [style '(close-button)]
                    [alignment '(center center)]
                    [min-height 60]))
(define dialog-panel1 (new horizontal-panel%	 
                           [parent dialog]	 
                           ;   	 	[enabled enabled]	 
                           [vert-margin 0]	 
                           ;   	 	[horiz-margin horiz-margin]	 
                           ;   	 	[border border]	 
                           ;;   	 	[spacing spacing]	 
                           [alignment '(center center)]	 
                           [min-width 50]	 
                           [min-height 60]))
(new button% [parent dialog-panel1]
     [label "Resume"]
     [min-height  40]
     [callback (lambda (button event)
                 (set! pause-state 0) (send dialog show #f))]
     )
(new button% [parent dialog-panel1]
     [label "Quit"]
     [min-height  40]
     [callback (lambda (button event)
                 (stop)
                 (set! nocrash 10) (set! lifes 0)
                 (send dialog show #f))]
     )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (p)
  (big-bang posn ;<-- initial state
            (to-draw  Making-Canvas) ;<-- redraws the world
            (on-key change)
            (on-tick move-bullets 0.05);<-- process the event of key press
            (on-mouse create-bullet)
            (stop-when exit-when)
            (close-on-stop #t))
  (stop))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frame (new frame% [label "SPACE-WAR"]
                   [width 400]
                   [height 300]
                   [style '(fullscreen-button)]))
                   
(send frame show #t)

(define help-frame (new frame% 
                        [label "Controls"]
                        [width 300]
                        [height 300]))

(new message% [parent help-frame] [label controls])

(new button% [parent help-frame]
     [label "Back"]
     [min-height  20]
     [font font1]
     [callback (lambda (button event)
                 (send help-frame show #f))]
     )

(define help (new horizontal-panel%
                  [parent frame]
                  [style '(border)]))
                  
(define game (new vertical-panel%
                  [parent help]
                  
                  [alignment '(left center)]
                  [min-width 300]
                  [min-height 50]))

(define d (new horizontal-panel%	 
               [parent game]	  
               [vert-margin 0]	 	 
               [alignment '(center top)]	 
               ))
(define a (new horizontal-panel%	 
               [parent game]	 	 
               [vert-margin 0]	 	 
               [alignment '(center top)]	 
               ))
(define b (new horizontal-panel%	 
               [parent game]	 	 
               [vert-margin 0]	  
               [alignment '(center top)]	 
               ))
(define c (new horizontal-panel%	 
               [parent game]	 	 
               [vert-margin 0]	 	 
               [alignment '(center top)]	 
               ))

(new button% [parent a]
     [label "Start"]
     [min-height  40]
     [font font1]
            
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (set! lifes 5)
                 (set! health (place-image (rectangle 30 (*  lifes 140) 'solid 'red)
                                           15
                                           (- 700 (* 70 lifes))
                                           (rectangle 30 700 'solid 'grey)))
                 (set! vir-score -50)
                 (set! pause-state 0)
                 (cond ((= (car (level-finder)) 4) (set! posn (list (list (list 350 600)) '() '())))
                       (else (set! posn (list (list (list 350 600))))))
                 (play intro)
                 (play back)
                 (set! crash-effect '())
                 (set! level-state 0)
                 (p))]
     )

(define level-choice (new choice% [parent b]
                          [choices (list "Level 1" "Level 2" "Level 3" "Level 4")]
                          [label "Select Level "]
                          [callback (lambda (c e) (begin (set! vir-score -50)
                                                         (set! score 
                                                               (list-ref score-database 
                                                                         (send level-choice get-selection)))))]
                          [font font1]
                          ))

(new button% [parent c]
     [label "Help"]
     [min-height  40]
     [font font1]
     [callback (lambda (button event)
                 (send help-frame show #t))]
     )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For Dialog Box and frame.