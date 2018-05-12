(in-package :pong)

;;;; State and Config ---------------------------------------------------------
(defvar *ball* nil)
(defvar *player-left* nil)
(defvar *player-right* nil)
(defvar *running* nil)
(defvar *score-left* nil)
(defvar *score-right* nil)

(defparameter *fps* 24.0)
(defparameter *width* 60)
(defparameter *height* 21)
(defparameter *player-height* 5.0)


;;;; Utils --------------------------------------------------------------------
(defun center (distance)
  (/ distance 2.0))


;;;; Entities -----------------------------------------------------------------
(defstruct ball
  (x (center *width*))
  (y (center *height*))
  (vx (* (if (randomp) 1.0 -1.0) (random-range 6.0 15.0)))
  (vy (* (if (randomp) 1.0 -1.0) (random-range 2.0 10.0))))

(defstruct player
  x
  (y (center *height*))
  (height *player-height*))

(defun player-left (player)
  (- (player-x player) 0.5))

(defun player-top (player)
  (- (player-y player) (/ (player-height player) 2.0)))


;;;; Collision ----------------------------------------------------------------
(defgeneric aabb (object))

(defmethod aabb ((ball ball))
  (values (- (ball-x ball) 0.5)
          (- (ball-y ball) 0.5)
          1.0
          1.0))

(defmethod aabb ((player player))
  (values (player-left player)
          (player-top player)
          1.0
          (player-height player)))


(defun overlapsp (n1 m1 n2 m2)
  ;; a          b
  ;; ------------
  ;;        ------------
  ;;        c          d
  ;; or
  ;;        a          b
  ;;        ------------
  ;; ------------
  ;; c          d
  (let ((a (min n1 m1))
        (b (max n1 m1))
        (c (min n2 m2))
        (d (max n2 m2)))
    (and (<= c b)
         (<= a d))))


(defun collidesp (o1 o2)
  (nest
    (multiple-value-bind (x1 y1 w1 h1) (aabb o1))
    (multiple-value-bind (x2 y2 w2 h2) (aabb o2))
    (and (overlapsp x1 (+ x1 w1)
                    x2 (+ x2 w2))
         (overlapsp y1 (+ y1 h1)
                    y2 (+ y2 h2)))))


(defun perturb-ball (ball)
  (mulf (ball-vx ball) (random-gaussian 1.1 0.1))
  (mulf (ball-vy ball) (random-gaussian 1.0 0.2)))

(defun check-player-collisions (ball player delta)
  (when (collidesp ball player)
    (callf (ball-vx ball) #'-)
    (perturb-ball ball)
    (incf (ball-x ball) (* 2.5 delta (ball-vx ball))))) ; hack

(defun check-wall-collisions (ball delta)
  (unless (in-range-p 0.5 (ball-y ball) (- *height* 0.5))
    (callf (ball-vy ball) #'-)
    (incf (ball-y ball) (* 2.5 delta (ball-vy ball)))))

(defun check-kill-plane-collisions (ball)
  (unless (in-range-p 0 (ball-x ball) *width*)
    (if (minusp (ball-x ball))
      (incf *score-right*)
      (incf *score-left*))
    (setf *ball* (make-ball))))

(defun check-collisions (delta)
  (check-kill-plane-collisions *ball*)
  (check-player-collisions *ball* *player-left* delta)
  (check-player-collisions *ball* *player-right* delta)
  (check-wall-collisions *ball* delta))


;;;; Drawing ------------------------------------------------------------------
(defun draw-ball (canvas ball)
  (boots:draw canvas
              (truncate (ball-y ball))
              (truncate (ball-x ball))
              "*"))

(defun draw-player (canvas player)
  (iterate
    (with col = (truncate (player-x player)))
    (for row :from (round (player-top player)))
    (repeat (round (player-height player)))
    (boots:draw canvas row col "#")))


(defun draw-playfield (canvas)
  (boots:clear canvas)
  (draw-ball canvas *ball*)
  (draw-player canvas *player-left*)
  (draw-player canvas *player-right*))

(defun draw-outer (canvas)
  (boots:clear canvas)
  (boots:border canvas)
  (let ((l (aesthetic-string *score-left*))
        (r (aesthetic-string *score-right*)))
    (boots:draw canvas 0 1 l)
    (boots:draw canvas 0 (- (+ 2 *width*)
                            1
                            (length r))
                r)))


;;;; Ticks --------------------------------------------------------------------
(defgeneric tick (object delta))

(defmethod tick ((ball ball) delta)
  (incf (ball-x ball) (* delta (ball-vx ball)))
  (incf (ball-y ball) (* delta (ball-vy ball))))

(defmethod tick ((player player) delta))


;;;; Movement -----------------------------------------------------------------
(defun clamp-player (player)
  (let ((h (/ (player-height player) 2.0)))
    (zapf (player-y player)
          (clamp h (- *height* h) %))))

(defun move-player (player direction)
  (incf (player-y player)
        (ecase direction
          (:up -1.01)
          (:down 1.01)))
  (clamp-player player)
  (values))


;;;; Game ---------------------------------------------------------------------
(defun update (delta)
  (tick *ball* delta)
  (tick *player-left* delta)
  (tick *player-right* delta))

(defun physics (delta)
  (check-collisions delta))

(defun init ()
  (setf *running* t
        *score-left* 0
        *score-right* 0
        *ball* (make-ball)
        *player-left* (make-player :x 1.5)
        *player-right* (make-player :x (- *width* 1.5))))

(defun handle-input ()
  (iterate
    (for event = (boots:read-event-no-hang))
    (while event)
    (case event
      (:up (move-player *player-right* :up))
      (:down (move-player *player-right* :down))
      ((#\w #\k) (move-player *player-left* :up))
      ((#\s #\j) (move-player *player-left* :down))
      (#\q (setf *running* nil)))))

(defun main ()
  (iterate
    (while *running*)

    (timing real-time :per-iteration-into delta :seconds t)

    (handle-input)
    (update delta)
    (physics delta)
    (boots:blit)

    (timing real-time :per-iteration-into time-taken :seconds t)
    (for extra-time = (- (/ *fps*) time-taken))
    (when (plusp extra-time)
      (sleep extra-time))))

(defun toplevel ()
  (boots:with-boots ()
    (boots:with-layer (:width (+ 2 *width*)
                       :height (+ 2 *height*))
        (boots:canvas () 'draw-outer)
      (boots:with-layer (:width *width* :height *height*)
          (boots:canvas () 'draw-playfield)
        (init)
        (main)))))
