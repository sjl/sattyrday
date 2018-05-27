(in-package :afk)

;;;; Config/State -------------------------------------------------------------
(defparameter *map-width* 80)
(defparameter *map-height* 20)
(defparameter *screen-width* (+ *map-width* 2))
(defparameter *screen-height* (+ *map-height* 2 2))
(defparameter *glyphs*
  (concatenate 'string
               "abcdefghijklmnopqrstuvwxyz"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               "0123456789"
               "{}()!@#$%^&*[]"))

(defparameter *non-kitten-item-count* 10)


(defvar *items* nil)
(defvar *map* nil)
(defvar *player* nil)
(defvar *kitten* nil)
(defvar *status* nil)


;;;; Utils --------------------------------------------------------------------
(defun press-any-key ()
  (iterate
    (while (member (boots:read-event) '(:resize)))))


(defun random-glyph ()
  (random-elt *glyphs*))

(defun random-unoccupied-location ()
  (let ((row (random *map-height*))
        (col (random *map-width*)))
    (if (item-at row col)
      (random-unoccupied-location)
      (values row col))))


(defun item-at (row col)
  (aref *map* row col))

(defun place-item (item row col)
  (setf (row item) row
        (col item) col
        (aref *map* row col) item)
  (push item *items*))

(defun remove-item (item)
  (setf (aref *map* (row item) (col item)) nil
        (row item) nil
        (col item) nil)
  (deletef *items* item))

(defun move-item (item row col)
  (remove-item item)
  (place-item item row col))


;;;; Initialization -----------------------------------------------------------
(defun initialize ()
  (setf *items* nil
        *status* nil
        *map* (make-array (list *map-height* *map-width*)
                :initial-element nil))
  (values))


;;;; Descriptions -------------------------------------------------------------
(chancery:define-string material
  "gold"
  "silver"
  "wood"
  "plastic"
  "metal"
  "ceramic"
  "leather")

(chancery:define-string animal
  "dog"
  "mouse"
  "zebra"
  "elephant"
  "hawk"
  "panda")

(chancery:define-string given-name
  "Tom"
  "James"
  "Mary"
  "Anne")

(chancery:define-string random-description
  (#(material chancery:a) animal "statue")
  (given-name "the" animal))


;;;; Population ---------------------------------------------------------------
(defclass* (item :conc-name "") ()
  (row col glyph color))

(defclass* (non-kitten-item :conc-name "") (item)
  ((description :accessor description :initarg :description)))

(defclass* (kitten :conc-name "") (item) ())
(defclass* (player :conc-name "") (item) ())

(defun make-kitten ()
  (make-instance 'kitten :glyph (random-glyph)))

(defun make-player ()
  (make-instance 'player :glyph #\?))

(defun make-non-kitten-item ()
  (make-instance 'non-kitten-item :glyph (random-glyph)
    :description (random-description)))

(defun populate ()
  (setf *player* (make-player)
        *kitten* (make-kitten))
  (multiple-value-call #'place-item *player* (random-unoccupied-location))
  (multiple-value-call #'place-item *kitten* (random-unoccupied-location))
  (do-repeat *non-kitten-item-count*
    (multiple-value-call #'place-item (make-non-kitten-item)
      (random-unoccupied-location))))


;;;; Winning ------------------------------------------------------------------
(defun win (&aux (countdown 5))
  (boots:with-layer (:width 14 :height 3)
      (boots:canvas () #'boots:border)
    (boots:with-layer (:width 12 :height 1)
        (boots:canvas
          ()
          (lambda (canvas)
            (boots:clear canvas)
            (boots:draw canvas 0 (- 5 countdown) (string (glyph *player*)))
            (boots:draw canvas 0 (+ 6 countdown) (string (glyph *kitten*)))))
      (iterate (boots:blit)
               (sleep 1)
               (decf countdown)
               (until (minusp countdown))))))


;;;; Introduction -------------------------------------------------------------
(defparameter *intro* '("alienfindskitten"
                        "By Steve Losh (c) 2018"
                        ""
                        "In this game, you are alien (?).  Your job is to find kitten.  This task is complicated by the existence of various things which are not kitten.  Alien must touch items to determine if they are kitten or not.  The game ends when alienfindskitten.  Alternatively, you may end the game by hitting the q key or a good old-fashioned Ctrl-C."
                        ""
                        "Press any key to start."))


(defun draw-intro (canvas)
  (boots:clear canvas)
  (iterate
    (for row :from 0 :below (boots:height canvas))
    (for line :in (bobbin:wrap *intro* (min 80 (boots:width canvas))))
    (boots:draw canvas row 0 line)))

(defun intro ()
  (boots:with-layer ()
      (boots:canvas () #'draw-intro)
    (boots:blit)
    (press-any-key)))


;;;; Game ---------------------------------------------------------------------
(defun draw-status (canvas)
  (boots:clear canvas)
  (boots:draw canvas 0 0 "alienfindskitten")
  (when *status*
    (boots:draw canvas 1 0 *status*)))

(defun draw-map-border (canvas)
  (let ((s (format nil "+~A+" (make-string *map-width* :initial-element #\-))))
    (boots:draw canvas 0 0 s)
    (boots:draw canvas (1+ *map-height*) 0 s)
    (iterate (for row :from 1)
             (repeat *map-height*)
             (boots:draw canvas row 0 "|")
             (boots:draw canvas row (1+ *map-width*) "|"))))

(defun draw-item (canvas item)
  (boots:draw canvas (1+ (row item)) (1+ (col item)) (string (glyph item))))

(defun draw-map (canvas)
  (boots:clear canvas)
  (draw-map-border canvas)
  (map nil (curry #'draw-item canvas) *items*))


(defun target (row col direction)
  (multiple-value-bind (dr dc)
      (ecase direction
        (:n (values -1  0))
        (:s (values  1  0))
        (:e (values  0  1))
        (:w (values  0 -1))
        (:nw (values -1 -1))
        (:ne (values -1  1))
        (:sw (values  1 -1))
        (:se (values  1  1)))
    (values (+ row dr)
            (+ col dc))))

(defun move (direction)
  (multiple-value-bind (row col)
      (target (row *player*) (col *player*) direction)
    (when (array-in-bounds-p *map* row col)
      (if-let ((item (item-at row col)))
        (etypecase item
          (kitten (return-from move :win))
          (non-kitten-item (setf *status* (description item))))
        (move-item *player* row col))))
  nil)

(defun handle-input (event)
  (case event
    (#\h (move :w))
    (#\j (move :s))
    (#\k (move :n))
    (#\l (move :e))
    (#\y (move :nw))
    (#\u (move :ne))
    (#\b (move :sw))
    (#\n (move :se))
    (#\q :quit)
    (t (setf *status* (structural-string event)) nil)))

(defun main ()
  (boots:with-layer ()
      (boots:shelf ()
        (boots:stack (:width *screen-width*)
          (boots:canvas (:height 2) #'draw-status)
          (boots:canvas (:height (- *screen-height* 2)) #'draw-map)
          (boots:shelf ()))
        (boots:shelf ()))
    (iterate
      (boots:blit)
      (for event = (boots:read-event))
      (case (handle-input event)
        (:quit (return-from main :quit))
        (:win (win) (return-from main :win))))))



;;;; Toplevel -----------------------------------------------------------------
(defun run ()
  (boots:with-boots ()
    (intro)
    (initialize)
    (populate)
    (main)))

(defun toplevel ()
  (setf *random-state* (make-random-state t))
  (sb-ext:exit :code (ecase (run)
                       ;; see `man robotfindskitten`
                       (:win 0)
                       (:quit 1))))
