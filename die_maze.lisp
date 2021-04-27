

; The current state of the die (x-coordinate, y-coordiate, face)
; Can be obtained from state, but this makes life easier
(defparameter *current-coord* (make-array 2 :initial-element nil))

; Current face of the dice. Stores what face is shown, and what face =
pointed down.
; All other faces can be determined from this
(defparameter *current-face* (make-array 3))

; A structure representating a state in the maze
(defstruct maze-state
  ; The coordinates of the die (x y)
  (coord (make-array 2))
  ; The face of the die (top up right)
  (face (make-array 3)))

; The current state
(defparameter *current-state* (make-maze-state))

; The goal state.
; The goal state always have the die with 1 facing up.
(defparameter *goal-state* (make-maze-state :face '(1 * *)))

; The maze environemt. 0 =3D free space. 1 =3D obstacle.
; The environment is static.
(defparameter *environment* (list))


(defun load-maze( file )
  "Load the die maze from the specified file" =20
 =20
  ; Reset the currently loaded globals
  (setf *current-state* (make-maze-state :face '(1 2 3)))
  (setf *goal-state* (make-maze-state :face '(1 * *)))
  (setf *environment* '())
 =20
  (let ((in (open file :if-does-not-exist nil)) (x 0) (y 0) (env-temp =
(list)))
    (when in
      (loop for char =3D (read-char in nil)
          while char do (when char=20
                          (cond
                           ((char=3D char #\.)
                            (setf x (+ x 1))
                            (setf env-temp (append env-temp '(0))))
                           ((char=3D char #\Newline)=20
                            (setf y (+ y 1))
                            (setf x 0)
                            (setf *environment* (append *environment* =
(list env-temp)))
                            (setf env-temp '()))
                           ((char=3D char #\S)=20
                            (setf (maze-state-coord *current-state*) =
(list x y))
                            (setf x (+ x 1))
                            (setf env-temp (append env-temp '(0))))
                           ((char=3D char #\*)
                            (setf x (+ x 1))
                            (setf env-temp (append env-temp '(1))))
                           ((char=3D char #\G)=20
                            (setf (maze-state-coord *goal-state*) (list =
x y))
                            (setf x (+ x 1))
                            (setf env-temp (append env-temp '(0)))))))
      (setf *environment* (append *environment* (list env-temp)))
      (setf env-temp '())
      (close in))))

(defmethod next-best-move ()
  "Return the path to the goal state"
 =20
  (when (goal-test *current-state*)
    (return-from next-best-move *current-state*))
 =20
  (let ((best-cost '90)
        (best-move '()))
   =20
    ;Find the COST
    (dolist (x (successors *current-state*))=20
      (when (<=3D (+ (distance-cost x *goal-state*) (face-cost x =
*goal-state*)) best-cost)
        (setf best-move x)
        (setf best-cost (+ (distance-cost x *goal-state*) (face-cost x =
*goal-state*)))))
   =20
    (setf *current-state* best-move)
    (return-from next-best-move best-move)))
 =20


(defmethod h-cost (state)
  "Heuristic function"
  ())

(defmethod successors ((state maze-state))
  "Return the successors of the current state. Move up/down/right/left"
  (let ((left-state (get-move-left-state state))
        (right-state (get-move-right-state state))
        (up-state (get-move-up-state state))
        (down-state (get-move-down-state state))
        (return-states))
   =20
    (if (char/=3D left-state nil) (setf return-states (append =
return-states (list left-state))))
    (if (char/=3D right-state nil) (setf return-states (append =
return-states (list right-state))))
    (if (char/=3D up-state nil) (setf return-states (append =
return-states (list up-state))))
    (if (char/=3D down-state nil) (setf return-states (append =
return-states (list down-state))))
   =20
    (return-from successors return-states)))
 =20
(defun get-move-left-state (state)=20
  ;(x y z) -> (z y 7-x)
  (let (( x ( elt (maze-state-coord state) 0))
        ( y ( elt (maze-state-coord state) 1))
        ( face ( maze-state-face state)))
   =20
    (let ((new-face ( list (elt face 2) (elt face 1) (abs (- 7 (elt face =
0)))))
            (new-x (- x 1) y))
        ( when ( and (>=3D new-x 0) (/=3D (get-coord new-x y) 1 ) (/=3D =
(elt new-face 0) 6))
          (make-maze-state :coord (list new-x y) :face new-face)))))

(defun get-move-right-state (state)
  ;(x y z) -> (7-z y x)
    (let (( x ( elt (maze-state-coord state) 0))
          ( y ( elt (maze-state-coord state) 1))
          ( face ( maze-state-face state)))

      (let ((new-face ( list (abs (- 7 (elt face 2))) (elt face 1) (elt =
face 0)))
            (new-x (+ 1 x)))
        ( when ( and (< new-x (length (grab-row y))) (/=3D (get-coord =
new-x y) 1 ) (/=3D (elt new-face 0) 6))
          (make-maze-state :coord (list new-x y) :face new-face)))))

(defun get-move-up-state (state)
  ;(x y z) -> (7-y x z)
      (let (( x ( elt (maze-state-coord state) 0))
            ( y ( elt (maze-state-coord state) 1))
            ( face ( maze-state-face state)))
       =20
          (let ((new-face ( list (abs (- 7 (elt face 1))) (elt face 0) =
(elt face 2)))
                (new-y (- y 1)))
            ( when ( and (>=3D new-y 0) (/=3D (get-coord x new-y) 1 ) =
(/=3D (elt new-face 0) 6))
              (make-maze-state :coord (list x new-y) :face new-face)))))

(defun get-move-down-state (state)
  ;(x y z) -> (y 7-x z)
      (let (( x ( elt (maze-state-coord state) 0))
            ( y ( elt (maze-state-coord state) 1))
            ( face ( maze-state-face state)))
       =20
        (let ((new-face ( list (elt face 1) (abs (- 7 (elt face 0))) =
(elt face 2)))
              (new-y (+ y 1)))
          ( when ( and (< new-y (length *environment*)) (/=3D (get-coord =
x new-y) 1 ) (/=3D (elt new-face 0) 6))
            (make-maze-state :coord (list x new-y) :face new-face)))))

(defun goal-test (state)
  "Test if the current state is a goal state"
  (and (equal (maze-state-coord state) (maze-state-coord *goal-state*))
       (eq (elt (maze-state-face state) 0) (elt (maze-state-face =
*goal-state*) 0) ) ))

(defun move-right=20
    "Move the die right one space"
  (setf *current-state* (get-move-right-state *current-state*)))

(defun move-left
    "Move the die left one space"
  (setf *current-state* (get-move-right-state *current-state*)))

(defun move-up
    "Move the die up one space"
  (setf *current-state* (get-move-up-state *current-state*)))

(defun move-up
    "Move the die up one space"
  (setf *current-state* (get-move-down-state *current-state*)))

(defun get-coord(x y)=20
  "Return the x y coordinate from the environment"
  (when (< y (length *environment*))
    (when(< x (length (grab-row y)))
      (elt (elt *environment* y) x))))

(defun grab-row(y)
  "Grab row y of the specified state"
  (when (< y (length *environment*))
    (elt *environment* y)))

(defun distance-cost (state state2)
  (+ (abs (- (elt (maze-state-coord state) 0) (elt (maze-state-coord =
state2) 0)))
     (abs (- (elt (maze-state-coord state) 1) (elt (maze-state-coord =
state2) 1)))))

(defun x-dist (state1 state2)
  (- (elt (maze-state-coord state1) 0) (elt (maze-state-coord state2) =
0)))

(defun y-dist (state1 state2)
  (- (elt (maze-state-coord state1) 1) (elt (maze-state-coord state2) =
1)))

(defun face-cost (state state2)
  (let (( x ( elt (maze-state-coord state) 0))
        ( x2 ( elt (maze-state-coord state2) 0))
        ( y ( elt (maze-state-coord state) 1))
        ( y2 ( elt (maze-state-coord state2) 1))
        ( face ( maze-state-face state))
        ( face2 ( maze-state-face state2)))
    (cond=20
     ((and (=3D (- 1 y) y2) (char=3D (elt face2 0) (abs (- 7 (elt face =
1))))) '1)
     ((and (=3D (+ 1 y) y2) (char=3D (elt face2 0) (elt face 1))) '1)
     ((and (=3D (- 1 x) x2) (char=3D (elt face2 0) (elt face 2)) '1))
     ((and (=3D (+ 1 x) x2) (char=3D (elt face2 0) (abs (- 7 (elt face =
2))))) '1))
  '0))
     =20

(defun cost(state)
  "The cost functions"
  (if (goal-test state) '0))


(defun puzzle1 ()
    (load-maze "C:\\puzzle1.txt"))

(defun puzzle2 ()
    (load-maze "C:\\puzzle2.txt"))

(defun puzzle3 ()
    (load-maze "C:\\puzzle3.txt"))

(defun puzzle4 ()
    (load-maze "C:\\puzzle4.txt"))

(defun puzzle5 ()
    (load-maze "C:\\puzzle5.txt"))