;; maze.lisp
;; A small console maze game.
;;
;; Run:
;;   sbcl --script maze.lisp
;; or from the REPL:
;;   (load "maze.lisp")

(defparameter *maze-width* 31)
(defparameter *maze-height* 17)
(defparameter *wall* #\#)
(defparameter *path* #\Space)
(defparameter *player* #\@)
(defparameter *goal* #\G)
(defparameter *color-reset* "~c[0m")
(defparameter *wall-color* "~c[34m")
(defparameter *player-color* "~c[32m")
(defparameter *goal-color* "~c[33m")

(defstruct (game (:constructor %make-game))
  maze
  width
  height
  player-x
  player-y
  goal-x
  goal-y
  finished-p)

;; Step 1: draw ### style walls and open spaces.
(defun make-filled-maze (width height &optional (fill *wall*))
  (make-array (list height width) :initial-element fill))

(defun maze-at (maze x y)
  (aref maze y x))

(defun (setf maze-at) (value maze x y)
  (setf (aref maze y x) value))

(defun clear-screen ()
  (format t "~c[2J~c[H" #\Esc #\Esc))

(defun move-cursor-home ()
  (format t "~c[H" #\Esc))

(defun clear-to-end-of-screen ()
  (format t "~c[J" #\Esc))

(defun hide-cursor ()
  (format t "~c[?25l" #\Esc))

(defun show-cursor ()
  (format t "~c[?25h" #\Esc))

(defun set-terminal-cbreak ()
  #+sbcl
  (ignore-errors
    (sb-ext:run-program "stty" '("cbreak" "-echo") :input t :output nil :error nil :search t))
  #-sbcl
  nil)

(defun restore-terminal ()
  #+sbcl
  (ignore-errors
    (sb-ext:run-program "stty" '("sane") :input t :output nil :error nil :search t))
  (show-cursor)
  #-sbcl
  nil)

(defun read-command-char ()
  (read-char *standard-input* nil #\q))

(defun write-colored-char (char color stream)
  (format stream color #\Esc)
  (write-char char stream)
  (format stream *color-reset* #\Esc))

(defun draw-tile (char stream)
  (cond
    ((char= char *wall*) (write-colored-char char *wall-color* stream))
    ((char= char *player*) (write-colored-char char *player-color* stream))
    ((char= char *goal*) (write-colored-char char *goal-color* stream))
    (t (write-char char stream))))

(defun game-frame (game)
  (with-output-to-string (stream)
    (format stream "Maze Explorer  move: W/A/S/D  quit: Q~%~%")
    (loop for y below (game-height game) do
      (loop for x below (game-width game) do
        (draw-tile
         (cond
           ((and (= x (game-player-x game)) (= y (game-player-y game))) *player*)
           ((and (= x (game-goal-x game)) (= y (game-goal-y game))) *goal*)
           (t (maze-at (game-maze game) x y)))
         stream))
      (terpri stream))
    (when (eq (game-finished-p game) t)
      (format stream "~%Goal! You escaped the maze.~%"))))

(defun draw-game (game)
  (move-cursor-home)
  (write-string (game-frame game))
  (clear-to-end-of-screen)
  (finish-output))

;; Step 2: draw the player and goal on top of the maze.
(defun make-game (&key (width *maze-width*) (height *maze-height*))
  (let* ((fixed-width (if (oddp width) width (1+ width)))
         (fixed-height (if (oddp height) height (1+ height)))
         (maze (generate-maze fixed-width fixed-height)))
    (%make-game
     :maze maze
     :width fixed-width
     :height fixed-height
     :player-x 1
     :player-y 1
     :goal-x (- fixed-width 2)
     :goal-y (- fixed-height 2)
     :finished-p nil)))

;; Step 3: generate a playable maze automatically.
(defun shuffle-list (items)
  (let ((vector (coerce items 'vector)))
    (loop for i downfrom (1- (length vector)) above 0 do
      (rotatef (aref vector i) (aref vector (random (1+ i)))))
    (coerce vector 'list)))

(defun inside-maze-p (x y width height)
  (and (< 0 x (1- width))
       (< 0 y (1- height))))

(defun carve-maze-from (maze x y width height)
  (setf (maze-at maze x y) *path*)
  (dolist (direction (shuffle-list '((0 -2) (2 0) (0 2) (-2 0))))
    (destructuring-bind (dx dy) direction
      (let ((next-x (+ x dx))
            (next-y (+ y dy)))
        (when (and (inside-maze-p next-x next-y width height)
                   (char= (maze-at maze next-x next-y) *wall*))
          (setf (maze-at maze (+ x (/ dx 2)) (+ y (/ dy 2))) *path*)
          (carve-maze-from maze next-x next-y width height))))))

(defun generate-maze (width height)
  (let ((maze (make-filled-maze width height)))
    (carve-maze-from maze 1 1 width height)
    maze))

;; Step 4: move the player when the target tile is not a wall.
(defun direction-for-key (key)
  (case (char-downcase key)
    (#\w '(0 -1))
    (#\a '(-1 0))
    (#\s '(0 1))
    (#\d '(1 0))
    (otherwise nil)))

(defun try-move-player (game dx dy)
  (let ((next-x (+ (game-player-x game) dx))
        (next-y (+ (game-player-y game) dy)))
    (when (and (inside-maze-p next-x next-y (game-width game) (game-height game))
               (not (char= (maze-at (game-maze game) next-x next-y) *wall*)))
      (setf (game-player-x game) next-x
            (game-player-y game) next-y))))

;; Step 5: finish when the player reaches the goal.
(defun update-goal-state (game)
  (when (and (= (game-player-x game) (game-goal-x game))
             (= (game-player-y game) (game-goal-y game)))
    (setf (game-finished-p game) t)))

(defun handle-key (game key)
  (cond
    ((char= (char-downcase key) #\q)
     (setf (game-finished-p game) :quit))
    ((direction-for-key key)
     (destructuring-bind (dx dy) (direction-for-key key)
       (try-move-player game dx dy)
       (update-goal-state game)))))

(defun run-game ()
  (let ((game (make-game)))
    (unwind-protect
         (progn
           (set-terminal-cbreak)
           (clear-screen)
           (hide-cursor)
           (loop until (game-finished-p game) do
             (draw-game game)
             (handle-key game (read-command-char)))
           (draw-game game)
           (when (eq (game-finished-p game) :quit)
             (format t "~%Bye.~%")))
      (restore-terminal))))

(run-game)
