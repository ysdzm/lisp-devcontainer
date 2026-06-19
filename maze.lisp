;; maze.lisp
;; A small console maze game.
;;
;; Run:
;;   sbcl --script maze.lisp
;; or from the REPL:
;;   (load "maze.lisp")

(defparameter *maze-width* 51)
(defparameter *maze-height* 23)
(defparameter *rock* #\Space)
(defparameter *horizontal-wall* #\-)
(defparameter *vertical-wall* #\|)
(defparameter *door* #\+)
(defparameter *corridor* #\#)
(defparameter *floor* #\.)
(defparameter *player* #\@)
(defparameter *amulet* #\,)
(defparameter *stairs* #\%)
(defparameter *max-floor* 5)

(defstruct (game (:constructor %make-game))
  maze
  width
  height
  player-x
  player-y
  goal-x
  goal-y
  floor
  max-floor
  finished-p)

(defstruct dungeon-room
  x
  y
  width
  height)

;; Step 1: draw a Rogue-style ASCII dungeon.
(defun make-filled-maze (width height &optional (fill *rock*))
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

(defun draw-tile (char stream)
  (write-char char stream))

(defun exit-char (game)
  (if (= (game-floor game) (game-max-floor game))
      *amulet*
      *stairs*))

(defun game-frame (game)
  (with-output-to-string (stream)
    (format stream "You are in the Dungeons of Doom.  Move: h/j/k/l/y/u/b/n  Quit: Q~%")
    (loop for y below (game-height game) do
      (loop for x below (game-width game) do
        (draw-tile
         (cond
           ((and (= x (game-player-x game)) (= y (game-player-y game))) *player*)
           ((and (= x (game-goal-x game)) (= y (game-goal-y game))) (exit-char game))
           (t (maze-at (game-maze game) x y)))
         stream))
      (terpri stream))
    (format stream "Level: ~d  Gold: 0  Hp: 12(12)  Str: 16  Armor: 10  Exp: 1/0~%"
            (game-floor game))
    (when (eq (game-finished-p game) t)
      (format stream "You found the Amulet of Yendor.~%"))))

(defun draw-game (game)
  (move-cursor-home)
  (write-string (game-frame game))
  (clear-to-end-of-screen)
  (finish-output))

;; Step 2: draw the player and stairs or goal on top of the dungeon.
(defun make-game (&key (width *maze-width*) (height *maze-height*) (max-floor *max-floor*))
  (let* ((fixed-width (if (oddp width) width (1+ width)))
         (fixed-height (if (oddp height) height (1+ height)))
         (dungeon (generate-dungeon fixed-width fixed-height))
         (maze (first dungeon))
         (start (dungeon-room-center (first (second dungeon))))
         (exit (dungeon-room-center (first (last (second dungeon))))))
    (%make-game
     :maze maze
     :width fixed-width
     :height fixed-height
     :player-x (first start)
     :player-y (second start)
     :goal-x (first exit)
     :goal-y (second exit)
     :floor 1
     :max-floor max-floor
     :finished-p nil)))

(defun inside-maze-p (x y width height)
  (and (< 0 x (1- width))
       (< 0 y (1- height))))

;; Step 3: generate a room-and-corridor dungeon automatically.
(defun random-between (minimum maximum)
  (+ minimum (random (1+ (- maximum minimum)))))

(defun dungeon-room-center (room)
  (list (+ (dungeon-room-x room) (floor (dungeon-room-width room) 2))
        (+ (dungeon-room-y room) (floor (dungeon-room-height room) 2))))

(defun rooms-overlap-p (a b)
  (not (or (< (+ (dungeon-room-x a) (dungeon-room-width a)) (1- (dungeon-room-x b)))
           (< (+ (dungeon-room-x b) (dungeon-room-width b)) (1- (dungeon-room-x a)))
           (< (+ (dungeon-room-y a) (dungeon-room-height a)) (1- (dungeon-room-y b)))
           (< (+ (dungeon-room-y b) (dungeon-room-height b)) (1- (dungeon-room-y a))))))

(defun carve-room (maze room)
  (let ((left (dungeon-room-x room))
        (top (dungeon-room-y room))
        (right (1- (+ (dungeon-room-x room) (dungeon-room-width room))))
        (bottom (1- (+ (dungeon-room-y room) (dungeon-room-height room)))))
    (loop for x from left to right do
      (setf (maze-at maze x top) *horizontal-wall*
            (maze-at maze x bottom) *horizontal-wall*))
    (loop for y from (1+ top) below bottom do
      (setf (maze-at maze left y) *vertical-wall*
            (maze-at maze right y) *vertical-wall*)
      (loop for x from (1+ left) below right do
        (setf (maze-at maze x y) *floor*)))))

(defun erase-room (maze room)
  (loop for y from (dungeon-room-y room) below (+ (dungeon-room-y room) (dungeon-room-height room)) do
    (loop for x from (dungeon-room-x room) below (+ (dungeon-room-x room) (dungeon-room-width room)) do
      (setf (maze-at maze x y) *rock*))))

(defun wall-char-p (char)
  (or (char= char *horizontal-wall*)
      (char= char *vertical-wall*)))

(defun carve-corridor-tile (maze x y)
  (when (char= (maze-at maze x y) *rock*)
    (setf (maze-at maze x y) *corridor*)))

(defun corridor-space-p (tile)
  (or (char= tile *rock*)
      (char= tile *corridor*)))

(defun room-horizontal-door (room target-x)
  (destructuring-bind (center-x center-y) (dungeon-room-center room)
    (declare (ignore center-x))
    (let ((wall-x (if (< target-x (dungeon-room-x room))
                      (dungeon-room-x room)
                      (1- (+ (dungeon-room-x room) (dungeon-room-width room))))))
      (list wall-x center-y
            (+ wall-x (if (< target-x wall-x) -1 1))
            center-y))))

(defun room-vertical-door (room target-y)
  (destructuring-bind (center-x center-y) (dungeon-room-center room)
    (declare (ignore center-y))
    (let ((wall-y (if (< target-y (dungeon-room-y room))
                      (dungeon-room-y room)
                      (1- (+ (dungeon-room-y room) (dungeon-room-height room))))))
      (list center-x wall-y
            center-x
            (+ wall-y (if (< target-y wall-y) -1 1))))))

(defun open-door (maze door)
  (setf (maze-at maze (first door) (second door)) *door*))

(defun position-key (position)
  (cons (first position) (second position)))

(defun reconstruct-path (came-from current)
  (let ((path (list current)))
    (loop for previous = (gethash (position-key current) came-from)
          while previous do
            (push previous path)
            (setf current previous))
    path))

(defun corridor-neighbors (maze width height position)
  (destructuring-bind (x y) position
    (loop for (dx dy) in '((1 0) (-1 0) (0 1) (0 -1))
          for nx = (+ x dx)
          for ny = (+ y dy)
          when (and (inside-maze-p nx ny width height)
                    (corridor-space-p (maze-at maze nx ny)))
            collect (list nx ny))))

(defun find-corridor-path (maze start goal)
  (let* ((height (array-dimension maze 0))
         (width (array-dimension maze 1))
         (queue (list start))
         (seen (make-hash-table :test 'equal))
         (came-from (make-hash-table :test 'equal)))
    (setf (gethash (position-key start) seen) t)
    (loop while queue do
      (let ((current (pop queue)))
        (when (equal current goal)
          (return-from find-corridor-path (reconstruct-path came-from current)))
        (dolist (neighbor (corridor-neighbors maze width height current))
          (unless (gethash (position-key neighbor) seen)
            (setf (gethash (position-key neighbor) seen) t
                  (gethash (position-key neighbor) came-from) current
                  queue (append queue (list neighbor)))))))
    nil))

(defun carve-corridor-path (maze path)
  (dolist (position path)
    (carve-corridor-tile maze (first position) (second position))))

(defun connect-door-outsides (maze outside-a outside-b)
  (let ((path (find-corridor-path maze outside-a outside-b)))
    (when path
      (carve-corridor-path maze path)
      t)))

(defun connect-rooms (maze a b)
  (destructuring-bind (ax ay) (dungeon-room-center a)
    (destructuring-bind (bx by) (dungeon-room-center b)
      (let* ((horizontal-first-p (> (abs (- ax bx)) (abs (- ay by))))
             (door-a (if horizontal-first-p
                         (room-horizontal-door a bx)
                         (room-vertical-door a by)))
             (door-b (if horizontal-first-p
                         (room-vertical-door b ay)
                         (room-horizontal-door b ax))))
        (when (connect-door-outsides maze
                                     (list (third door-a) (fourth door-a))
                                     (list (third door-b) (fourth door-b)))
          (open-door maze door-a)
          (open-door maze door-b)
          t)))))

(defun distance-between-rooms (a b)
  (destructuring-bind (ax ay) (dungeon-room-center a)
    (destructuring-bind (bx by) (dungeon-room-center b)
      (+ (abs (- ax bx)) (abs (- ay by))))))

(defun nearest-room (room rooms)
  (let ((nearest (first rooms)))
    (dolist (candidate (rest rooms) nearest)
      (when (< (distance-between-rooms room candidate)
               (distance-between-rooms room nearest))
        (setf nearest candidate)))))

(defun try-random-room (width height)
  (let* ((dungeon-room-width (random-between 5 11))
         (dungeon-room-height (random-between 4 7))
         (x (random-between 1 (- width dungeon-room-width 2)))
         (y (random-between 1 (- height dungeon-room-height 2))))
    (make-dungeon-room :x x :y y :width dungeon-room-width :height dungeon-room-height)))

(defun dungeon-passable-tile-p (tile)
  (or (char= tile *floor*)
      (char= tile *corridor*)
      (char= tile *door*)))

(defun dungeon-reachable-map (maze width height start)
  (let ((seen (make-array (list height width) :initial-element nil))
        (queue (list start)))
    (setf (aref seen (second start) (first start)) t)
    (loop while queue do
      (destructuring-bind (x y) (pop queue)
        (dolist (direction '((1 0) (-1 0) (0 1) (0 -1)))
          (let ((nx (+ x (first direction)))
                (ny (+ y (second direction))))
            (when (and (inside-maze-p nx ny width height)
                       (not (aref seen ny nx))
                       (dungeon-passable-tile-p (maze-at maze nx ny)))
              (setf (aref seen ny nx) t)
              (setf queue (append queue (list (list nx ny)))))))))
    seen))

(defun dungeon-all-passable-reachable-p (maze width height start)
  (let ((seen (dungeon-reachable-map maze width height start)))
    (loop for y below height always
      (loop for x below width always
        (or (not (dungeon-passable-tile-p (maze-at maze x y)))
            (aref seen y x))))))

(defun connected-dungeon-p (dungeon width height)
  (let* ((maze (first dungeon))
         (rooms (second dungeon))
         (start (dungeon-room-center (first rooms))))
    (dungeon-all-passable-reachable-p maze width height start)))

(defun generate-dungeon-once (width height)
  (let ((maze (make-filled-maze width height))
        (rooms nil))
    (loop repeat 120 do
      (let ((candidate (try-random-room width height)))
        (unless (some (lambda (room) (rooms-overlap-p candidate room)) rooms)
          (carve-room maze candidate)
          (if rooms
              (if (connect-rooms maze (nearest-room candidate rooms) candidate)
                  (push candidate rooms)
                  (erase-room maze candidate))
              (push candidate rooms)))))
    (when (< (length rooms) 2)
      (let ((left (make-dungeon-room :x 2 :y 2 :width 9 :height 6))
            (right (make-dungeon-room :x (- width 12) :y (- height 8) :width 9 :height 6)))
        (setf maze (make-filled-maze width height)
              rooms (list right left))
        (carve-room maze left)
        (carve-room maze right)
        (connect-rooms maze left right)))
    (list maze (reverse rooms))))

(defun generate-dungeon (width height)
  (loop repeat 100
        for dungeon = (generate-dungeon-once width height)
        when (connected-dungeon-p dungeon width height)
          return dungeon
        finally (return (generate-dungeon-once width height))))

(defun load-next-floor (game)
  (let* ((dungeon (generate-dungeon (game-width game) (game-height game)))
         (maze (first dungeon))
         (rooms (second dungeon))
         (start (dungeon-room-center (first rooms)))
         (exit (dungeon-room-center (first (last rooms)))))
    (setf (game-maze game) maze
          (game-player-x game) (first start)
          (game-player-y game) (second start)
          (game-goal-x game) (first exit)
          (game-goal-y game) (second exit)
          (game-floor game) (1+ (game-floor game)))))

;; Step 4: move the player when the target tile is passable.
(defun direction-for-key (key)
  (case key
    (#\y '(-1 -1))
    (#\u '(1 -1))
    (#\b '(-1 1))
    (#\n '(1 1))
    (otherwise
     (case (char-downcase key)
       (#\w '(0 -1))
       (#\a '(-1 0))
       (#\s '(0 1))
       (#\d '(1 0))
       (#\k '(0 -1))
       (#\h '(-1 0))
       (#\j '(0 1))
       (#\l '(1 0))
       (otherwise nil)))))

(defun passable-tile-p (tile)
  (dungeon-passable-tile-p tile))

(defun try-move-player (game dx dy)
  (let ((next-x (+ (game-player-x game) dx))
        (next-y (+ (game-player-y game) dy)))
    (when (and (inside-maze-p next-x next-y (game-width game) (game-height game))
               (passable-tile-p (maze-at (game-maze game) next-x next-y)))
      (setf (game-player-x game) next-x
            (game-player-y game) next-y))))

;; Step 5: finish when the player reaches the goal.
(defun update-goal-state (game)
  (when (and (= (game-player-x game) (game-goal-x game))
             (= (game-player-y game) (game-goal-y game)))
    (if (= (game-floor game) (game-max-floor game))
        (setf (game-finished-p game) t)
        (load-next-floor game))))

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
