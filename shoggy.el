;;; shoggy.el --- A fairy chess variant game -*- lexical-binding: t -*-

;; Copyright (C) 2024 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/shoggy
;; Version: 0.1
;; Package-Requires: ((emacs "29.3"))
;; Keywords: games

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A fairy chess game for Emacs.

;; v0.1: Spring Lisp Game Jam 2024

;; DONE 2024-05-18: translator of SQUARE to notation {PIECE}{x}{position}

;; IDEA 2024-05-18: read FEN-link string as board initial setup; export to FEN

;; NOTE 2024-05-18: FEN input will be unreadable with spell cards enable


;;; Code:

(require 'cl-lib)


;;;; User options

(defgroup shoggy nil
  "Group for `shoggy' customizations."
  :group 'games)

(defcustom shoggy-board-pawn-moves-two-squares-p nil
  "Should pawns be able to move two squares on their first move?"
  :type 'boolean)

(defcustom shoggy-board-homerow '(r n f s w f)
  "Starting position of pieces in the home row."
  :type 'sexp)

(defcustom shoggy-board-homerow-random-p t
  "Randomize the starting position of the pieces for each game."
  :type 'boolean)

(defcustom shoggy-engine 'sanefish
  "The opponent's engine."
  :type '(choice (const dumbfish) (const sanefish)))

(defcustom shoggy-sound-enabled t
  "Should shoggy play sounds?"
  :type 'boolean)

(defcustom shoggy-sound-command #'play-sound-file
  "Play sound command. Default is the Emacs builtin `play-sound-file'.
In case it doesn't work properly, try `shoggy-play-sound-file', which see."
  :type 'function)


;;;; Variables

(defvar shoggy-board nil
  "The board structure.")

(defvar shoggy-board-size 6
  "Board size.")

(defvar shoggy-board-flip-count 0
  "Board flip counter.")

(defvar shoggy-player-color "white")

(defvar shoggy-captured-pieces nil
  "List of captured pieces.")

(defvar shoggy-board-buffer "*shoggy-board*"
  "Buffer for the graphical `shoggy-board'.")

(defvar shoggy-user-color "white")

(defvar shoggy-board-ui-p nil
  "Control if this is an user action.")

(defvar shoggy--game-over nil)

(declare-function shoggy-ui-headerline-format "shoggy-ui")
(declare-function shoggy-ui-promotion-prompt "shoggy-ui")
(declare-function shoggy-ui-sound-play "shoggy-ui")
(declare-function shoggy-spell-draw-card "shoggy-spell")


;;;; Functions

(defun shoggy-shuffle (seq &optional n)
  "Return SEQ shuffled.
If N is a number, take the first N elements of the shuffled SEQ."
  (let ((shuffle (seq-sort-by (lambda (_) (random)) #'<= seq)))
    (if (numberp n)
        (take n shuffle)
      shuffle)))

(defun shoggy-get-buffer ()
  "Get `shoggy-board-buffer'."
  (get-buffer-create shoggy-board-buffer))

(defun shoggy-play-sound-file (file &optional volume)
  "Alternate play sound in case `play-sound-file' doesn't work in some OS.
It defaults to mplayer. Modify it properly."
  (start-process "play-sound-file" nil
                 "mplayer" "-volume" (format "%s" (or volume 100))
                 file))


;;;; Pieces

;; REVIEW: reconsider some slots
(cl-defstruct (shoggy-piece (:constructor shoggy-piece-make)
                            (:copier nil))
  "A shoggy piece."
  color     ;; "white" or "black" (string)
  name      ;; piece name (string)
  atom      ;; single letter representation (symbol)
  value     ;; piece value (number)
  range     ;; piece square range (number)
  ;; REVIEW 2024-05-17: use direction names (N E S W NE SE SW NW) *and*
  ;; direction groups ('all orthogonal 'diagonal 'hippogonal)?
  direction ;; list of directions (list)
  position  ;; current position in the board (cons)
  leaper    ;; whether piece is a leaper (boolean)
  boosted   ;; if piece was boosted by spell card (boolean)
  image     ;; graphical representation ;; REVIEW 2024-05-18: unused
  unicode)  ;; unicode glyph ;; REVIEW 2024-05-18: unused

(cl-defstruct (shoggy-piece-pawn
               (:constructor shoggy-piece-make-pawn)
               (:copier nil)
               (:include shoggy-piece
                         (name "Pawn")
                         (atom 'p)
                         (value 1))))

(cl-defstruct (shoggy-piece-ferz
               (:constructor shoggy-piece-make-ferz)
               (:copier nil)
               (:include shoggy-piece
                         (name "Ferz")
                         (atom 'f)
                         (value 2)
                         (range 1)
                         (direction '(NE SE SW NW)))))

(cl-defstruct (shoggy-piece-wazir
               (:constructor shoggy-piece-make-wazir)
               (:copier nil)
               (:include shoggy-piece
                         (name "Wazir")
                         (atom 'w)
                         (value 2)
                         (range 1)
                         (direction '(N E S W)))))

(cl-defstruct (shoggy-piece-sage
               (:constructor shoggy-piece-make-sage)
               (:copier nil)
               (:include shoggy-piece
                         (name "Sage")
                         (atom 's)
                         (value 99)
                         (range 1)
                         (direction '(N E S W NE SE SW NW)))))

(cl-defstruct (shoggy-piece-knight
               (:constructor shoggy-piece-make-knight)
               (:copier nil)
               (:include shoggy-piece
                         (name "Knight")
                         (atom 'n)
                         (value 3)
                         (leaper '(2 . 1)))))

(cl-defstruct (shoggy-piece-rook
               (:constructor shoggy-piece-make-rook)
               (:copier nil)
               (:include shoggy-piece
                         (name "Rook")
                         (atom 'r)
                         (value 5)
                         (range shoggy-board-size)
                         (direction '(N E S W)))))

(defun shoggy-piece-print (piece)
  "Print PIECE atom as in FEN notation. White pieces are uppercase letters."
  (if (equal (shoggy-piece-color piece) "white")
      (intern (upcase (format "%s" (shoggy-piece-atom piece))))
    (shoggy-piece-atom piece)))

(defun shoggy-piece-enemy-p (piece other-piece)
  "Return t if PIECE and OTHER-PIECE are enemies."
  (and piece
       other-piece
       (not (equal (shoggy-piece-color piece)
                   (shoggy-piece-color other-piece)))))

(defun shoggy-piece-friend-p (piece other-piece)
  "Return t if PIECE and OTHER-PIECE are friends."
  (and piece
       other-piece
       (equal (shoggy-piece-color piece)
              (shoggy-piece-color other-piece))))

(defun shoggy-piece-own-p (piece)
  "Return t if PIECE belongs to player."
  (and piece
       (equal (shoggy-piece-color piece) shoggy-player-color)))


;;;; Board

(defun shoggy-board-init ()
  "Initialize an empty `shoggy-board' as a list of vectors."
  (setq shoggy--game-over nil)
  (setq shoggy-captured-pieces nil)
  (setq shoggy-board-flip-count 0)
  (setq shoggy-board (make-list shoggy-board-size nil))
  (dotimes (i shoggy-board-size)
    (setf (nth i shoggy-board) (make-vector shoggy-board-size nil))))

(defun shoggy-board-square-p (square)
  "Return SQUARE if it is a valid square in `shoggy-board'."
  (let ((row (car square))
        (col (cdr square)))
    (and row col
         (>= row 0) (>= col 0)
         (< row shoggy-board-size) (< col shoggy-board-size)
         square)))

(defmacro shoggy-board-get (square)
  "Return the piece that is in SQUARE. If it is empty, return nil."
  `(when (shoggy-board-square-p ,square)
     (elt (elt shoggy-board (car ,square)) (cdr ,square))))

(defun shoggy-board-put (piece square)
  "Put PIECE in SQUARE position."
  (when (shoggy-piece-p piece)
    (setf (shoggy-piece-position piece) square)
    (setf (shoggy-board-get square) piece)))

(defun shoggy-board-put-new (atom color square)
  "Make piece of type ATOM and COLOR and put it in SQUARE."
  (let ((fn (cond ((eq atom 'p) #'shoggy-piece-make-pawn)
                  ((eq atom 'f) #'shoggy-piece-make-ferz)
                  ((eq atom 'w) #'shoggy-piece-make-wazir)
                  ((eq atom 's) #'shoggy-piece-make-sage)
                  ((eq atom 'n) #'shoggy-piece-make-knight)
                  ((eq atom 'r) #'shoggy-piece-make-rook))))
    (shoggy-board-put (funcall fn :color color :position square) square)))

(defun shoggy-board-pop (square)
  "Clear SQUARE and return the piece that was on it."
  (when-let (piece (shoggy-board-get square))
    (setf (shoggy-piece-position piece) nil)
    (setf (shoggy-board-get square) nil)
    piece))

(defun shoggy-board-game-over (&optional msg)
  "End of game. Show MSG in header-line and stop game loop."
  (shoggy-ui-headerline-format (or msg "GAME OVER!") 'end)
  (shoggy-ui-sound-play 'end)
  (setq shoggy--game-over t)
  (shoggy-ui-board-update
    (setq shoggy-ui-board--square-map nil)
    (setq shoggy-ui-board--keymap nil))
  (shoggy-ui-modeline-setup)
  (kill-matching-buffers "\ \\*shoggy-.*-state\\*" t t))

(defun shoggy-user-p ()
  "Return t if current turn is the user's turn."
  (equal shoggy-user-color shoggy-player-color))

(defun shoggy-board-move (from-square to-square)
  "Move piece from FROM-SQUARE to TO-SQUARE."
  (let ((moved-piece (shoggy-board-pop from-square))
        (captured-piece (shoggy-board-pop to-square)))
    (shoggy-board-put moved-piece to-square)
    (when captured-piece
      (push captured-piece shoggy-captured-pieces)
      (shoggy-spell-draw-card))

    (shoggy-ui-headerline-format
     (concat (capitalize shoggy-player-color) ": "
             (shoggy-board-notation-move from-square
                                         to-square
                                         captured-piece))
     (when captured-piece 'turn))

    (when (and (shoggy-user-p) shoggy-board-ui-p)
      (shoggy-ui-sound-play (if captured-piece 'capture 'move)))

    ;; When Sage is captured, the game ends.
    (when (and (shoggy-piece-sage-p captured-piece) shoggy-board-ui-p)
      (shoggy-board-game-over
       (format "Sage was captured! %s wins!"
               (capitalize shoggy-player-color))))

    ;; Pawn promotion
    (when (and (shoggy-piece-pawn-p moved-piece)
               (= (car to-square) 0))
      (if (and (shoggy-user-p) shoggy-board-ui-p)
          (shoggy-ui-promotion-prompt to-square)
        (shoggy-board-put-new (car (shoggy-shuffle '(n r) 1))
                              shoggy-player-color
                              to-square)))))

;; IDEA: implement option to read FEN-like notation into board
;; NOTE: FEN-like notation should allow for spell events...
;; TODO 2024-05-23: crashes when board size is bigger than homerow list
(defun shoggy-board-setup (&optional FEN)
  "Setup `shoggy-board' initial position.
With optional argument FEN, set FEN string as the initial position."
  (shoggy-board-init)
  (let ((homerow (if shoggy-board-homerow-random-p
                     (shoggy-shuffle shoggy-board-homerow)
                   shoggy-board-homerow)))
    (dotimes (i shoggy-board-size)
      ;; Pawns
      (let ((pos (cons 1 i)))
        (shoggy-board-put-new 'p "black" pos))
      (let ((pos (cons (- shoggy-board-size 2) i)))
        (shoggy-board-put-new 'p "white" pos))
      ;; Home row
      (let ((pos-black (cons 0 i))
            (pos-white (cons (1- shoggy-board-size) i)))
        (shoggy-board-put-new (nth i (reverse homerow))
                              "black" pos-black)
        (shoggy-board-put-new (nth i  homerow)
                              "white" pos-white)))))

(defun shoggy-board-print ()
  "Print current `shoggy-board' as ASCII."
  (append
   (list (mapconcat #'number-to-string
                    (number-sequence 0 (1- shoggy-board-size))
                    " "))
   (mapcar (lambda (y)
             (mapcar (lambda (x)
                       (if x
                           (shoggy-piece-print x)
                         '-))
                     y))
           shoggy-board)))

(eval-when-compile (defvar shoggy-ui-square-labels))

(defun shoggy-board-notation-square (square)
  "Print board notation for SQUARE."
  (let ((r (car square))
        (c (cdr square))
        (labels (take shoggy-board-size shoggy-ui-square-labels)))
    (format "%s%s"
            (nth c (if (cl-oddp shoggy-board-flip-count)
                       (reverse labels)
                     labels))
            (if (cl-oddp shoggy-board-flip-count)
                (1+ r)
              (- shoggy-board-size r)))))

(defun shoggy-board-notation-move (from-square to-square &optional capture)
  "Print board notation for move FROM-SQUARE TO-SQUARE.
When CAPTURE is non-nil, print \"x\" in between squares."
  (when shoggy-board-ui-p
    (let ((piece (shoggy-board-get to-square)))
      (concat
       (unless (shoggy-piece-pawn-p piece)
         (upcase (format "%s" (shoggy-piece-print piece))))
       (substring (shoggy-board-notation-square from-square)
                  0 (cond ((and capture (shoggy-piece-pawn-p piece)) 1)
                          (t -2)))
       (and capture "x")
       (shoggy-board-notation-square to-square)))))

(defmacro shoggy-board-map (&rest body)
  "Map through all squares and run BODY."
  `(mapcar (lambda (r)
             (mapcar (lambda (c)
                       ,@body)
                     (number-sequence 0 (1- shoggy-board-size))))
           (number-sequence 0 (1- shoggy-board-size))))

(defmacro shoggy-board-map-flatten (&rest map)
  "Return MAP flattened by one level and without nil elements."
  (declare (indent defun))
  `(delete nil (apply #'append ,@map)))

(defun shoggy-board-swap-player-color ()
  "Swap player's color."
  (if (equal shoggy-player-color "black")
      (setq shoggy-player-color "white")
    (setq shoggy-player-color "black")))

(defun shoggy-board-updade-piece-positions ()
  "Update the position slot of all pieces on the board."
  (shoggy-board-map
   (when-let (piece (shoggy-board-get (cons r c)))
     (setf (shoggy-piece-position piece) (cons r c)))))

;; NOTE: We flip the board in order to calculate the legal moves,
;; since it always considers the current player's POV.
;; `shoggy-board-flip-count' gives a sense of turns; it should be
;; restored to its value after in-depth engine evaluation.
(defun shoggy-board-flip ()
  "Return `shoggy-board' flipped upside-down, swap `shoggy-player-color'."
  (shoggy-board-swap-player-color)
  (cl-incf shoggy-board-flip-count)
  (setq shoggy-board (mapcar #'reverse (reverse shoggy-board)))
  ;; NOTE: After flipping the board, pieces' positions must be updated.
  ;; We might need another (and better) way to get pieces' positions.
  ;; Given the board is small, this is not a real problem for now.
  (shoggy-board-updade-piece-positions))

;; FIXME 2024-05-17: unused
;; might be useful to return the precise notation for when
;; two same-type pieces are in the same row or column.
(defun shoggy-board-get-row (row)
  "Return `shoggy-board' row ROW."
  (elt shoggy-board row))

;; FIXME 2024-05-17: unused
(defun shoggy-board-get-col (col)
  "Return `shoggy-board' column COL."
  (mapcar (lambda (f) (elt f col)) shoggy-board))

;; NOTE: it does not test if the next square is within the board;
;; the testing is done in the methods for the legal moves
(defun shoggy-board-next (square direction)
  "Return the next SQUARE in DIRECTION."
  (let ((row (car square))
        (col (cdr square)))
    (and row col
         (cond ((or (eq direction 'north) (eq direction 'N))
                (cons (+ row -1) col))
               ((or (eq direction 'east) (eq direction 'E))
                (cons row (+ col 1)))
               ((or (eq direction 'south) (eq direction 'S))
                (cons (+ row 1) col))
               ((or (eq direction 'west) (eq direction 'W))
                (cons row (+ col -1)))
               ((or (eq direction 'northeast) (eq direction 'NE))
                (cons (+ row -1) (+ col 1)))
               ((or (eq direction 'northwest) (eq direction 'NW))
                (cons (+ row -1) (+ col -1)))
               ((or (eq direction 'southeast) (eq direction 'SE))
                (cons (+ row 1) (+ col 1)))
               ((or (eq direction 'southwest) (eq direction 'SW))
                (cons (+ row 1) (+ col -1)))))))


;;;; Legal moves

(cl-defgeneric shoggy-legal-moves (piece)
  "Return a list of legal moves for PIECE in current position.")


;;;;; Legal moves for rook-like and bishop-like pieces

(cl-defmethod shoggy-legal-moves ((piece shoggy-piece))
  "Return a list of legal moves for PIECE in current position."
  (let ((pos (shoggy-piece-position piece)))
    (delete
     nil
     (mapcan
      (lambda (d)
        (cl-loop with r = (car pos)
                 with c = (cdr pos)
                 with range = (shoggy-piece-range piece)
                 while (and (shoggy-board-square-p (cons r c))
                            (> range 0)
                            (not (shoggy-piece-enemy-p
                                  piece
                                  (shoggy-board-get (cons r c)))))
                 do (when-let (next (shoggy-board-next (cons r c) d))
                      (setq r (car next))
                      (setq c (cdr next))
                      (setq range (1- range)))
                 until (shoggy-piece-friend-p
                        piece
                        (shoggy-board-get (cons r c)))
                 ;; TODO: (if (piece-leaper-p piece) t ...)
                 collect (when (shoggy-board-square-p (cons r c))
                           (cons r c))))
      (shoggy-piece-direction piece)))))


;;;;; Legal moves for pawns

(cl-defmethod shoggy-legal-moves ((piece shoggy-piece-pawn))
  "Return a list of legal moves for pawn PIECE."
  (let ((pos (shoggy-piece-position piece)))
    (delete
     nil
     (append
      ;; Try move one square forward
      (list
       (let ((square (shoggy-board-next pos 'N)))
         (when (and (shoggy-board-square-p square)
                    (not (shoggy-board-get square)))
           square)))

      ;; If first move, try moving 2 squares forward
      (when shoggy-board-pawn-moves-two-squares-p
        (list
         (when (= (car pos) (- shoggy-board-size 2))
           (let ((square (shoggy-board-next (shoggy-board-next pos 'N) 'N)))
             (when (and (not (shoggy-board-get (shoggy-board-next pos 'N)))
                        (not (shoggy-board-get square)))
               square)))))

      ;; Try possible captures
      (delete
       nil
       (mapcar (lambda (d)
                 (let ((square (shoggy-board-next pos d)))
                   (when (and (shoggy-board-square-p square)
                              (shoggy-piece-enemy-p
                               piece (shoggy-board-get square)))
                     square)))
               '(NE NW)))))))


;;;;; Legal moves for knights

(cl-defmethod shoggy-legal-moves ((piece shoggy-piece-knight))
  "Return a list of legal moves for knight PIECE."
  (let ((pos (shoggy-piece-position piece)))
    (cl-remove-duplicates
     (delete
      nil
      (mapcan
       (lambda (d)
         (cl-loop
          with pos1 = pos
          for i from 1 to (car (shoggy-piece-leaper piece))
          do (setq pos1 (shoggy-board-next pos1 d))
          finally return
          (when (shoggy-board-square-p pos1)
            (mapcar
             (lambda (x)
               (cl-loop
                with pos2 = pos1
                for j from 1 to (cdr (shoggy-piece-leaper piece))
                do (setq pos2 (shoggy-board-next pos2 x))
                finally return
                (when (and (shoggy-board-square-p pos2)
                           (not (shoggy-piece-friend-p
                                 piece (shoggy-board-get pos2))))
                  pos2)))
             (if (or (eq d 'N) (eq d 'S))
                 '(E W)
               '(N S))))))
       '(N E S W)))
     :test #'equal)))


;;;; Game setup

;;;;; Load engine & UI definitions

(require 'shoggy-spell)
(require 'shoggy-engine)
(require 'shoggy-ui)
(require 'shoggy-splash)


;;;;; Game start

;;;###autoload
(defun shoggy ()
  (interactive)
  (shoggy-splash))

(defun shoggy-game-start ()
  "Start a new game."
  (interactive)
  (shoggy-board-setup)
  (shoggy-spell-init)
  (if (equal shoggy-user-color "black")
      (shoggy-board-flip)
    (setq shoggy-player-color "white")
    (shoggy-ui-board-redraw))

  (shoggy-ui-headerline-format "Game start!")
  (shoggy-ui-modeline-setup)
  (pop-to-buffer shoggy-board-buffer)
  (shoggy-ui-sound-play 'start)

  (when (equal shoggy-user-color "black")
    (setq shoggy-player-color "black")
    (shoggy-ui-board-redraw)
    (shoggy-engine-run)))


;;; Provide shoggy

(provide 'shoggy)

;;; shoggy.el ends here
