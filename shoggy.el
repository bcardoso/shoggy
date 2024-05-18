;;; shoggy.el --- A fairy chess game for Emacs -*- lexical-binding: t -*-

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

;; TODO 2024-05-18: translator of SQUARE to notation {PIECE}{x}{position}

;; TODO 2024-05-18: read FEN-link string as board initial setup; export to FEN

;; NOTE 2024-05-18: FEN input will be unreadable with spell cards enable


;;; Code:


;;;; User options



;;;; Variables

;; TODO 2024-05-17: write var docstrings

(defvar shoggy-board nil)

(defvar shoggy-board-size 6)

(defvar shoggy-captured-pieces nil)

(defvar shoggy-board-flip-count 0)

;; TODO 2024-05-18: user option: should pawn move 2 squares in the beginning?

;; TODO 2024-05-17: user option: fisher-random to shuffle default home row
(defvar shoggy-board-homerow '(c n f s w n))

;; REVIEW 2024-05-20: option; player's color should be set at game start
(defvar shoggy-player-color "white")


;;;; Functions

(defun shoggy-shuffle (seq &optional n)
  "Return SEQ shuffled.
If N is a number, take the first N elements of the shuffled SEQ."
  (let ((shuffle (seq-sort-by (lambda (_) (random)) #'<= seq)))
    (if (numberp n)
        (take n shuffle)
      shuffle)))


;;;; Pieces

;; REVIEW: reconsider some slots
(cl-defstruct (shoggy-piece (:constructor shoggy-piece-make)
                            (:copier nil))
  "A shoggy piece."
  color     ;; "white" or "black"
  name      ;; piece name (string)
  atom      ;; single letter representation (symbol)
  value     ;; piece value (number)
  range     ;; piece square range (number)
  ;; REVIEW 2024-05-17: use direction names (N E S W NE SE SW NW) *and*
  ;; direction groups ('all orthogonal 'diagonal 'hippogonal)?
  direction ;; list of directions
  leaper    ;; whether piece is a leaper (boolean)
  position  ;; current position in the board (cons)
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

(cl-defstruct (shoggy-piece-chariot
               (:constructor shoggy-piece-make-chariot)
               (:copier nil)
               (:include shoggy-piece
                         (name "Chariot")
                         (atom 'c)
                         (value 5)
                         (range shoggy-board-size)
                         (direction '(N E S W)))))

(defun shoggy-piece-print (piece)
  "Print piece atom as in FEN notation. White pieces are uppercase letters."
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
  (setq shoggy-captured-pieces nil)
  (setq shoggy-board-flip-count 0)
  (setq shoggy-board (make-list shoggy-board-size nil))
  (dotimes (i shoggy-board-size)
    (setf (nth i shoggy-board) (make-vector shoggy-board-size nil))))

(defun shoggy-board-square-p (square)
  "Return SQUARE if it is a valid square in `shoggy-board'."
  (let ((row (car square))
        (col (cdr square)))
    (and (>= row 0) (>= col 0)
         (< row shoggy-board-size) (< col shoggy-board-size)
         square)))

(defmacro shoggy-board-get (square)
  "Return the piece that is in SQUARE. If it is empty, return nil."
  `(when (shoggy-board-square-p ,square)
     (elt (elt shoggy-board (car ,square)) (cdr ,square))))

(defun shoggy-board-put (piece square)
  "Put PIECE in SQUARE position."
  (setf (shoggy-piece-position piece) square)
  (setf (shoggy-board-get square) piece))

(defun shoggy-board-put-new (atom color square)
  "Make piece of type ATOM and COLOR and put it in SQUARE."
  (let ((fn (cond ((eq atom 'p) #'shoggy-piece-make-pawn)
                  ((eq atom 'f) #'shoggy-piece-make-ferz)
                  ((eq atom 'w) #'shoggy-piece-make-wazir)
                  ((eq atom 's) #'shoggy-piece-make-sage)
                  ((eq atom 'n) #'shoggy-piece-make-knight)
                  ((eq atom 'c) #'shoggy-piece-make-chariot))))
    (shoggy-board-put (funcall fn :color color :position square) square)))

(defun shoggy-board-pop (square)
  "Clear SQUARE and return the piece that was on it."
  (when-let (piece (shoggy-board-get square))
    (setf (shoggy-piece-position piece) nil)
    (setf (shoggy-board-get square) nil)
    piece))

(defun shoggy-board-move (from-square to-square)
  "Move piece from FROM-SQUARE to TO-SQUARE."
  (when-let (piece (shoggy-board-pop to-square))
    (push piece shoggy-captured-pieces))
  (shoggy-board-put (shoggy-board-pop from-square) to-square)
  ;; Pawn promotion
  (when (and (shoggy-piece-pawn-p (shoggy-board-get to-square))
             (= (car to-square) 0))
    ;; FIXME 2024-05-18: promotion should prompt for a new piece
    ;; use `shoggy-board-put-new' to make piece
    ;; promoting to a Knight for now...
    (shoggy-board-put (shoggy-piece-make-knight :color shoggy-player-color)
                      to-square)))

;; IDEA: implement option to read FEN-like notation into board
;; NOTE: FEN-like notation should allow for spell events...
(defun shoggy-board-setup (&optional FEN)
  "Setup `shoggy-board' initial position.
With optional argument FEN, set FEN string as the initial position."
  (shoggy-board-init)
  (dotimes (i shoggy-board-size)
    ;; Pawns
    (let ((pos (cons 1 i)))
      (shoggy-board-put-new 'p "black" pos))
    (let ((pos (cons (- shoggy-board-size 2) i)))
      (shoggy-board-put-new 'p "white" pos))
    ;; Home row
    (let ((pos-black (cons 0 i))
          (pos-white (cons (1- shoggy-board-size) i))
          (p (nth i shoggy-board-homerow)))
      (shoggy-board-put-new (nth i (reverse shoggy-board-homerow))
                            "black" pos-black)
      (shoggy-board-put-new (nth i  shoggy-board-homerow)
                            "white" pos-white))))

;; NOTE: for debugging only?
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

(defmacro shoggy-board-map (&rest body)
  "Map through all squares and run BODY."
  `(mapcar (lambda (r)
             (mapcar (lambda (c)
                       ,@body)
                     (number-sequence 0 (1- shoggy-board-size))))
           (number-sequence 0 (1- shoggy-board-size))))

(defun shoggy-board-swap-player-color ()
  "Swap player's color."
  (if (equal shoggy-player-color "black")
      (setq shoggy-player-color "white")
    (setq shoggy-player-color "black")))

;; NOTE: We flip the board in order to calculate the legal moves,
;; since it always considers the current player's POV.
;; NOTE: `shoggy-board-flip-count' gives a sense of turns; it should be
;; restored to its value after the engine evaluation.
(defun shoggy-board-flip ()
  "Return `shoggy-board' flipped upside-down and swap `shoggy-player-color'."
  (shoggy-board-swap-player-color)
  (cl-incf shoggy-board-flip-count)
  (setq shoggy-board (mapcar #'reverse (reverse shoggy-board)))
  ;; NOTE: After flipping the board, pieces' positions must be updated.
  ;; We might need another (and better) way to get pieces' positions.
  ;; Given the board is small, this is not a real problem for now.
  (shoggy-board-map
   (when-let (p (shoggy-board-get (cons r c)))
     (setf (shoggy-piece-position p) (cons r c)))))

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
           (cons (+ row 1) (+ col -1))))))


;;;; Legal moves

(cl-defgeneric shoggy-board-legal-moves (piece)
  "Return a list of legal moves for PIECE in current position.")


;;;;; Legal moves for rook-like and bishop-like pieces

(cl-defmethod shoggy-board-legal-moves ((piece shoggy-piece))
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

(cl-defmethod shoggy-board-legal-moves ((piece shoggy-piece-pawn))
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
      (list
       (when (= (car pos) (- shoggy-board-size 2))
         (let ((square (shoggy-board-next (shoggy-board-next pos 'N) 'N)))
           (when (and (not (shoggy-board-get (shoggy-board-next pos 'N)))
                      (not (shoggy-board-get square)))
             square))))

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

(cl-defmethod shoggy-board-legal-moves ((piece shoggy-piece-knight))
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


;;; Provide shoggy

(provide 'shoggy)

;;; shoggy.el ends here