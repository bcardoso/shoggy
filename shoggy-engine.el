;;; shoggy-engine.el --- Engines for shoggy -*- lexical-binding: t -*-

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

;; Engines for shoggy.

;; `shoggy-engine-dumbfish': Tries to capture a piece or makes a random move
;; `shoggy-engine-sanefish': Evaluates the position to make a reasonable move


;;; Code:

(defun shoggy-engine-square-value (square)
  "Return the value of piece in SQUARE. Return 0 if empty."
  (if-let (piece (shoggy-board-get square))
      (shoggy-piece-value piece)
    0))

(defun shoggy-engine-legal-moves-in-position ()
  "Return a plist of legal moves for each piece in current position."
  (delete
   nil
   (apply #'append
          (apply #'append
                 (shoggy-board-map
                  (let ((piece (shoggy-board-get (cons r c))))
                    (when (shoggy-piece-own-p piece)
                      (mapcar (lambda (s)
                                (list :from (cons r c)
                                      :to s
                                      :value (shoggy-engine-square-value s)))
                              (shoggy-board-legal-moves piece)))))))))

(defun shoggy-engine-sort-moves-by-value (legal-moves)
  (sort (shoggy-shuffle legal-moves)
        (lambda (m1 m2)
          (> (plist-get m1 :value) (plist-get m2 :value)))))

(defun shoggy-engine-most-valuable-move (legal-moves)
  "From a LEGAL-MOVES list, return the most valuable move.
If all moves are the same, return a random one."
  (take 1 (shoggy-engine-sort-moves-by-value legal-moves)))

;; TODO 2024-05-18: shoggy-board-eval -- for minimax
(defun shoggy-board-eval ()
  "Return an integer representing the current position eval.
A positive integer means the current position is in white's favor.
A negative integer, in black's favor. Zero means position is in balance."
  )


;;;; Dumbfish: tries to capture a piece or choose a random move

;; list of legal moves with captures
;; (seq-filter (lambda (m) (> (plist-get m :value) 0))
;;             (shoggy-engine-legal-moves-in-position))

(defun shoggy-engine-dumbfish ()
  "Simple engine. Tries to capture a piece or choose a random move."
  ;; TODO: turn it into a waiting function
  (sit-for (car (shoggy-shuffle '(1 1 1 2 2 2 3) 1)))
  (shoggy-board-flip)
  (let ((move (car (shoggy-engine-most-valuable-move
                    (shoggy-engine-legal-moves-in-position)))))
    ;; TODO turn this into a game-end function; here it ends only when
    ;; its out of moves, but other endings should be considered here
    (if move
        (shoggy-board-move (plist-get move :from) (plist-get move :to))
      (message "You win!"))
    (shoggy-board-flip)
    (shoggy-ui-board-redraw
     ;; highlight engine's last move
     (when move
       (cl-flet ((convert (s)
                   (cons (- (1- shoggy-board-size) (car s))
                         (- (1- shoggy-board-size) (cdr s)))))
         (list (convert (plist-get move :from))
               (convert (plist-get move :to))))))))



;;; Provide shoggy-engine

(provide 'shoggy-engine)

;;; shoggy-engine.el ends here
