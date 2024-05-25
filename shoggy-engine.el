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

;; TODO 2024-05-25: engine card play
(defun shoggy-engine-run ()
  "Engine makes a move or plays a card."
  (funcall shoggy-engine))

(defun shoggy-engine-wait ()
  "Wait for a random amount of seconds as if thinking about the position."
  (sit-for (car (shoggy-shuffle '(0.75 1 1 1 1.25 1.5 1.75 2 2) 1))))

(defun shoggy-engine-square-value (square)
  "Return the value of piece in SQUARE. Return 0 if empty."
  (if-let (piece (shoggy-board-get square))
      (shoggy-piece-value piece)
    0))

(defun shoggy-engine-square-relative-value (square piece)
  "Return the relative value of SQUARE from the perspective of PIECE."
  (let ((v (shoggy-engine-square-value square)))
    (cond ((> v 0)
           (- (* 2.1 v)
              (shoggy-piece-value piece)))
          ((and (shoggy-piece-pawn-p piece)
                (> (car (shoggy-piece-position piece)) 2))
           0.5)
          (t
           v))))

(defun shoggy-engine-legal-moves-in-position ()
  "Return a plist of legal moves for each piece in current position."
  (shoggy-board-map-flatten
    (apply
     #'append
     (shoggy-board-map
      (let ((piece (shoggy-board-get (cons r c))))
        (when (shoggy-piece-own-p piece)
          (mapcar
           (lambda (s)
             (list :from (cons r c)
                   :to s
                   :value (shoggy-engine-square-relative-value s piece)))
           (shoggy-legal-moves piece))))))))

(defun shoggy-engine-sort-moves-by-value (legal-moves)
  "Sort the plist of LEGAL-MOVES by the :value property."
  (sort (shoggy-shuffle legal-moves)
        (lambda (m1 m2)
          (> (plist-get m1 :value) (plist-get m2 :value)))))

(defun shoggy-engine-most-valuable-move (legal-moves &optional n)
  "From a LEGAL-MOVES list, return the most valuable move.
If all moves are the same, return a random one.
When N is a number, return N moves."
  (take (or (and (numberp n) n) 1)
        (shoggy-engine-sort-moves-by-value legal-moves)))

(defun shoggy-engine-convert-move-to-squares (move)
  "Convert MOVE element to a list of squares (from-square to-square)."
  (when move
    (cl-flet ((convert (s)
                (cons (- (1- shoggy-board-size) (car s))
                      (- (1- shoggy-board-size) (cdr s)))))
      (list (convert (plist-get move :from))
            (convert (plist-get move :to))))))

(defun shoggy-engine-eval-board ()
  "Return an integer representing the current position eval.
A positive integer means the position is in `shoggy-player-color' favor.
A negative integer means it is in opponent's favor.
Zero means position is in balance."
  (let ((player-sum 0)
        (opponent-sum 0))
    (shoggy-board-map
     (when-let* ((piece (shoggy-board-get (cons r c)))
                 (value (shoggy-piece-value piece)))
       (if (shoggy-piece-own-p piece)
           (cl-incf player-sum value)
         (cl-incf opponent-sum value))))
    (- player-sum opponent-sum)))

(defun shoggy-engine-eval-captures ()
  "Return an integer representing the current position eval.
A positive integer means the position is in `shoggy-player-color' favor.
A negative integer means it is in opponent's favor.
Zero means position is in balance."
  (let ((player-sum 0)
        (opponent-sum 0))
    (when shoggy-captured-pieces
      (mapc (lambda (piece)
              (let ((value (shoggy-piece-value piece)))
                (if (shoggy-piece-own-p piece)
                    (cl-incf player-sum value)
                  (cl-incf opponent-sum value))))
            shoggy-captured-pieces))
    (- player-sum opponent-sum)))


;;;; Dumbfish: tries to capture a piece or choose a random move

;; list of legal moves with captures
;; (seq-filter (lambda (m) (> (plist-get m :value) 0))
;;             (shoggy-engine-legal-moves-in-position))

(defun shoggy-engine-make-move (move)
  "Make MOVE."
  (shoggy-board-move (plist-get move :from) (plist-get move :to)))

;; NOTE 2024-05-19: function body can be a macro; the difference between
;; engines is how they choose their moves, all else is pretty much the same
(defun shoggy-engine-dumbfish ()
  "Simple engine. Try to capture a piece or choose a random move."
  (shoggy-engine-wait)
  (shoggy-board-flip)
  (let ((move (car (shoggy-engine-most-valuable-move
                    (shoggy-engine-legal-moves-in-position)))))
    (if move
        (shoggy-engine-make-move move)
      ;; FIXME 2024-05-19: this is not right
      (shoggy-board-game-over "No more moves!"))
    (shoggy-board-flip)
    (shoggy-ui-board-redraw (shoggy-engine-convert-move-to-squares move))))


;;;; Sanefish: evaluates the position to make a reasonable move

(defvar shoggy-engine--state nil
  "Current game state.")

(defmacro shoggy-engine--write-state (buffer-name data)
  (declare (indent defun))
  `(with-current-buffer
       (get-buffer-create (format " *shoggy-%s-state*" ,buffer-name))
     (erase-buffer)
     (insert (format "%S" ,data))))

(defmacro shoggy-engine--read-state (buffer-name data)
  (declare (indent defun))
  `(with-current-buffer
       (get-buffer-create (format " *shoggy-%s-state*" ,buffer-name))
     (goto-char (point-min))
     (setq ,data (read (current-buffer)))))

(defun shoggy-engine-save-state ()
  "Save current game state."
  (shoggy-engine--write-state "board" shoggy-board)
  (shoggy-engine--write-state "captured" shoggy-captured-pieces)
  (setq shoggy-engine--state (cons shoggy-board-flip-count
                                   shoggy-player-color)))

(defun shoggy-engine-restore-state ()
  "Restore current game state."
  (shoggy-engine--read-state "board" shoggy-board)
  (shoggy-engine--read-state "captured" shoggy-captured-pieces)
  (setq shoggy-board-flip-count (car shoggy-engine--state))
  (setq shoggy-player-color     (cdr shoggy-engine--state)))

(eval-when-compile (defvar shoggy-board-ui-p))

;; NOTE 2024-05-23: this is kinda hideous
(defun shoggy-engine-move-eval ()
  (let ((shoggy-board-ui-p nil)
        (eval (shoggy-engine-eval-captures))
        (color shoggy-player-color))
    (cl-flet ((make-move ()
                (shoggy-engine-make-move
                 (car (shoggy-engine-most-valuable-move
                       (shoggy-engine-legal-moves-in-position))))
                (shoggy-board-flip)
                (setq eval (* (if (equal color shoggy-player-color)
                                  1 -1)
                              (shoggy-engine-eval-captures)))))
      (cl-loop for x from 0 to 2
               do (make-move)
               do (make-move)
               finally return eval))))

(defun shoggy-engine-sanefish-move ()
  "Return a Sanefish move after restoring board state."
  (shoggy-engine-save-state)
  (let ((shoggy-board-ui-p nil))
    (prog1
        (let ((moves (shoggy-engine-most-valuable-move
                      (shoggy-engine-legal-moves-in-position) 5)))
          (car (shoggy-engine-most-valuable-move
                (mapcar (lambda (move)
                          (shoggy-engine-restore-state)
                          (shoggy-engine-make-move move)
                          (shoggy-board-flip)
                          (plist-put move :value (shoggy-engine-move-eval)))
                        moves))))
      (shoggy-engine-restore-state)
      (shoggy-ui-board-redraw))))


;;; Provide shoggy-engine

(provide 'shoggy-engine)

;;; shoggy-engine.el ends here
