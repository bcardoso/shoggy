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

;; `shoggy-engine-dumbfish' tries to capture a piece or makes a random move
;; `shoggy-engine-sanefish' evaluates the position to make a reasonable move


;;; Code:

(defun shoggy-engine-wait ()
  "Wait for a random amount of seconds as if thinking about the position."
  (sit-for (car (shoggy-shuffle '(0.75 1 1 1 1.25 1.5 1.75 2) 1))))

(defun shoggy-engine-square-value (square)
  "Return the value of piece in SQUARE. Return 0 if empty."
  (if-let (piece (shoggy-board-get square))
      (shoggy-piece-value piece)
    0))

(defun shoggy-engine-square-relative-value (square piece)
  "Return the relative value of SQUARE from the perspective of PIECE."
  (let ((square-value (shoggy-engine-square-value square)))
    (cond ((> square-value 0)
           (max square-value (shoggy-piece-value piece)))
          ((and (shoggy-piece-pawn-p piece)
                (> (car (shoggy-piece-position piece)) 2))
           0.5)
          (t
           square-value))))

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

(defun shoggy-engine-sort-squares-by-value (squares)
  "Sort the list SQUARES by the piece value."
  (sort (shoggy-shuffle squares)
        (lambda (s1 s2)
          (> (shoggy-engine-square-value s1)
             (shoggy-engine-square-value s2)))))

(defun shoggy-engine-sort-moves-by-value (legal-moves)
  "Sort the plist of LEGAL-MOVES by the :value property."
  (sort (shoggy-shuffle legal-moves)
        (lambda (m1 m2)
          (> (plist-get m1 :value)
             (plist-get m2 :value)))))

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
                    (cl-incf opponent-sum value)
                  (cl-incf player-sum value))))
            shoggy-captured-pieces))
    (- player-sum opponent-sum)))

(defun shoggy-engine-make-move (move)
  "Make MOVE."
  (shoggy-board-move (plist-get move :from) (plist-get move :to)))


;;;; Dumbfish: tries to capture a piece or choose a random move

(defun shoggy-engine-dumbfish-move ()
  "Return a Dumbfish move. Try to capture a piece or choose a random move."
  (car (shoggy-engine-most-valuable-move
        (shoggy-engine-legal-moves-in-position))))


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
  (setq shoggy-engine--state `( :counter ,shoggy-board-flip-count
                                :color ,shoggy-player-color
                                :deck1 ,shoggy-spell-deck
                                :deck2 ,shoggy-spell-deck-opponent)))

(defun shoggy-engine-restore-state ()
  "Restore current game state."
  (shoggy-engine--read-state "board" shoggy-board)
  (shoggy-engine--read-state "captured" shoggy-captured-pieces)
  (setq shoggy-board-flip-count (plist-get shoggy-engine--state :counter))
  (setq shoggy-player-color (plist-get shoggy-engine--state :color))
  (setq shoggy-spell-deck (plist-get shoggy-engine--state :deck1))
  (setq shoggy-spell-deck-opponent (plist-get shoggy-engine--state :deck2)))

(eval-when-compile (defvar shoggy-board-ui-p))

(defun shoggy-engine-move-eval (&optional depth)
  "Return the eval of the position in DEPTH."
  (let ((shoggy-board-ui-p nil)
        (moves (shoggy-engine-legal-moves-in-position))
        (d (or depth 5)))
    (if (or (<= d 0) (not moves) )
        (shoggy-engine-eval-captures)
      (shoggy-engine-make-move
       (car (shoggy-engine-most-valuable-move moves 1)))
      (shoggy-board-flip)
      (shoggy-engine-move-eval (1- d)))))

(defun shoggy-engine-sanefish-move ()
  "Return a Sanefish move after restoring board state."
  (shoggy-engine-save-state)
  (let ((shoggy-board-ui-p nil))
    (prog1
        (let ((moves (shoggy-engine-legal-moves-in-position)))
          (car (shoggy-engine-most-valuable-move
                (mapcar (lambda (move)
                          (shoggy-engine-restore-state)
                          (shoggy-engine-make-move move)
                          (shoggy-board-flip)
                          (plist-put move :value (shoggy-engine-move-eval)))
                        moves))))
      (shoggy-engine-restore-state)
      (shoggy-ui-board-redraw))))


;;;; Engine spell

(defun shoggy-engine-try-spell-p ()
  "Return t if engine should try to cast a spell."
  (and (< 3 (car (shoggy-shuffle (number-sequence 0 9) 1)))
       (>= 2 (plist-get (car (shoggy-engine-most-valuable-move
                              (shoggy-engine-legal-moves-in-position)))
                        :value))))

(defun shoggy-engine-flip-square (square)
  "Return SQUARE flipped."
  (cons (- (1- shoggy-board-size) (car square))
        (- (1- shoggy-board-size) (cdr square))))

(defun shoggy-engine-spell ()
  (let* ((spell (car (shoggy-shuffle shoggy-spell-deck-opponent 1)))
         (square-action (cond ((equal spell "Boost")
                               (cons (shoggy-spell-boost-get-squares)
                                     #'shoggy-spell-boost-action))
                              ((equal spell "Promote")
                               (cons (shoggy-spell-promote-get-squares)
                                     #'shoggy-spell-promote-action))
                              ((equal spell "Demote")
                               (cons (shoggy-spell-demote-get-squares)
                                     #'shoggy-spell-demote-action)))))
    (if (not (car square-action))
        (shoggy-spell-fail spell)
      (let* ((squares (car square-action))
             (square (car (shoggy-engine-sort-squares-by-value squares))))
        ;; NOTE 2024-05-26: paint squares from user's POV
        (shoggy-ui-board-update
          (setq shoggy-ui-board--square-map nil)
          (mapc (lambda (s)
                  (shoggy-ui-board-highlight-square
                   (shoggy-engine-flip-square s)
                   shoggy-ui-square-color-changed))
                squares))
        (sit-for 1)
        (funcall (cdr square-action) square)
        (sit-for 0.5)
        (shoggy-board-flip)
        (shoggy-ui-board-redraw
         (list (shoggy-engine-flip-square square))
         shoggy-ui-square-color-changed)))
    (shoggy-spell-discard-card spell)))


;;;; Engine run

;; list of legal moves with captures
;; (seq-filter (lambda (m) (> (plist-get m :value) 0))
;;             (shoggy-engine-legal-moves-in-position))

(defun shoggy-engine-run ()
  "Engine turn. Make a move or plays a card."
  (shoggy-engine-wait)
  (shoggy-board-flip)
  (if (and shoggy-spell-deck-opponent (shoggy-engine-try-spell-p))
      (shoggy-engine-spell)
    (let ((move (if (eq shoggy-engine 'sanefish)
                    (shoggy-engine-sanefish-move)
                  (shoggy-engine-dumbfish-move))))
      (if (not move)
          (shoggy-board-game-over (format "No more moves! %s is lost!"
                                          (capitalize shoggy-player-color)))
        ;; HACK 2024-05-26: we need to know if move is a capture
        ;; to play the correct sound
        (let ((capture-p (shoggy-piece-p (shoggy-board-get
                                          (plist-get move :to)))))
          (shoggy-engine-make-move move)
          (shoggy-ui-sound-play (if capture-p 'capture 'move)))
        (shoggy-board-flip)
        (shoggy-ui-board-redraw (shoggy-engine-convert-move-to-squares
                                 move))))))


;;; Provide shoggy-engine

(provide 'shoggy-engine)

;;; shoggy-engine.el ends here
