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


;;; Provide shoggy

(provide 'shoggy)

;;; shoggy.el ends here
