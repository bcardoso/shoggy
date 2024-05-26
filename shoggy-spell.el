;;; shoggy-spell.el --- Spells for shoggy -*- lexical-binding: t -*-

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

;; Spell cards mechanics.


;;; Code:

(declare-function shoggy-shuffle "shoggy")
(declare-function shoggy-engine-run "shoggy-engine")

(defvar shoggy-spell-cards '("Boost" "Promote" "Demote"))
(defvar shoggy-spell-deck nil)
(defvar shoggy-spell-deck-opponent nil)
(defvar shoggy-spell-deck-max-size 3)

(defun shoggy-spell-init ()
  "Reset decks."
  (setq shoggy-spell-deck nil)
  (setq shoggy-spell-deck-opponent nil))

(defun shoggy-spell-list ()
  "Return an alist of user's spell cards and action functions."
  (mapcar (lambda (spell)
            (cond ((equal spell "Boost")
                   (cons spell 'shoggy-spell-boost))
                  ((equal spell "Promote")
                   (cons spell 'shoggy-spell-promote))
                  ((equal spell "Demote")
                   (cons spell 'shoggy-spell-demote))))
          shoggy-spell-deck))


;;;; Draw a spell card

(defun shoggy-spell-draw-card ()
  "Add a random card to `shoggy-spell-deck'.
If deck size is greater than `shoggy-spell-deck-max-size', also discard a
random card. Return the deck."
  (let ((deck (if (shoggy-user-p)
                  shoggy-spell-deck
                shoggy-spell-deck-opponent))
        (card (car (shoggy-shuffle shoggy-spell-cards 1))))
    (when (>= (length deck) shoggy-spell-deck-max-size)
      (setq deck (shoggy-shuffle deck 2)))
    (push card deck)
    (prog1
        (if (shoggy-user-p)
            (setq shoggy-spell-deck deck)
          (setq shoggy-spell-deck-opponent deck))
      (when shoggy-board-ui-p
        (with-current-buffer (shoggy-get-buffer)
          (shoggy-ui-modeline-setup))))))


;;;; Discard a spell card

(defun shoggy-spell-discard-card (card)
  "Discard CARD from deck."
  (let ((deck (if (shoggy-user-p)
                  'shoggy-spell-deck
                'shoggy-spell-deck-opponent)))
    (prog1
        (setf (symbol-value deck)
              (cl-remove card (symbol-value deck) :count 1 :test #'equal))
      (when shoggy-board-ui-p
        (with-current-buffer (shoggy-get-buffer)
          (shoggy-ui-modeline-setup))))))


;;;; Spell casting

(defun shoggy-spell-setup (card square-list action-fn &optional other-fn)
  "Setup `shoggy-ui-board-svg' properties for spell actions.
CARD is the name of the card to be discarded.
SQUARE-LIST is the list of relevant squares for a spell.
ACTION-FN is a function that should run when user click on a square.
OTHER-FN is a function that is not a clickable event."
  (shoggy-ui-board-redraw)
  (shoggy-spell-discard-card card)
  (shoggy-ui-board-update
    (setq shoggy-ui-board--keymap (make-sparse-keymap))
    (setq shoggy-ui-board--square-map nil)
    (mapc (lambda (square)
            (shoggy-ui-board-highlight-square
             square shoggy-ui-square-color-changed)
            (when action-fn
              (add-to-list
               'shoggy-ui-board--square-map
               (shoggy-ui-board-square-props square
                                             shoggy-ui-board--keymap
                                             action-fn))))
          square-list))
  (and other-fn (funcall other-fn))
  (shoggy-ui-board-set-pieces))


;;;;; Boost piece range

(defun shoggy-spell-boost-action (square)
  "Boost range of piece on SQUARE."
  (setf (shoggy-piece-range (shoggy-board-get square)) shoggy-board-size)
  (setf (shoggy-piece-value (shoggy-board-get square)) 5)
  (setf (shoggy-piece-boosted (shoggy-board-get square)) t)
  (shoggy-ui-headerline-format "Piece boosted!" 'spell)
  (when (shoggy-user-p)
    (shoggy-ui-board-redraw)
    (shoggy-engine-run)))

(defun shoggy-spell-boost-get-squares ()
  "Return a list of squares with pieces that can be boosted."
  (shoggy-board-map-flatten
    (shoggy-board-map
     (let ((piece (shoggy-board-get (cons r c))))
       (when (and piece
                  (equal (shoggy-piece-color piece)
                         shoggy-player-color)
                  (or (shoggy-piece-ferz-p piece)
                      (shoggy-piece-wazir-p piece)))
         (cons r c))))))

(defun shoggy-spell-boost ()
  "Setup the squares with the pieces that can be boosted (Ferz and Wazir)."
  (let ((squares (shoggy-spell-boost-get-squares)))
    (if squares
        (shoggy-spell-setup "Boost" squares #'shoggy-spell-boost-action)
      (shoggy-ui-headerline-format "No piece to boost! Card vanishes!")
      (shoggy-spell-discard-card "Boost"))))


;;;;; Promote piece

(defun shoggy-spell-promote-action (square)
  "Promote a piece on SQUARE."
  (let ((piece (shoggy-board-get square)))
    (shoggy-board-pop square)
    (shoggy-board-put-new
     (cond ((shoggy-piece-pawn-p piece) (car (shoggy-shuffle '(f w) 1)))
           ((shoggy-piece-ferz-p piece) 'n)
           ((shoggy-piece-wazir-p piece) 'n)
           ((shoggy-piece-knight-p piece) 'c))
     shoggy-player-color
     square))
  (shoggy-ui-headerline-format "Piece promoted!" 'spell)
  (when (shoggy-user-p)
    (shoggy-ui-board-redraw)
    (shoggy-engine-run)))

(defun shoggy-spell-promote-get-squares ()
  "Return a list of squares with pieces that can be promoted."
  (shoggy-board-map-flatten
    (shoggy-board-map
     (let ((piece (shoggy-board-get (cons r c))))
       (when (and piece
                  (equal (shoggy-piece-color piece)
                         shoggy-player-color)
                  (not (shoggy-piece-chariot-p piece))
                  (not (shoggy-piece-sage-p piece)))
         (cons r c))))))

(defun shoggy-spell-promote ()
  "Setup the squares with the *player's* pieces that can be promoted.
Promotion order: Pawn -> Ferz/Wazir -> Knight -> Chariot."
  (let ((squares (shoggy-spell-promote-get-squares)))
    (if squares
        (shoggy-spell-setup "Promote" squares #'shoggy-spell-promote-action))
    (shoggy-ui-headerline-format "No piece to promote! Card vanishes!")
    (shoggy-spell-discard-card "Promote")))


;;;;; Demote piece

(defun shoggy-spell-demote-action (square)
  "Demote a piece on SQUARE."
  (let ((piece (shoggy-board-get square)))
    (shoggy-board-pop square)
    (shoggy-board-put-new
     (cond ((shoggy-piece-ferz-p piece) 'p)
           ((shoggy-piece-wazir-p piece) 'p)
           ((shoggy-piece-knight-p piece) (car (shoggy-shuffle '(f w) 1)))
           ((shoggy-piece-chariot-p piece) 'n))
     (if (equal shoggy-player-color "white") "black" "white")
     square))
  (shoggy-ui-headerline-format "Piece demoted!" 'spell)
  (when (shoggy-user-p)
    (shoggy-ui-board-redraw)
    (shoggy-engine-run)))

(defun shoggy-spell-demote-get-squares ()
  "Return a list of squares with pieces that can be demoted."
  (shoggy-board-map-flatten
    (shoggy-board-map
     (let ((piece (shoggy-board-get (cons r c))))
       (when (and piece
                  (not (equal (shoggy-piece-color piece)
                              shoggy-player-color))
                  (not (shoggy-piece-pawn-p piece))
                  (not (shoggy-piece-sage-p piece)))
         (cons r c))))))

(defun shoggy-spell-demote ()
  "Setup the squares with the *enemy's* pieces that can be demoted.
Demotion order: Chariot -> Knight -> Ferz/Wazir -> Pawn."
  (let ((squares (shoggy-spell-demote-get-squares)))
    (if squares
        (shoggy-spell-setup "Demote" squares #'shoggy-spell-demote-action)
      (shoggy-ui-headerline-format "No piece to demote! Card vanishes!")
      (shoggy-spell-discard-card "Demote"))))


;;; Provide shoggy-spell

(provide 'shoggy-spell)

;;; shoggy-spell.el ends here
