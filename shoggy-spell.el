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

(defvar shoggy-spell-deck '("Boost" "Promote" "Demote" "None"))

;;;; Spell casting

(defun shoggy-spell-deck ()
  "Prompt for a spell card in variable `shoggy-spell-deck' and cast it."
  (interactive)
  ;; TODO 2024-05-20: use widgets to pick cards
  (let ((card (ido-completing-read "Spell card: " shoggy-spell-deck)))
    (funcall (cond ((equal card "Boost")
                    #'shoggy-spell-boost)
                   ((equal card "Promote")
                    #'shoggy-spell-promote)
                   ((equal card "Demote")
                    #'shoggy-spell-demote)
                   (t #'ignore)))))

(defun shoggy-spell-setup (square-list action-fn &optional other-fn)
  "Setup `shoggy-ui-board-svg' properties for spell actions.
SQUARE-LIST is the list of relevant squares for a spell.
ACTION-FN is a function that should run when user click on a square.
OTHER-FN is a function that is not a clickable event."
  (shoggy-ui-board-redraw)
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
  (setf (shoggy-piece-boosted (shoggy-board-get square)) t)
  (shoggy-ui-headerline-setup
   (shoggy-ui-headerline-format "Piece boosted!" 'spell))
  (shoggy-ui-board-redraw))

(defun shoggy-spell-boost ()
  "Setup the squares with the pieces that can be boosted (Ferz and Wazir)."
  (let ((squares (shoggy-board-map-flatten
                   (shoggy-board-map
                    (let ((piece (shoggy-board-get (cons r c))))
                      (when (and piece
                                 (equal (shoggy-piece-color piece)
                                        shoggy-player-color)
                                 (or (shoggy-piece-ferz-p piece)
                                     (shoggy-piece-wazir-p piece)))
                        (cons r c)))))))
    (if squares
        (shoggy-spell-setup squares #'shoggy-spell-boost-action)
      (shoggy-ui-headerline-setup
       (shoggy-ui-headerline-format "No piece to boost! Card vanishes!")))))


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
  (shoggy-ui-headerline-setup
   (shoggy-ui-headerline-format "Piece promoted!" 'spell))
  (shoggy-ui-board-redraw))

(defun shoggy-spell-promote ()
  "Setup the squares with the *player's* pieces that can be promoted.
Promotion order: Pawn -> Ferz/Wazir -> Knight -> Chariot."
  (let ((squares (shoggy-board-map-flatten
                   (shoggy-board-map
                    (let ((piece (shoggy-board-get (cons r c))))
                      (when (and piece
                                 (equal (shoggy-piece-color piece)
                                        shoggy-player-color)
                                 (not (shoggy-piece-chariot-p piece))
                                 (not (shoggy-piece-sage-p piece)))
                        (cons r c)))))))
    (shoggy-spell-setup squares #'shoggy-spell-promote-action)))


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
  (shoggy-ui-headerline-setup
   (shoggy-ui-headerline-format "Piece demoted!" 'spell))
  (shoggy-ui-board-redraw))

(defun shoggy-spell-demote ()
  "Setup the squares with the *enemy's* pieces that can be demoted.
Demotion order: Chariot -> Knight -> Ferz/Wazir -> Pawn."
  (let ((squares (shoggy-board-map-flatten
                   (shoggy-board-map
                    (let ((piece (shoggy-board-get (cons r c))))
                      (when (and piece
                                 (not (equal (shoggy-piece-color piece)
                                             shoggy-player-color))
                                 (not (shoggy-piece-pawn-p piece))
                                 (not (shoggy-piece-sage-p piece)))
                        (cons r c)))))))
    (shoggy-spell-setup squares #'shoggy-spell-demote-action)))


;;; Provide shoggy-spell

(provide 'shoggy-spell)

;;; shoggy-spell.el ends here
