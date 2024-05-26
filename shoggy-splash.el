;;; shoggy-splash.el --- Splash screen for shoggy -*- lexical-binding: t -*-

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

;; Splash screen for shoggy.


;;; Code:

(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defun shoggy-splash ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*shoggy*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (setq-local animate-n-steps 10)
  (animate-string "
                 __
           _____/ /_  ____  ____ _____ ___  __
          / ___/ __ \\/ __ \\/ __ `/ __ `/ / / /
         (__  ) / / / /_/ / /_/ / /_/ / /_/ /
        /____/_/ /_/\\____/\\__, /\\__, /\\__, /
                         /____//____//____/
" 0)
  (shoggy-ui-sound-play 'splash)
  (sit-for 0.3)
  (goto-char (point-max))
  (widget-insert "\nWelcome to shoggy!\n\n")
  (widget-insert "Player color?\n")
  (widget-create 'radio-button-choice
                 :value (format "%s" (capitalize shoggy-user-color))
                 :notify (lambda (widget &rest ignore)
                           (let ((color (widget-value widget)))
                             (setq shoggy-user-color
                                   (cond ((equal color "White")
                                          "white")
                                         ((equal color "Black")
                                          "black")
                                         ((equal color "Random")
                                          (car (shoggy-shuffle '("white"
                                                                 "black")
                                                               1)))))))
                 '(item "White") '(item "Black") '(item "Random"))
  (widget-insert "\nShould pawns move two squares?\n")
  (widget-create 'radio-button-choice
                 :value (format "%s"
                                (if shoggy-board-pawn-moves-two-squares-p
                                    "Yes" "No"))
                 :notify (lambda (widget &rest ignore)
                           (setopt shoggy-board-pawn-moves-two-squares-p
                                   (equal (widget-value widget) "Yes")))
                 '(item "Yes") '(item "No"))
  (widget-insert "\nRandomize home row?\n")
  (widget-create 'radio-button-choice
                 :value (format "%s"
                                (if shoggy-board-homerow-random-p
                                    "Yes" "No"))
                 :notify (lambda (widget &rest ignore)
                           (setopt shoggy-board-homerow-random-p
                                   (equal (widget-value widget) "Yes")))
                 '(item "Yes") '(item "No"))
  (widget-insert "\nEngine?\n")
  (widget-create 'radio-button-choice
                 :value (format "%s"
                                (if (eq shoggy-engine 'sanefish)
                                    "Sanefish" "Dumbfish"))
                 :notify (lambda (widget &rest ignore)
                           (setopt shoggy-engine
                                   (if (equal (widget-value widget)
                                              "Sanefish")
                                       'sanefish
                                     'dumbfish)))
                 '(item "Sanefish") '(item "Dumbfish"))
  (widget-insert "\nPlay sounds?\n")
  (widget-create 'radio-button-choice
                 :value (format "%s" (if shoggy-sound-enabled "Yes" "No"))
                 :notify (lambda (widget &rest ignore)
                           (setopt shoggy-sound-enabled
                                   (equal (widget-value widget) "Yes")))
                 '(item "Yes") '(item "No"))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (shoggy-game-start))
                 " Play! ")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (line-beginning-position)))


;;; Provide shoggy-splash

(provide 'shoggy-splash)

;;; shoggy-splash.el ends here
