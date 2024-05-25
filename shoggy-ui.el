;;; shoggy-ui.el --- UI for shoggy -*- lexical-binding: t -*-

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

;; UI for shoggy: graphical SVG board and shoggy-buffer setup


;;; Code:

(require 'button)
(require 'cl-lib)
(require 'svg)


;;;;; UI Board settings

(defvar shoggy-ui-board-svg nil)
(defvar shoggy-ui-square-size 64)
(defvar shoggy-ui-square-offset (/ shoggy-ui-square-size 2))


;;;;; Palette

(defvar shoggy-ui-square-color-dark     (concat "#876054" "ff"))
(defvar shoggy-ui-square-color-dark     (concat "#875b4e" "ff"))
(defvar shoggy-ui-square-color-light    (concat "#ccbcbc" "ff"))
(defvar shoggy-ui-square-color-legal    (concat "#73aebf" "50"))
(defvar shoggy-ui-square-color-selected (concat "#2bb567" "40"))
(defvar shoggy-ui-square-color-changed  (concat "#ab2bb5" "40"))
(defvar shoggy-ui-square-color-boosted  (concat "#9b3b25" "e0"))


;;;;; Board labels

(defvar shoggy-ui-square-labels '(a b c d e f g h i j k l)) ;; just in case
(defvar shoggy-ui-square-labels-font-size 12)


;;;; Board buffer settings

(eval-when-compile (defvar shoggy-spell-deck)
                   (defvar shoggy-board-ui-p)
                   (defvar shoggy-board-size))

;;;;; Mode line
(defun shoggy-ui-modeline-setup ()
  "Mode-line setup for `shoggy-board-buffer'."
  (with-current-buffer (shoggy-get-buffer)
    (unless (eq major-mode 'image-mode)
      (image-mode))
    ;; (set-window-margins (get-buffer-window shoggy-board-buffer) 10)
    (setq-local
     mode-line-format
     (list
      "  ♘  *shoggy-board*"
      "   "
      (propertize (format "[ Spells (%s) ]"
                          (length shoggy-spell-deck))
                  'face font-lock-constant-face
                  'mouse-face 'header-line-highlight
                  'help-echo "Available actions"
                  'local-map
                  '(keymap
                    (mode-line .(keymap
                                 (mouse-1 . shoggy-spell-deck)))))
      "  "
      ;; (propertize "[ Draw ]"
      ;;             'face font-lock-function-name-face
      ;;             'mouse-face 'header-line-highlight
      ;;             'help-echo "Offer a draw")
      "  "
      (propertize "[ Resign ]"
                  'face font-lock-function-name-face
                  'mouse-face 'header-line-highlight
                  'help-echo "Resign this match") ;; TODO: go to menu
      "  "
      (propertize "[ Restart ]"
                  'face font-lock-function-name-face
                  'mouse-face 'header-line-highlight
                  'help-echo "Restart game"
                  'local-map
                  '(keymap
                    (mode-line . (keymap
                                  (mouse-1 . shoggy-game-start)))))))))


;;;;; Header line

(defvar shoggy-ui-headerline-prefix "SHOGGY ➤ ")

(defun shoggy-ui-headerline-setup (&optional msg)
  "Header-line setup for `shoggy-board-buffer'. Show MSG in herder-line."
  (with-current-buffer (shoggy-get-buffer)
    (setq-local header-line-format (or msg "SHOGGY!"))))

(defun shoggy-ui-headerline-format (msg &optional type)
  "Format propertized MSG of TYPE and display in the header-line."
  (when shoggy-board-ui-p
    (shoggy-ui-headerline-setup
     (concat shoggy-ui-headerline-prefix
             (propertize msg
                         'face (cond ((eq type 'spell)
                                      'font-lock-constant-face)
                                     ((eq type 'turn)
                                      'font-lock-function-name-face)
                                     ((eq type 'end)
                                      'font-lock-warning-face)
                                     (t 'default)))))))


;;;; Make SVG board

;;;;; Make empty board

(defun shoggy-ui-board-make ()
  "Make an empty board SVG and store it in `shoggy-ui-board-svg'."
  (let* ((board-size (* shoggy-ui-square-size (1+ shoggy-board-size)))
         (square-size shoggy-ui-square-size)
         (offset shoggy-ui-square-offset)
         (labels (take shoggy-board-size shoggy-ui-square-labels)))
    (setf shoggy-ui-board-svg (svg-create board-size board-size))
    (shoggy-board-map
     ;; squares
     (svg-rectangle shoggy-ui-board-svg
                    (+ offset (* c square-size)) ;; x
                    (+ offset (* r square-size)) ;; y
                    square-size ;; width
                    square-size ;; height
                    :fill (if (cl-evenp (+ r c))
                              shoggy-ui-square-color-light
                            shoggy-ui-square-color-dark))
     ;; label numbers
     (when (= c (1- shoggy-board-size))
       (svg-text shoggy-ui-board-svg
                 (format "%s" (if (cl-oddp shoggy-board-flip-count)
                                  (1+ r)
                                (- shoggy-board-size r)))
                 :font-size shoggy-ui-square-labels-font-size
                 :text-anchor "left"
                 :x (+ offset (* c square-size) square-size -10)
                 :y (+ offset (* r square-size) 15)
                 :fill (if (cl-oddp (+ r c))
                           shoggy-ui-square-color-light
                         shoggy-ui-square-color-dark)))
     ;; label letters
     (when (= r (1- shoggy-board-size))
       (svg-text shoggy-ui-board-svg
                 (format "%s" (nth c (if (cl-oddp shoggy-board-flip-count)
                                         (reverse labels)
                                       labels)))
                 :font-size shoggy-ui-square-labels-font-size
                 :text-anchor "left"
                 :x (+ offset (* c square-size) 5)
                 :y (+ offset (* r square-size) square-size -5)
                 :fill (if (cl-oddp (+ r c))
                           shoggy-ui-square-color-light
                         shoggy-ui-square-color-dark))))))


;;;;; Set the pieces on the board

(defun shoggy-ui-board-set-pieces ()
  "Set the pieces on the graphical board according to the current position."
  (let ((path (expand-file-name "img/")) ; FIXME 2024-05-25: img path
        (square-size shoggy-ui-square-size)
        (offset shoggy-ui-square-offset))
    (shoggy-board-map
     (when-let (piece (shoggy-board-get (cons r c)))
       ;; NOTE 2024-05-19: keep pieces' positions updated
       (setf (shoggy-piece-position piece) (cons r c))
       (svg-embed
        shoggy-ui-board-svg
        (let ((atom (shoggy-piece-atom piece))
              (color (shoggy-piece-color piece)))
          (cond ((and (eq atom 'p) (equal color "white"))
                 (concat path "pawn.png"))
                ((and (eq atom 'p) (equal color "black"))
                 (concat path "pawn1.png"))
                ((and (eq atom 'f) (equal color "white"))
                 (concat path "bishop.png"))
                ((and (eq atom 'f) (equal color "black"))
                 (concat path "bishop1.png"))
                ((and (eq atom 'w) (equal color "white"))
                 (concat path "queen.png"))
                ((and (eq atom 'w) (equal color "black"))
                 (concat path "queen1.png"))
                ((and (eq atom 'n) (equal color "white"))
                 (concat path "knight.png"))
                ((and (eq atom 'n) (equal color "black"))
                 (concat path "knight1.png"))
                ((and (eq atom 's) (equal color "white"))
                 (concat path "king.png"))
                ((and (eq atom 's) (equal color "black"))
                 (concat path "king1.png"))
                ((and (eq atom 'c) (equal color "white"))
                 (concat path "rook.png"))
                ((and (eq atom 'c) (equal color "black"))
                 (concat path "rook1.png"))))
        "image/png"
        nil
        :x (+ shoggy-ui-square-offset (* c shoggy-ui-square-size) 8)
        :y (- (* (1+ r) shoggy-ui-square-size) 25)
        :width 48
        :height 48)
       (when (eq (shoggy-piece-boosted piece) t)
         (svg-circle shoggy-ui-board-svg
                     (+ offset (* c square-size) 15)
                     (+ offset (* r square-size) square-size -15)
                     10
                     :fill shoggy-ui-square-color-boosted))))))


;;;;; Set square properties

(defun shoggy-ui-board-square-props (square keymap &optional action-fn)
  "Return SQUARE with KEYMAP.
Default action is `shoggy-ui-board-selected-square'.
With optional argument ACTION-FN, use it instead."
  (let* ((size shoggy-ui-square-size)
         ;; NOTE: col is X, row is Y
         (x0 (+ shoggy-ui-square-offset (* (cdr square) size)))
         (y0 (+ shoggy-ui-square-offset (* (car square) size)))
         (x1 (+ x0 size))
         (y1 (+ y0 size))
         (area-string (format "r%sc%s" (car square) (cdr square)))
         (area (intern area-string)))
    (prog1
        `((rect . ((,x0 . ,y0) . (,x1 . ,y1)))
          ,area
          (pointer hand)) ;; help-echo ,area-string))
      (define-key keymap `[,area mouse-1]
                  (lambda ()
                    (interactive)
                    (funcall (or action-fn
                                 #'shoggy-ui-board-selected-square)
                             square))))))


;;;;; Update board's properties

(defvar shoggy-ui-board--keymap nil
  "Keymap for the graphical board.")

(defvar shoggy-ui-board--square-map nil
  "Map of current selectable pieces in graphical board.")

(defmacro shoggy-ui-board-update (&rest body)
  "Run BODY in `shoggy-board-buffer' and update board properties."
  (declare (indent defun))
  `(with-current-buffer (shoggy-get-buffer)
     (setq shoggy-ui-board--keymap (get-text-property (point-min) 'keymap))
     ,@body
     (set-text-properties
      (point-min) (point-max)
      (list 'display (svg-image shoggy-ui-board-svg
                                :map shoggy-ui-board--square-map)
            'keymap shoggy-ui-board--keymap))))


;;;;; Highlight square

(defun shoggy-ui-board-highlight-square (square &optional color)
  "Highlight current SQUARE with COLOR."
  (let ((size shoggy-ui-square-size)
        (offset shoggy-ui-square-offset))
    (svg-rectangle shoggy-ui-board-svg
                   ;; NOTE: col is X, row is Y
                   (+ offset (* (cdr square) size))
                   (+ offset (* (car square) size))
                   size size
                   :fill (or color shoggy-ui-square-color-legal))))


;;;;; Redraw board

(defun shoggy-ui-board-redraw (&optional highlight-squares color)
  "Redraw graphical board and update its squares' properties.
Optional argument HIGHLIGHT-SQUARES is a list of squares that will be
highlighted with COLOR *before* setting up the pieces."
  (shoggy-ui-board-make)
  (when highlight-squares
    (shoggy-ui-board-update
      (mapc (lambda (square)
              (shoggy-ui-board-highlight-square
               square (or color shoggy-ui-square-color-selected)))
            highlight-squares)))
  (shoggy-ui-board-set-pieces)
  (shoggy-ui-board-update
    (erase-buffer)
    (insert "SHOGGY")
    (goto-char (point-min)) ;; HACK: point over image
    (setq shoggy-ui-board--keymap (make-sparse-keymap))
    (setq shoggy-ui-board--square-map
          (shoggy-board-map-flatten
            (shoggy-board-map
             (when (shoggy-piece-own-p (shoggy-board-get (cons r c)))
               (shoggy-ui-board-square-props
                (cons r c) shoggy-ui-board--keymap)))))))


;;;;; Highlight legal moves

(defun shoggy-ui-board-highlight-legal-moves (square)
  "Highlight all current legal moves for the selected SQUARE."
  (when-let* ((piece (shoggy-board-get square))
              (legal-moves (shoggy-legal-moves piece)))

    (mapc #'shoggy-ui-board-highlight-square legal-moves)

    ;; add legal moves to keymap
    (shoggy-ui-board-update
      (setq shoggy-ui-board--keymap (get-text-property (point-min) 'keymap))
      (mapc (lambda (square)
              (add-to-list 'shoggy-ui-board--square-map
                           (shoggy-ui-board-square-props
                            square shoggy-ui-board--keymap)))
            legal-moves))))


;;;; Selected square action

(defvar shoggy-ui-board--selected-piece nil)

;; TODO 2024-05-19: jack-in spell card mechanics
(defvar shoggy-ui-board-before-move-hook nil)

;; TODO 2024-05-19: add `shoggy-ui-sound-piece-move'
;; for the engine move to make a sound, this hook should be elsewhere
;; or we need a specific `shoggy-ui-board-after-user-move-hook'
(defvar shoggy-ui-board-after-move-hook nil)

(defun shoggy-ui-board-selected-square (square)
  "Action of mouse event on selected SQUARE."
  (let ((piece (shoggy-board-get square)))
    (setq shoggy-board-ui-p t)
    (if (or (not piece)
            (shoggy-piece-enemy-p piece shoggy-ui-board--selected-piece))
        (let ((from-square (shoggy-piece-position
                            shoggy-ui-board--selected-piece)))
          (run-hooks 'shoggy-ui-board-before-move-hook)
          (shoggy-board-move from-square square)
          (setq shoggy-ui-board--selected-piece nil)
          (when shoggy-ui-board-after-move-hook
            (shoggy-ui-board-redraw (list from-square square))
            (run-hooks 'shoggy-ui-board-after-move-hook)))
      (setq shoggy-ui-board--selected-piece piece)
      (shoggy-ui-board-redraw (list square) shoggy-ui-square-color-selected)
      (shoggy-ui-board-highlight-legal-moves square)
      (shoggy-ui-board-set-pieces)
      (shoggy-ui-board-update))))

;;;; Buttons for prompts

(defmacro shoggy-ui-prompt-buttons (prompt action-list button-fn)
  "Make buttons for PROMPT from ACTION-LIST. BUTTON-FN is the button action."
  (declare (indent defun))
  `(with-current-buffer (shoggy-get-buffer)
     (goto-char (point-max))
     (insert (format "\n   %s: " ,prompt))
     (mapc (lambda (action)
             (let ((button (concat "[ " (car action) " ]"))
                   (button-type (intern (format
                                         "shoggy-ui--button-%s"
                                         (downcase (car action))))))
               (define-button-type button-type
                 'action ,button-fn
                 'follow-link t)
               (goto-char (point-max))
               (insert button)
               (re-search-backward
                (concat "\\(" (regexp-quote button) "\\)") nil t)

               (make-button (match-beginning 0)
                            (match-end 0)
                            :type button-type)
               (goto-char (point-max))
               (insert " ")))
           ,action-list)
     (goto-char (line-beginning-position))))


;;;;; Promotion UI

(defvar shoggy-ui--square nil)
(defvar shoggy-ui--after-move-hook nil)

(defun shoggy-ui-promotion-prompt (square)
  "Return the atom of the piece to promote to at SQUARE."
  (shoggy-ui-board-redraw)
  (shoggy-ui-board-update
    (shoggy-ui-board-highlight-square square
                                      shoggy-ui-square-color-changed))

  ;; HACK 2024-05-25: so the button can know the target square
  ;; also, stop the engine loop until decision
  (setq shoggy-ui--square square)
  (setq shoggy-ui--after-move-hook shoggy-ui-board-after-move-hook)
  (setq shoggy-ui-board-after-move-hook nil)

  (shoggy-ui-prompt-buttons
    "Promote to"
    '(("Ferz"    . f)
      ("Wazir"   . w)
      ("Knight"  . n)
      ("Chariot" . c))
    (lambda (&rest _)
      (shoggy-board-put-new (cdr action)
                            shoggy-player-color
                            shoggy-ui--square)
      (shoggy-ui-board-redraw)
      (setq shoggy-ui-board-after-move-hook
            shoggy-ui--after-move-hook)
      (run-hooks 'shoggy-ui-board-after-move-hook)) ))


;;;;; Spell cards UI




;;; Provide shoggy-ui

(provide 'shoggy-ui)

;;; shoggy-ui.el ends here
