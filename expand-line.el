;;; expand-line.el --- Expand selection by line

;; Copyright (C) 2015  Kai Yu

;; Author: Kai Yu <1@YeaMacky.local>
;; Keywords:

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

;; This package is similar to expand region,
;; but expand selection by line.

;;; Code:

(defvar expand-line-saved-position nil
  "The position before line expanding.")

(defun expand-line-save-point ()
  "Save current point."
  (setq expand-line-saved-position (point)))

(defun expand-line-restore-point ()
  "Restore point before expanding line."
  (goto-char expand-line-saved-position))

;;;###autoload
(defun expand-line-mark-line ()
  "Mark current line. After mark current line, use `expand-line' to expand."
  (interactive)
  (expand-line-save-point)
  (push-mark (point))
  (push-mark (line-beginning-position) nil t)
  (goto-char (line-end-position))
  (expand-line-mode 1)
  (add-hook 'deactivate-mark-hook 'turn-off-expand-line-mode))

(defun turn-off-expand-line-mode ()
  "Turn off `expand-line-mode'."
  (expand-line-mode -1)
  (remove-hook 'deactivate-mark-hook 'turn-off-expand-line-mode))

(defun expand-line-expand-previous-line ()
  "Expand to previous line."
  (interactive)
  (if (> (point) (mark))
      (exchange-point-and-mark))
  (move-beginning-of-line 0))

(defun expand-line-expand-next-line ()
  "Expand to next line."
  (interactive)
  (if (< (point) (mark))
      (exchange-point-and-mark))
  (move-end-of-line 2))

(defun expand-line-contract-next-line ()
  "Contract to next line."
  (interactive)
  (if (< (point) (mark))
      (exchange-point-and-mark))
  (move-end-of-line 0))

(defun expand-line-contract-previous-line ()
  "Contract to previous line."
  (interactive)
  (if (> (point) (mark))
      (exchange-point-and-mark))
  (move-beginning-of-line 2))

(defadvice keyboard-quit (before expand-line-restore-point activate)
  (if (memq last-command '(expand-line-mark-line
                           expand-line-expand-previous-line
                           expand-line-expand-next-line
                           expand-line-contract-previous-line
                           expand-line-contract-next-line))
      (progn
        (expand-line-restore-point)
        (expand-line-mode -1))))

(defun expand-line (arg)
  "Expand selection by line. If ARG is provided, select "
  (interactive "p")

  )

;; Keymap

(global-set-key (kbd "s-l") 'expand-line-mark-line)
(global-set-key (kbd "C-c l") 'expand-line-mark-line)

(defvar expand-line-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-p") 'expand-line-expand-previous-line)
    (define-key map (kbd "C-n") 'expand-line-expand-next-line)
    (define-key map (kbd "M-n") 'expand-line-contract-previous-line)
    (define-key map (kbd "M-p") 'expand-line-contract-next-line)
    map)
  "Keymap for Projectile mode.")

;; Mode

(define-minor-mode expand-line-mode
  "Mode for easy expand line when expand line is activated."
  :keymap expand-line-mode-map)

(provide 'expand-line)
;;; expand-line.el ends here
