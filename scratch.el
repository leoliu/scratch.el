;;; scratch.el --- use scratch buffers easily        -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.0
;; Keywords: convenience
;; Created: 2013-10-01

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

;; M-x scratch - handy scratch buffer at your finger tip.

;;; Code:

(defgroup scratch nil
  "Use scratch buffers easily."
  :group 'editing
  :group 'help)

(defcustom scratch-base-buffer-name "*Scratch*"
  "The base scratch buffer name."
  :type 'string
  :group 'scratch)

(defvar scratch-stack nil "Internal variable.")

;; This has precedence higher than other major or minor mode maps so
;; keep the key bindings to the minimum.
(defvar scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") 'scratch-change-buffer-or-mode)
    (define-key map (kbd "C-c C-k") 'scratch-kill-buffer)
    (define-key map (kbd "C-c C-o") 'scratch-erase-buffer)
    map))

(defun scratch-kill-buffer (&optional no-confirm)
  "Kill current buffer."
  (interactive "P")
  (when (or no-confirm
            (not (buffer-modified-p))
            (kill-buffer-ask (current-buffer)))
    (quit-window t)))

;; Since command `erase-buffer' is disabled by default.
(defun scratch-erase-buffer ()
  (interactive "*")
  (erase-buffer))

(defvar scratch-read-major-mode-history nil)

(defun scratch-read-major-mode ()
  "Choose a major mode with completion."
  (let ((modes
         (let (mm mmx)
           (mapatoms
            (lambda (m)
              (when (and (commandp m)
                         (string-match-p "-mode\\'" (symbol-name m))
                         ;; Some minor modes don't use `define-minor-mode'.
                         (not (string-match-p "-minor-mode\\'" (symbol-name m)))
                         (not (memq m minor-mode-list))
                         (not (member (symbol-name m) scratch-read-major-mode-history)))
                (push (symbol-name m)
                      ;; Modes by define-derived-mode
                      (if (plist-member (symbol-plist m) 'derived-mode-parent)
                          mm
                        mmx)))))
           (append scratch-read-major-mode-history mm mmx))))
    (intern-soft (let ((history-delete-duplicates t))
                   (ido-completing-read "Major mode: " modes nil nil nil
                                        'scratch-read-major-mode-history)))))

(defun scratch-buffer-names (&optional exclude-new)
  ;; Prune dead buffers.
  (setq scratch-stack
        (delq nil (mapcar (lambda (b) (and (buffer-live-p b) b))
                          scratch-stack)))
  (let ((bufs (mapcar #'buffer-name scratch-stack)))
    (if exclude-new
        bufs
      (cons (propertize (generate-new-buffer-name scratch-base-buffer-name)
                        'face 'error)
            bufs))))

(defun scratch-change-buffer-or-mode (&optional what)
  (interactive
   (list (if current-prefix-arg
             (ido-completing-read "Buffer: " (scratch-buffer-names) nil t)
           (scratch-read-major-mode))))
  (cond
   ((or (stringp what) (bufferp what))
    (let ((new (not (get-buffer what))))
      (switch-to-buffer what)
      (setq scratch-stack (cons (current-buffer)
                                (delq (current-buffer) scratch-stack)))
      (and new (scratch-change-buffer-or-mode))))
   (t (with-demoted-errors
        (funcall (or what
                     (intern-soft (car scratch-read-major-mode-history))
                     'text-mode)))))
  (scratch-mode 1))

(defun scratch-kill-buffer-hook ()
  (setq scratch-stack (delq (current-buffer) scratch-stack)))

(put 'scratch-mode 'permanent-local t)
(define-minor-mode scratch-mode nil :lighter " draft"
  (if scratch-mode
      (progn
        (add-hook 'kill-buffer-hook 'scratch-kill-buffer-hook nil t)
        ;; Make scratch-mode-map the highest of all minor mode maps.
        (add-to-list 'minor-mode-overriding-map-alist
                     `(scratch-mode . ,scratch-mode-map)))
    (remove-hook 'kill-buffer-hook 'scratch-kill-buffer-hook t)
    (assq-delete-all 'scratch-mode minor-mode-overriding-map-alist)))

;;;###autoload
(defun scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (scratch-change-buffer-or-mode
   (or (car (scratch-buffer-names t)) scratch-base-buffer-name)))

(defun scratch-change-major-mode-hook ()
  (when scratch-mode
    (scratch-mode 1)
    (let ((history-delete-duplicates t))
      (add-to-history 'scratch-read-major-mode-history
                      (symbol-name major-mode)))))

;; Change major mode in any other way should just work.
(add-hook 'after-change-major-mode-hook 'scratch-change-major-mode-hook)

(provide 'scratch)
;;; scratch.el ends here
