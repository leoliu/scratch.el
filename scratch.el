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
;;
;; - C-c C-j     => Change major mode or buffer (with C-u)
;; - C-c C-k     => Kill buffer (no confirmation if with C-u)
;; - C-c C-o     => Erase buffer
;;
;; NOTE: variable `scratch-major-mode-history' keeps a history of
;; major modes used in scratch buffers, which are placed at the front
;; when calling `scratch-change-buffer-or-mode'. It might be helpful
;; to have (savehist-mode 1) to preserve it across emacs sessions.

;;; Code:

(defgroup scratch nil
  "Use scratch buffers easily."
  :group 'editing
  :group 'help)

(defcustom scratch-buffer-name "*Scratch*"
  "The scratch buffer name."
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
            ;; Same as `kill-buffer-ask'.
            (yes-or-no-p (format "Buffer %s HAS BEEN EDITED.  Kill? "
                                 (buffer-name (current-buffer)))))
    (quit-window t)))

;; Since command `erase-buffer' is disabled by default.
(defun scratch-erase-buffer ()
  (interactive "*")
  (erase-buffer))

(defvar scratch-major-mode-history nil
  "History list for major modes in scratch buffers.

Maximum length of the history list is determined by the value of
`history-length', which see.")

(defun scratch-read-major-mode ()
  "Choose a major mode with completion."
  (let ((modes
         (let ((pred (lambda (m1 m2) (< (length m1) (length m2))))
               mm mmx)
           (mapatoms
            (lambda (m)
              (when (and (commandp m)
                         (string-match-p "-mode\\'" (symbol-name m))
                         ;; Some minor modes don't use `define-minor-mode'.
                         (not (string-match-p "-minor-mode\\'" (symbol-name m)))
                         (not (memq m minor-mode-list))
                         (not (member (symbol-name m) scratch-major-mode-history)))
                (push (symbol-name m)
                      ;; Modes by define-derived-mode
                      (if (plist-member (symbol-plist m) 'derived-mode-parent)
                          mm
                        mmx)))))
           (append scratch-major-mode-history
                   (sort mm pred) (sort mmx pred)))))
    (intern-soft (let ((history-delete-duplicates t))
                   (ido-completing-read "Major mode: " modes nil nil nil
                                        'scratch-major-mode-history)))))

(defun scratch-buffer-names (&optional exclude-new)
  ;; Prune dead buffers.
  (setq scratch-stack
        (delq nil (mapcar (lambda (b) (and (buffer-live-p b) b))
                          scratch-stack)))
  (let ((bufs (mapcar #'buffer-name scratch-stack)))
    (if exclude-new
        bufs
      (cons (propertize (generate-new-buffer-name scratch-buffer-name)
                        'face 'error)
            bufs))))

(defun scratch-change-buffer-or-mode (&optional what)
  "Change buffer or major mode according to WHAT.
When called interactively with prefix change buffer, otherwise
change major mode."
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
                     (intern-soft (car scratch-major-mode-history))
                     'text-mode)))))
  (scratch-mode 1))

(defun scratch-kill-buffer-hook ()
  (setq scratch-stack (delq (current-buffer) scratch-stack)))

(put 'scratch-mode 'permanent-local t)

;;;###autoload
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
  "Switch to *Scratch* buffer."
  (interactive)
  (scratch-change-buffer-or-mode
   (or (car (scratch-buffer-names t)) scratch-buffer-name)))

(defun scratch-change-major-mode-hook ()
  (when scratch-mode
    (scratch-mode 1)
    (let ((history-delete-duplicates t))
      (add-to-history 'scratch-major-mode-history (symbol-name major-mode)))))

;; Change major mode in any other way should just work.
(add-hook 'after-change-major-mode-hook 'scratch-change-major-mode-hook)

(provide 'scratch)
;;; scratch.el ends here
