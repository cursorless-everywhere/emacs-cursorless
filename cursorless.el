;;; cursorless.el --- Voice based structural editing with Cursorless -*- lexical-binding: t; -*-
;;
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (command-server "0.0.1"))
;; Keywords: cursorless, voice
;; Homepage: https://github.com/cursorless-everywhere/emacs-cursorless
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'seq)
(require 'svg)
(require 'filenotify)

(defconst cursorless-directory "~/.cursorless/")

(defun cursorless-line-and-column (pos)
  (list
   ;; I thought cursorless wanted 1-indexed line #s, but 0-indexed seems to make
   ;; it work properly?
   'line (1- (line-number-at-pos pos t))
   ;; (current-column) would be wrong here: we want # of characters since start
   ;; of line, not the logical position. (eg. tab counts as 1 char.)
   'column (save-excursion
             (goto-char pos)
             (- pos (line-beginning-position)))))

(defun cursorless-goto-line-column (line column)
  (goto-char (point-min))
  (forward-line line)
  ;; TODO: check we haven't gone over the end of the line
  (forward-char column))

(defun line-and-column-to-offset (line column)
  (save-mark-and-excursion
    (save-restriction
      (widen)
      (cursorless-goto-line-column line column)
      (point))))

;; Load everything.
(let ((load-path (cons (file-name-directory (or load-file-name (buffer-file-name)))
                       load-path)))
  (require 'cursorless-log)
  (require 'cursorless-command-client)
  (require 'cursorless-state)
  (require 'cursorless-hats))

(provide 'cursorless)
;;; cursorless.el ends here
