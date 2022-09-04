(require 'cl-macs)
(require 'svg)
(require 'filenotify)
(require 'seq)


;; Utilities.
(defconst cursorless-directory "~/.cursorless/")

(defvar cursorless-measure-time nil)
(defmacro measure-time (name &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     (prog1 (progn ,@body)
       (when cursorless-measure-time
        (message "%30s %3d ms" ',name (round (* 1000 (float-time (time-since time)))))))))

(defun line-and-column (pos)
  (list
   ;; I thought cursorless wanted 1-indexed line #s, but 0-indexed seems to make
   ;; it work properly?
   'line (1- (line-number-at-pos pos t))
  ;; (current-column) would be wrong here: we want # of characters since start
  ;; of line, not the logical position. (eg. tab counts as 1 char.)
   'column (save-excursion
             (goto-char pos)
             (- pos (line-beginning-position)))))

(defun goto-line-column (line column)
  (goto-char (point-min))
  (forward-line line)
  ;; TODO: check we haven't gone over the end of the line
  (forward-char column))

(defun line-and-column-to-offset (line column)
  (save-mark-and-excursion
    (save-restriction
      (widen)
      (goto-line-column line column)
      (point))))


;; Load everything.
(let ((load-path (cons (file-name-directory (or load-file-name (buffer-file-name)))
                       load-path)))
  (require 'cursorless-state)
  (require 'cursorless-hats)
  (require 'command-client))
(provide 'cursorless)
