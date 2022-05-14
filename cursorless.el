; a very simple test test test test test

;; what is hats? a list ((position color) (position color) ...)
;; TODO: interpret these colors using a customizable list
(defun update-overlays (hats)
  (remove-overlays nil nil 'cursorless t)
  (dolist (h hats)
    (cl-destructuring-bind (position color) h ; need cl-macs.el
      (let ((o (make-overlay position (+ 1 position) (current-buffer) t nil)))
        (overlay-put o 'cursorless t)
        (overlay-put o 'face `((:background ,color)))))))

(update-overlays '((1 "coral") (3 "lightblue")))
(update-overlays '())

(defvar serial-number 0)
(defvar cursorless-state '())

(defun line-and-column (pos)
  ;; Note that (current-column) is wrong, we want # of characters since start of
  ;; line, NOT the logical position. (eg. tab counts as 1 char).
  ;; cursorless line numbers are 1-indexed. not sure about column numbers.
  (vector (line-number-at-pos pos t)
          (save-excursion
            (goto-char pos)
            (- pos (line-beginning-position)))))

;; Serialize editor state to file, at the moment:
;; - a serial number
;; - current file path
;; - top & bottom visible lines
;;   edge case: long lines without wrapping, does cursorless hat them?
;;   looks like it does.
;; - where the cursors/selections are
(defun get-state ()
  ;; produces something that can be passed to json-serialize
  (list
   'serialNumber serial-number
   'bufferFileName (or (buffer-file-name) :null)
   ;; top & bottom visible lines
   'visibleLineRange (vector (line-number-at-pos (window-start))
                             (line-number-at-pos (- (window-end) 1)))
   ;; where the cursor is. cursorless wants line/column, not offset.
   'cursor (line-and-column (point))   ; point/cursor position
   ;; TODO: also, the mark if there's a selection (ie. if transient mark is on)
   ))

(defun dump-state (file)
  (interactive "F")
  (let ((state (get-state)))
   (with-temp-file file
     (json-insert state)
     (json-pretty-print-buffer))))

(defun cursorless-update ()
  ;; TODO: maybe figure out how to avoid dumping state if it didn't change?
  ;; but when will that happen?
  (setq cursorless-update-timer nil)
  (setq serial-number (+ 1 serial-number))
  (dump-state "~/cursorless/state"))

(defvar cursorless-update-timer nil)

(defun cursorless-update-callback ()
  (unless cursorless-update-timer
    (setq cursorless-update-timer
          (run-with-idle-timer 0 nil 'cursorless-update))))

(add-hook 'post-command-hook 'cursorless-update-callback)
(remove-hook 'post-command-hook 'cursorless-update-callback)
