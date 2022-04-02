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

;; TODO: serialize editor state to file, at the moment:
;; - a serial number
;; - current file path
;; - top & bottom visible lines
;;   edge case: long lines without wrapping, does cursorless hat them?
;;   looks like it does.
;; - where the cursors/selections are
(defun get-state ()
  (list
   serial-number
   (buffer-file-name)
   ;; top & bottom visible lines
   (line-number-at-pos (window-start))
   (line-number-at-pos (- (window-end) 1))
   ;; TODO: where the cursor is. cursorless wants line/column, not offset.
   ;; also, if there's a selection (ie. if transient mark is on)
   (line-and-column (point)) ; point/cursor position
   ;; TODO: selection (mark) position
   ))

(defun line-and-column (pos)
  ;; (current-column) is WRONG, we want # of characters since start of line, NOT
  ;; the logical position. (eg. tab counts as 1 char).

  ;; cursorless line numbers are 1-indexed. not sure about column numbers.
  (list (line-number-at-pos pos t)
        (save-excursion
          (goto-char pos)
          (- pos (line-beginning-position)))))

(defun lac ()
  (interactive)
  (message "%s" (line-and-column (point))))
