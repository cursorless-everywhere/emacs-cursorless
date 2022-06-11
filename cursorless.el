; a very simple test test test test test
(require 'cl-macs)

(defvar serial-number 0)
(defvar cursorless-state '())

(defun line-and-column (pos)
  ;; Note that (current-column) is wrong, we want # of characters since start of
  ;; line, NOT the logical position. (eg. tab counts as 1 char).
  ;; cursorless line numbers are 1-indexed. not sure about column numbers.
  (list
   'line (line-number-at-pos pos t)
   'column (save-excursion
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
  ;; in this case, a plist
  (list
   :serialNumber serial-number
   :activeEditor
   (list
    :path (or (buffer-file-name) :null)
    :firstVisibleLine (line-number-at-pos (window-start))
    :lastVisibleLine  (line-number-at-pos (- (window-end) 1))
    ;; where the cursors are. in emacs, only one cursor, so a singleton vector.
    ;; note that cursorless wants line/column, not offset.
    ;; TODO: if transient-mark-mode is enabled, represent the whole selection.
    :cursors (vector (line-and-column (point))))
   ))


;; DUMPING OUR STATE TO CURSORLESS ;;
(defun dump-state (file)
  (interactive "F")
  (let ((state (get-state)))
   (with-temp-file file
     (json-insert state)
     (json-pretty-print-buffer) ;; optional, for human consumption
     )))

(defun cursorless-send-state ()
  ;; TODO: maybe figure out how to avoid dumping state if it didn't change?
  ;; but when will that happen?
  (setq cursorless-send-state-timer nil)
  (setq serial-number (+ 1 serial-number))
  (dump-state "~/cursorless/state"))

(defvar cursorless-send-state-timer nil)

;;; Scrolling seems janky, but it doesn't look like we're causing it?
;;; that is, removing this hook doesn't seem to fix the issue.
(defun cursorless-send-state-callback ()
  (unless cursorless-send-state-timer
    (setq cursorless-send-state-timer
          (run-with-idle-timer 0.1 nil 'cursorless-send-state))))

(add-hook 'post-command-hook 'cursorless-send-state-callback)
;(remove-hook 'post-command-hook 'cursorless-send-state-callback)


;; DRAWING & READING HATS FROM CURSORLESS ;;
(defun clear-overlays ()
  (remove-overlays nil nil 'cursorless t))

;; TODO: defcustom
(defparam cursorless-color-alist
  '((default . "lightgrey")
    (blue . "lightblue")
    (red . "pink")
    (pink . "light goldenrod")
    (green . "light green")
    ))

;; what is hats? a list ((color line offset) (color line offset) ...)
(defun update-overlays (hats)
  (clear-overlays)
  (dolist (h hats)
    (cl-destructuring-bind (color line offset) h
      (let* ((position (line-and-column-to-offset line offset))
             (o (make-overlay position (+ 1 position) (current-buffer) t nil)))
        (overlay-put o 'cursorless t)
        (setq color (or (cdr (assoc color cursorless-color-alist)) (symbol-name color)))

        ;; TODO: if we have multiple working strategies (eg foreground+underline
        ;; vs background-color), we can use these as "shapes/styles"

        ;; change background color
        (overlay-put o 'face `((:background ,color)))

        ;; change foreground color & underline
        ;(overlay-put o 'face `((:foreground ,color) (:underline ,color)))

        ;(overlay-put o 'face `((:box (:line-width 2 :color ,color))))
        ;(overlay-put o 'face `((:overline ,color)))
        ))))

(defun line-and-column-to-offset (line column)
  (save-mark-and-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; It looks like we're NOT off-by-one, which confuses me a bit.
      (forward-line line)
      ;; TODO: check we haven't gone over the end of the line
      (forward-char column)
      (point))))

;(update-overlays '((1 "coral") (3 "lightblue")))
;(update-overlays '())

(defun read-hats-raw (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (json-parse-buffer :object-type 'alist)))

(defun read-hats (file)
  (let* ((hats (read-hats-raw file))
         (file-hats (cdar hats)))
    ;; hats-json is an alist of the form ((file . file-hats) ...)
    ;; file-hats are of the form: ((color . [hat ...]) ...)
    ;; hats are of the form: ((start (line . n) (character . n)) (end (line . n) (character . c)))
    ;;
    ;; for now we assume there's only one file, so we just grab the cdar
    ;; TODO: this ought to be much simpler.
    (cl-loop for (color . hats) in file-hats
             nconc (cl-loop for hat across hats
                            collect (let ((x (alist-get 'start hat)))
                                      (list color (alist-get 'line x) (alist-get 'character x)))))))

(defun hide-hats ()
  (interactive)
  (with-current-buffer hats-buffer
   (update-overlays '()))
  (setq hats-buffer nil))

(defun show-hats ()
  (interactive)
  (setq hats-buffer (current-buffer))
  (update-overlays (read-hats "~/.vscode-hats.json")))

(defvar hats-buffer nil)

(defun hats-change-callback (event)
  (when hats-buffer
    (with-current-buffer hats-buffer
      (update-overlays (read-hats "~/.vscode-hats.json"))
      (message "Updated hats in %s" hats-buffer))))

(defvar vscode-hats-watcher
  (file-notify-add-watch "~/.vscode-hats.json" '(change) 'hats-change-callback))
