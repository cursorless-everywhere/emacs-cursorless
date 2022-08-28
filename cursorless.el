;; This file:
;; (1) displays/updates hats based on a hats file written by vscode.
;; (2) writes out emacs' state to files watched by vscode.
;;
;; It does not handle:
;; (3) the cursorless command path, talon <-> emacs <-> vscode.
;;     for that, see cursorless-socket.el and emacs_command_client.py
;;
;; Eventually I should either split (1), (2), (3) into three files
;; or unify them into one.
(require 'cl-macs)
(require 'svg)
(require 'json) ;; used for json-pretty-print-buffer, which could be removed
(require 'filenotify)

(defvar cursorless-measure-time t)
(defmacro measure-time (name &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     (prog1 (progn ,@body)
       (when cursorless-measure-time
        (message "%30s %3d ms" ',name (round (* 1000 (float-time (time-since time)))))))))

(defun line-and-column (pos)
  ;; Note that (current-column) is wrong, we want # of characters since start of
  ;; line, NOT the logical position. (eg. tab counts as 1 char).
  ;; cursorless line numbers are 1-indexed. not sure about column numbers.
  (list
   'line (1- (line-number-at-pos pos t))
   'column (save-excursion
             (goto-char pos)
             (- pos (line-beginning-position)))))

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

(defvar cursorless-serial-number 0)

(defconst cursorless-directory "~/.cursorless/")
(defconst cursorless-editor-state-file
  (concat cursorless-directory "editor-state.json"))
(defconst cursorless-hats-file
  (concat cursorless-directory "vscode-hats.json"))


;; DUMPING OUR STATE TO CURSORLESS ;;
(make-variable-buffer-local 'cursorless-temporary-file)
;; permanent-local --> survives major mode change
(put 'cursorless-temporary-file 'permanent-local t)
;; TODO: remove temporary files when the buffer is closed
;; need buffer-list-update-hook

(defvar cursorless-enabled t)
(defvar cursorless-send-state-timer (timer-create))
(timer-set-function cursorless-send-state-timer 'cursorless-send-state)
;; idle time 1ms, gotta go fast. don't repeat automatically.
(timer-set-idle-time cursorless-send-state-timer 0 nil)

(add-hook 'post-command-hook 'cursorless-send-state-callback)
;(remove-hook 'post-command-hook 'cursorless-send-state-callback)

(defun cursorless-enable ()
  (interactive)
  (setq cursorless-enabled t)
  (cursorless-send-state))

(defun cursorless-disable ()
  (interactive)
  (setq cursorless-enabled nil)
  ;; TODO: file-notify-rm-watch cursorless-hats-watcher?
  (cancel-timer cursorless-send-state-timer))

;;; Scrolling seems janky, but it doesn't look like we're causing it?
;;; that is, removing this hook doesn't seem to fix the issue.
(defun cursorless-send-state-callback ()
  (when cursorless-enabled
    (if cursorless-send-state-timer
        (timer-activate-when-idle cursorless-send-state-timer t)
      (error "no idle timer"))))

(defun cursorless-send-state ()
  ;; TODO: maybe figure out how to avoid dumping state if it didn't change?
  ;; but when will that happen?
  (unless (minibufferp) ;; don't do anything when in minibuffer
    (progn ; measure-time "cursorless send state"
      (setq cursorless-serial-number (+ 1 cursorless-serial-number))
      (cursorless-dump-state))))

(defun cursorless-dump-state ()
  (interactive)
  ;; TODO: only write if buffer contents have changed since last write!
  ;; TODO: check if file is too damn big.
  (let ((file-name-handler-alist '())) ;; avoid compression etc.
    (write-region nil nil (cursorless-temporary-file-path) nil 'ignore-message))
  (let ((state (get-state))
        (buffer (current-buffer)))
    (with-temp-file cursorless-editor-state-file
      (json-insert state)
      ;(json-pretty-print-buffer) ;; for human consumption (optional)
      )))

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
   :serialNumber cursorless-serial-number
   :activeEditor
   (list
    :path (or (buffer-file-name) :null)
    :temporaryFilePath (cursorless-temporary-file-path)
    :firstVisibleLine (line-number-at-pos (window-start))
    :lastVisibleLine  (line-number-at-pos (- (window-end) 1))
    ;; where the cursors are. in emacs, only one cursor, so a singleton vector.
    ;; note that cursorless wants line/column, not offset.
    ;; TODO: if transient-mark-mode is enabled, represent the whole selection.
    :cursors (vector (line-and-column (point))))
   ))

(defun cursorless-temporary-file-path ()
  (if (and (local-variable-p 'cursorless-temporary-file)
           ;; if file has been deleted we probably want to make a new one.
           (file-exists-p cursorless-temporary-file))
      cursorless-temporary-file
    (let* ((extension (if (null (buffer-file-name)) ""
                        (concat "." (file-name-extension (buffer-file-name)))))
           (dirname (concat (file-name-as-directory temporary-file-directory)
                            "cursorless.el/"))
           (name (replace-regexp-in-string "[*/\\\\]" "_" (buffer-name)))
           (prefix (concat dirname name "-")))
      (make-directory dirname t)
      ;; make-temp-file-internal because it doesn't try to do magic with file names
      (setq cursorless-temporary-file
            (make-temp-file-internal prefix nil extension nil)))))


;; READING & DRAWING HATS FROM CURSORLESS ;;
(defvar hats-buffer nil)

(defvar cursorless-updating-hats nil)
(defvar cursorless-hats-update-timer nil)
(defun cursorless-hats-update-callback (&optional event)
  ;; recursive invocations can occur if running cursorless-update-hats causes
  ;; the hats file to change; eg. if cursorless-update-hats writes to *Messages*
  ;; and we're visiting *Messages*. Without care this can lock up emacs.
  (if cursorless-updating-hats
      (warn "cursorless-hats-update-callback: recursive invocation detected!")
    (unwind-protect
     (progn
       (setq cursorless-updating-hats t)
       (setq cursorless-hats-update-timer nil)
       (when hats-buffer (with-current-buffer hats-buffer (cursorless-update-hats))))
     (setq cursorless-updating-hats nil))))
(defun cursorless-hats-change-callback (&optional event)
  (if cursorless-updating-hats
      (message "cursorless-hats CHANGE RECURSIVE, set cursorless-updating-hats to nil to re-enable")
    (ignore))
  (unless (or cursorless-updating-hats cursorless-hats-update-timer)
    (setq cursorless-hats-update-timer
          (run-with-idle-timer 0 nil 'cursorless-hats-update-callback))))

(defvar cursorless-hats-watcher
  (progn
    (when (and (boundp 'cursorless-hats-watcher) cursorless-hats-watcher)
      (file-notify-rm-watch cursorless-hats-watcher))
    (file-notify-add-watch cursorless-hats-file '(change) 'cursorless-hats-change-callback)))

;; TODO: defcustom
(defconst cursorless-color-alist
  '((default . "#999") (blue . "#04f") (red . "dark red") (pink . "coral") (green . "#0b0")))
;; (defconst cursorless-color-alist ; dark theme
;;   '((default . "#999") (blue . "#0af") (red . "#f00") (pink . "#fa8072") (green . "#0a0")))

(defun show-hats ()
  (interactive)
  (when hats-buffer (with-current-buffer hats-buffer (cursorless-clear-overlays)))
  (setq hats-buffer (current-buffer))
  (cursorless-initialize-hats)
  (cursorless-hats-update-callback))

(defun hide-hats ()
  (interactive)
  (when hats-buffer (with-current-buffer hats-buffer (cursorless-clear-overlays)))
  (setq hats-buffer nil))

(defun cursorless-clear-overlays ()
  (interactive)
  (measure-time cursorless-clear-overlays
   (remove-overlays nil nil 'cursorless t)))

(defun cursorless-read-hats-json ()
  (with-temp-buffer
    (insert-file-contents-literally cursorless-hats-file)
    (json-parse-buffer :object-type 'alist)))


;;; ---------- NEW VERSION ----------
;; hats are stored as a JSON object like:
;; {
;;   FILEPATH: {
;;     COLORNAME: [ { "start": { "line": l, "character": c },
;;                    "end": { "line": l, "character": c } },
;;                  ... more hat positions ... ]
;;     ... more colors ...
;;   },
;;   ... more files ...
;; }
;;
;; when hats have shapes, COLORNAME is altered (TODO: how).
(defun cursorless-update-hats ()
  (measure-time "update hats"
    (cursorless-update-overlays
    (measure-time "read/index hats"
      (cursorless-index-hats
      (cursorless-read-hats-json))))))

(defvar cursorless-hats (make-hash-table))
(defun cursorless-index-hats (hats-json)
  (clrhash cursorless-hats)
  (cl-loop
   ;; for now we assume there's only one file, so we just grab the cdar.
   for (color . hats) in (cdar hats-json)
   do (cl-loop
       for hat across hats
       ;; assume each hat's start & end position are the same; we only use the
       ;; start.
       for position = (alist-get 'start hat)
       for line     = (alist-get 'line position)
       for column   = (alist-get 'character position)
       for rest     = (gethash line cursorless-hats '())
       for hat     = `(:column ,column
                               :color ,(or (cdr (assoc color cursorless-color-alist))
                                           (symbol-name color)))
       do (puthash line (cons hat rest) cursorless-hats)))
  cursorless-hats)

(defun cursorless-initialize-hats ()
  (cursorless-update-overlays-between (make-hash-table) (point-min) (point-max)))

(defun cursorless-update-overlays (line-hats)
  (cursorless-update-overlays-between line-hats (window-start) (window-end)))

(defun cursorless-update-overlays-between (line-hats &optional start end)
  (measure-time "update overlays"
   (remove-overlays start end 'cursorless t)
   (let ((font-width  (window-font-width))
         (font-height (window-font-height)))
     (save-excursion
       (goto-char (or start (point-min)))
       (cl-loop
        for line = (1- (line-number-at-pos)) then (1+ line)
        until (or (eobp) (>= (point) end))
        for line-start = (point)
        do (end-of-line)
        for columns = (- (point) line-start)
        do (let* ((overlay (make-overlay line-start line-start))
                  (text    (copy-sequence "x\n"))
                  (hats    (gethash line line-hats '()))
                  (image   (cursorless-line-svg columns hats font-width font-height)))
             (put-text-property 0 (length text) 'line-height t text)
             (put-text-property 0 1 'display image text)
             (overlay-put overlay 'cursorless t)
             (overlay-put overlay 'before-string text))
        do (forward-line))))))

(defun cursorless-line-svg (columns hats font-width font-height)
  (let* ((w font-width)
         (dia (* w 0.5)) (h (* 0.6 font-height)) (r (/ dia 2)) (ypos (- h (* r 2)))
         ;; (dia (* w .45)) (h (* w 0.8)) (r (/ dia 2)) (ypos (- h (* r 1.5)))
         (dia (* w .44)) (r (* 0.5 dia)) (h (+ 2 (round dia))) (ypos (- h (* r 1) 1))
         (dia (* w .44)) (r (* 0.5 dia)) (h (ceiling dia)) (ypos (- h r))
         ;; if columns = 0, we still want a 1-pixel-wide image, otherwise we get
         ;; a weird 'empty box' image out of emacs.
         (svg (svg-create (max 1 (* w columns)) h)))
    ;; hats is a list of plists with at least the properties :column & :color.
    (dolist (hat hats)
      (let* ((color (plist-get hat :color))
             (xoffset (* w (plist-get hat :column)))
             (xcenter (+ xoffset (/ w 2.0))))
        ;; TODO: squash this to an ellipse to save vertical space.
        ;(svg-circle svg xcenter ypos r :fill color)
        (svg-ellipse svg xcenter ypos r (/ (- h 1) 2.0) :fill color)
        ;(svg-rectangle svg (- xcenter r) 1 dia dia :fill color)
        ))
    ;; scale 1 because we've already accounted for pixel sizes correctly.
    (svg-image svg :scale 1)))
