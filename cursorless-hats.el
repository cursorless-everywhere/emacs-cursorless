;; READING & DRAWING HATS FROM CURSORLESS
(defconst cursorless-hats-file
  (concat cursorless-directory "vscode-hats.json"))

(defcustom cursorless-color-alist
  '((default . "#999")
    (blue . "#04f")
    (red . "#e00")
    (pink . "#ffa0ff")
    (green . "#0b0")
    (yellow . "ffc000")
    (userColor1 . "#6a00ff")
    (userColor2 . "#ffd8b1"))
  "The mapping from cursorless color phrases to emacs colors."
  :type '(alist :key-type symbol :value-type string)
  :group 'cursorless)

(defvar cursorless-show-hats t)

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
       (when cursorless-show-hats (cursorless-update-hats)))
     (setq cursorless-updating-hats nil))))
(defun cursorless-hats-change-callback (&optional event)
  (when cursorless-updating-hats
    (message "cursorless-hats CHANGE RECURSIVE, set cursorless-updating-hats to nil to re-enable"))
  (unless (or cursorless-updating-hats cursorless-hats-update-timer)
    (setq cursorless-hats-update-timer
          (run-with-idle-timer 0 nil 'cursorless-hats-update-callback))))

(defvar cursorless-hats-watcher
  (progn
    (when (and (boundp 'cursorless-hats-watcher) cursorless-hats-watcher)
      (file-notify-rm-watch cursorless-hats-watcher))
    (file-notify-add-watch cursorless-hats-file '(change) 'cursorless-hats-change-callback)))

;; FIXME: need to initialize hats whenever we switch to a buffer without them.
(defun show-hats ()
  (interactive)
  (when cursorless-show-hats (cursorless-clear-overlays))
  (setq cursorless-show-hats t)
  (cursorless-initialize-hats)
  (cursorless-hats-update-callback))

;;; FIXME: need to deinitialize hats in all buffers which have them.
(defun hide-hats ()
  (interactive)
  (when cursorless-show-hats (cursorless-clear-overlays))
  (setq cursorless-show-hats nil))

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
(defvar cursorless-hats-json)
(defvar cursorless-hats-buffer nil)

(defun cursorless-update-hats ()
  ;; TODO: need to _actually_ use the file path info from the hats file.
  ;; to do this we need a reverse map from temp files to buffers.
  (let* ((json (cursorless-read-hats-json))
         (temporary-file (and (caar json) (symbol-name (caar json))))
         (buffer (gethash temporary-file cursorless-temporary-file-buffers)))
    (cond
     ((null json)
      (message "cursorless-update-hats: vscode-hats.json contained empty object."))
     ((null temporary-file)
      (warn "could not extract temporary file name from json")
      (setq cursorless-hats-json json))
     ((null buffer)
      (warn "temporary file not associated with a buffer: %S" temporary-file)))
    (unless (equal buffer cursorless-hats-buffer)
      (when (and cursorless-hats-buffer (buffer-live-p cursorless-hats-buffer))
        (with-current-buffer cursorless-hats-buffer (cursorless-clear-overlays)))
      (when (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer (cursorless-initialize-hats))))
    (setq cursorless-hats-buffer buffer)
    (measure-time "update hats"
      (cursorless-update-overlays
        (measure-time "read/index hats"
          (cursorless-index-hats json))))))

(defvar cursorless-hats (make-hash-table))
(defun cursorless-index-hats (hats-json)
  (clrhash cursorless-hats)
  (cl-loop
   ;; for now we assume there's only one file, so we just grab the cdar.
   ;; FIXME: use cursorless-temporary-file-buffers instead!
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

;; FIXME: multiplying character width by character offset doesn't work for
;; strange-width characters (eg. TABS!).
(defun cursorless-line-svg (columns hats font-width font-height)
  (let* ((h     (* font-height .3333))
         (xrad  (* .2857 font-width))
         (yrad  (* .4 h))
         (ypos  (- h yrad))
         ;; if columns = 0, we still want a 1-pixel-wide image, otherwise we get
         ;; a weird 'empty box' image out of emacs.
         (svg (svg-create (max 1 (* font-width columns)) h)))
    ;; hats is a list of plists with at least the properties :column & :color.
    (dolist (hat hats)
      (let* ((color (plist-get hat :color))
             (xoffset (* font-width (plist-get hat :column)))
             (xcenter (+ xoffset (/ font-width 2.0))))
        ;(svg-circle svg xcenter ypos r :fill color)
        ;; squashed ellipse to save vertical space.
        (svg-ellipse svg xcenter ypos xrad yrad :fill color)
        ;(svg-rectangle svg (- xcenter r) 1 dia dia :fill color)
        ))
    ;; scale 1 because we've already accounted for pixel sizes correctly.
    (svg-image svg :scale 1)))

(provide 'cursorless-hats)
