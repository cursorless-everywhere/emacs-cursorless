(require 'dash)

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
  "Read the hats file and return an alist.

Hats are stored as a JSON object like:
{
  FILEPATH: {
    COLORNAME: [ { \"start\": { \"line\": l, \"character\": c },
                   \"end\": { \"line\": l, \"character\": c } },
                 ... more hat positions ... ]
    ... more colors ...
  },
  ... more files ...
}

COLORNAME is sometimes a color name e.g. blue and sometimes a color followed
by a shape e.g. blue-bolt."
  (with-temp-buffer
    (insert-file-contents-literally cursorless-hats-file)
    (json-parse-buffer :object-type 'alist)))

(defvar cursorless-hats-buffer nil)

(defun cursorless-update-hats ()
  "Update the relevant buffer with the latest hats."
  (let* ((json (cursorless-read-hats-json))
         ;; TODO: only looks at the first file in the hats json.
         (temporary-file (and (caar json) (symbol-name (caar json))))
         (buffer (gethash temporary-file cursorless-temporary-file-buffers)))
    (cond
     ((null json)
      (message "cursorless-update-hats: vscode-hats.json contained empty object."))
     ((null temporary-file)
      (warn "could not extract temporary file name from json"))
     ((null buffer)
      (warn "temporary file not associated with a buffer: %S" temporary-file)))
    ;; clear the previous hat buffers overlays
    (unless (equal buffer cursorless-hats-buffer)
      (when (and cursorless-hats-buffer (buffer-live-p cursorless-hats-buffer))
        (with-current-buffer cursorless-hats-buffer (cursorless-clear-overlays))))
    (setq cursorless-hats-buffer buffer)
    (cursorless-clear-overlays)
    (-map (lambda(color-positions)
            (let ((draw-hat (-partial 'cursorless-draw-hat (car color-positions))))
              (-map draw-hat (cdr color-positions)))) (cdar json))))

(defun point-from-cursorless-position (cursorless-position)
  "Return the proper point for an alist from a cursorless position.

CURSORLESS-POSITION is an alist parsed from `cursorless-read-hats-json'."
  (let ((pos (alist-get 'start cursorless-position)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (alist-get 'line pos))
      (forward-char (alist-get 'character pos))
      (point))))

(defun cursorless-draw-hat (cursorless-color cursorless-position)
  "Draw an individual hat on the current buffer.

CURSORLESS-COLOR is a color name (e.g. default, blue, pink) which is sometimes
followed by a hat shape e.g. default-bolt, blue-fox, pink-frame.

CURSORLESS-POSITION is an alist parsed from `cursorless-read-hats-json'."
  (if-let ((hat-color (alist-get cursorless-color cursorless-color-alist)))
      (let* ((hat-point (point-from-cursorless-position cursorless-position))
             (hat-overlay (make-overlay hat-point (+ hat-point 1))))
        (overlay-put hat-overlay 'cursorless t)
        (overlay-put hat-overlay 'face `(:cursorless ,hat-color)))
    (display-warning 'cursorless
                     (format "Unable to find mapping for cursorless color %s."
                             cursorless-color) :error)))

(provide 'cursorless-hats)
