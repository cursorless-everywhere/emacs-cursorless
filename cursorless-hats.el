;;; cursorless-hats.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;; The code for managing hat decorations.
;;
;;; Code:

(require 'dash)
(require 's)
(require 'json)

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
  (unless cursorless-running-command
    ;; recursive invocations can occur if running cursorless-update-hats causes
    ;; the hats file to change; eg. if cursorless-update-hats writes to *Messages*
    ;; and we're visiting *Messages*. Without care this can lock up emacs.
    (if cursorless-updating-hats
        (progn
          (cursorless-log "cursorless-hats-update-callback: recursive invocation detected!")
          (warn "cursorless-hats-update-callback: recursive invocation detected!"))
      ;; the hats file could be stale which would make finding the right cursor positions fail
      ;; with an end-of-buffer error. ignore these errors since they're normally remediated
      ;; very quickly.
      (if (not (ignore-error end-of-buffer
                 (setq cursorless-updating-hats t)
                 (when cursorless-show-hats
                   (let ((hats-json (cursorless-read-hats-json)))
                     (-each hats-json (lambda (file-hats)
                                        (-let* (((file . hats) file-hats)
                                                (file (symbol-name file))
                                                (related-buffer (gethash file cursorless-temporary-file-buffers)))
                                          (if (not related-buffer)
                                              (message "temporary file not associated with a buffer: %S" file)
                                            (cursorless-update-hats related-buffer hats)))))))
                 t))
          (cursorless-log "failed to update hats (end-of-buffer aka stale hats file)"))
      (setq cursorless-updating-hats nil))))

(defun cursorless-hats-change-callback (event)
  (when (equal (nth 1 event) 'renamed)
    (when cursorless-updating-hats
      (cursorless-log (format "cursorless-hats CHANGE RECURSIVE, set cursorless-updating-hats to nil to re-enable\n%s"  (backtrace-to-string)))
      (message "cursorless-hats CHANGE RECURSIVE, set cursorless-updating-hats to nil to re-enable"))
    (unless cursorless-updating-hats
      (cursorless-hats-update-callback))))

(defvar cursorless-hats-watcher
  (progn
    (when (and (boundp 'cursorless-hats-watcher) cursorless-hats-watcher)
      (file-notify-rm-watch cursorless-hats-watcher))
    (file-notify-add-watch cursorless-hats-file '(change) 'cursorless-hats-change-callback)))

;; FIXME: need to initialize hats whenever we switch to a buffer without them.
(defun cursorless-show-hats ()
  (interactive)
  (when cursorless-show-hats (cursorless-clear-overlays))
  (setq cursorless-show-hats t)
  (cursorless-hats-update-callback))

(defun cursorless-hide-hats ()
  (interactive)
  ;; TODO: filter buffer-list by a cursorless-mode marker
  (-each (buffer-list) (lambda (buffer)
                         (with-current-buffer buffer
                             (cursorless-clear-overlays))))
  (setq cursorless-show-hats nil))

(defun cursorless-clear-overlays ()
  (interactive)
  (remove-overlays nil nil 'cursorless t))

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

(defun cursorless-update-hats (buffer hats)
  "Update BUFFER with HATS."
  (with-current-buffer buffer
    (cursorless-log (format "updating hats on %S" buffer))
    (cursorless-clear-overlays)
    (-map (lambda(color-shape-positions)
            (-let* (((color shape) (s-split "-" (symbol-name (car color-shape-positions))))
                    (draw-hat (-partial 'cursorless-draw-hat (intern color) shape)))
              (-map draw-hat (cdr color-shape-positions)))) hats)
    (cursorless-log (format "done updating hats on %S" buffer))))

(defun cursorless-point-from-cursorless-position (cursorless-position)
  "Return the proper point for an alist from a cursorless position.

CURSORLESS-POSITION is an alist parsed from `cursorless-read-hats-json'."
  (let ((pos (alist-get 'start cursorless-position)))
    (save-excursion
      (goto-char (point-min))
      ;; consider using https://emacs.stackexchange.com/a/3822. this offers roughly a 10x speedup.
      (forward-line (alist-get 'line pos))
      (forward-char (alist-get 'character pos))
      (point))))

(defun cursorless--get-hat-color (cursorless-color)
  (if-let ((hat-color (alist-get cursorless-color cursorless-color-alist)))
      hat-color
    (display-warning 'cursorless
                     (format "Unable to find mapping for cursorless color %s."
                             cursorless-color) :error)))

(defun cursorless-draw-hat (cursorless-color cursorless-shape cursorless-position)
  "Draw an individual hat on the current buffer.

CURSORLESS-COLOR is a color name (e.g. default, blue, pink) that gets translated
through `cursorless-color-alist'.

CURSORLESS-SHAPE is the shape to render. If CURSORLESS-SHAPE is nil, the default
dot gets rendered.

CURSORLESS-POSITION is an alist parsed from `cursorless-read-hats-json'."
  (when-let* ((hat-color (cursorless--get-hat-color cursorless-color))
              (hat-point (cursorless-point-from-cursorless-position cursorless-position))
              (hat-overlay (make-overlay hat-point (+ hat-point 1))))
    (overlay-put hat-overlay 'cursorless t)
    (cond ((null cursorless-shape)
           (overlay-put hat-overlay 'face `(:cursorless ,hat-color)))
          ((string-equal cursorless-shape "frame")
           (overlay-put hat-overlay 'face `(:box (:line-width (0 . -2) :color ,hat-color))))
          (t (display-warning
              'cursorless
              (format "Unable to find mapping for cursorless shape %s." cursorless-shape) :error)))))

(provide 'cursorless-hats)
;;; cursorless-hats.el ends here
