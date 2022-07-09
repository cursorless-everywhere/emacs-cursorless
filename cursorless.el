(require 'cl-macs)

(defmacro measure-time (name &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     (prog1 (progn ,@body)
       (message "%30s %3d ms" ',name (round (* 1000 (float-time (time-since time))))))))

(defun line-and-cloumn (pos)
  ;; Note that (current-column) is wrong, we want # of characters since start of
  ;; line, NOT the logical position. (eg. tab counts as 1 char).
  ;; cursorless line numbers are 1-indexed. not sure about column numbers.
  (list
   'line (line-number-at-pos pos t)
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
    (progn ;measure-time
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
      (json-pretty-print-buffer) ;; for human consumption (optional)
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

(defvar cursorless-hats-update-timer nil)
(defun cursorless-hats-update-callback (&optional event)
  (setq cursorless-hats-update-timer nil)
  (when hats-buffer (with-current-buffer hats-buffer (cursorless-update-hats))))
(defun cursorless-hats-change-callback (&optional event)
  (unless cursorless-hats-update-timer
    (setq cursorless-hats-update-timer
          (run-with-idle-timer 0.1 nil 'cursorless-hats-update-callback))))

(defvar cursorless-hats-watcher
  (progn
    (when (and (boundp 'cursorless-hats-watcher) cursorless-hats-watcher)
      (file-notify-rm-watch cursorless-hats-watcher))
    (file-notify-add-watch cursorless-hats-file '(change) 'cursorless-hats-change-callback)))

;; TODO: defcustom
(defparam cursorless-color-alist
  '((default . "#999") (blue . "#04f") (red . "dark red") (pink . "coral") (green . "#0b0")))
(defparam cursorless-color-alist ; dark theme
  '((default . "#999") (blue . "#0af") (red . "#f00") (pink . "#fa8072") (green . "#0a0")))

(defun show-hats ()
  (interactive)
  (when hats-buffer (with-current-buffer hats-buffer (cursorless-clear-overlays)))
  (setq hats-buffer (current-buffer))
  (cursorless-update-hats))

(defun hide-hats ()
  (interactive)
  (when hats-buffer (with-current-buffer hats-buffer (cursorless-clear-overlays)))
  (setq hats-buffer nil))

(defun cursorless-clear-overlays ()
  (interactive)
  (measure-time cursorless-clear-overlays
   (remove-overlays nil nil 'cursorless t)))

(defun cursorless-read-hats-json ()
  (measure-time cursorless-read-hats-json
   (with-temp-buffer
     (insert-file-contents-literally cursorless-hats-file)
     (json-parse-buffer :object-type 'alist))))


;; ;;; ---------- OLD VERSION ----------
;; (defun cursorless-update-hats ()
;;   (message " ")
;;   (measure-time cursorless-update-hats
;;     (cursorless-update-overlays (read-hats))))

;; (defun read-hats ()
;;   (measure-time read-hats
;;    (let* ((hats (cursorless-read-hats-json))
;;           (file-hats (cdar hats)))
;;      ;; hats-json is an alist of the form ((file . file-hats) ...)
;;      ;; file-hats are of the form: ((color . [hat ...]) ...)
;;      ;; hats are of the form: ((start (line . n) (character . n)) (end (line . n) (character . c)))
;;      ;;
;;      ;; for now we assume there's only one file, so we just grab the cdar
;;      ;; TODO: this ought to be much simpler.
;;      (cl-loop for (color . hats) in file-hats
;;               nconc (cl-loop for hat across hats
;;                              collect (let ((x (alist-get 'start hat)))
;;                                        (list color (alist-get 'line x) (alist-get 'character x))))))))

;; (defun cursorless-update-overlays (hats)
;;   (measure-time cursorless-update-overlays
;;    (cursorless-clear-overlays)
;;    (overlay-recenter (point))
;;    (let ((images (cursorless-hat-images hats)))
;;      (measure-time cursorless-add-overlays
;;        (maphash 'cursorless-add-overlay-to-line images)))))

;; (defvar cursorless-hat-svgs (make-hash-table))
;; (defvar cursorless-hat-images (make-hash-table))
;; (defun cursorless-hat-images (hats)
;;   (measure-time cursorless-hat-images
;;    (clrhash cursorless-hat-svgs)
;;    (clrhash cursorless-hat-images)
;;    (let* ((w (window-font-width))
;;           (dia (* w 0.5)) (h (* 0.6 (window-font-height))) (r (/ dia 2)) (ypos (- h (* r 2)))
;;           ;; (dia (* w .45)) (h (* w 0.8)) (r (/ dia 2)) (ypos (- h (* r 1.5)))
;;           (dia (* w .44)) (r (* 0.5 dia)) (h (+ 2 (round dia))) (ypos (- h (* r 1) 1))
;;           )
;;      ;; hats is a list ((color line offset) (color line offset) ...)
;;      (dolist (hat hats)
;;        (cl-destructuring-bind (color line column) hat
;;          (let* ((color (or (cdr (assoc color cursorless-color-alist))
;;                            (symbol-name color)))
;;                 (svg (or (gethash line cursorless-hat-svgs)
;;                          (let ((columns (save-excursion
;;                                           (goto-char (point-min))
;;                                           (forward-line line)
;;                                           (- (end-of-line-position) (point)))))
;;                            (puthash line (svg-create (* w columns) h)
;;                                     cursorless-hat-svgs)))))
;;            (svg-circle svg (+ (* w column) (/ w 2.0)) ypos r :fill color))))
;;      (cl-loop for line being the hash-keys of cursorless-hat-svgs
;;                        using (hash-values svg)
;;               do (puthash line (svg-image svg :scale 1) cursorless-hat-images))
;;      cursorless-hat-images)))

;; (defun cursorless-add-overlay-to-line (line svg)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (forward-line line)
;;     (let* ((overlay (make-overlay (point) (point)))
;;            (text (copy-sequence "x\n"))
;;            (image svg))
;;       (put-text-property 0 (length text) 'line-height t text)
;;       (put-text-property 0 1 'display image text)
;;       (overlay-put overlay 'cursorless t)
;;       (overlay-put overlay 'before-string text))))


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
  (message " ")
  (measure-time cursorless-update-hats
    (cursorless-update-overlays
     (cursorless-index-hats
      (cursorless-read-hats-json)))))

(defvar cursorless-hats (make-hash-table))
(defun cursorless-index-hats (hats-json)
  (measure-time cursorless-index-hats
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
        do (puthash line (cons hat rest) cursorless-hats))))
  cursorless-hats)

(defun num-columns ()
  (- (line-end-position) (point)))

(defun cursorless-update-overlays (line-hats)
  (cursorless-clear-overlays)
  ;; for every line, generate an image.
  ;; TODO: new strategy:
  ;; 1. for each line, generate info we need to annotate it:
  ;;    (line position num-columns)
  (let ((lines
         (measure-time examine-lines
          (save-excursion
            ;;(goto-char (window-start))
            (goto-char (point-min))
            (cl-loop
             for line = (1- (line-number-at-pos)) then (1+ line)
             ;;until (>= (point) (or (window-end) (point-max)))
             until (>= (point) (point-max))
             for line-start = (point)
             do (end-of-line)
             for columns = (- (point) line-start)
             collect (list line line-start columns)
             do (forward-line)))))
        (font-width (window-font-width))
        (font-height (window-font-height))
        (window-start-line (line-number-at-pos (window-start)))
        (window-end-line   (line-number-at-pos (window-end))))
    ;; 2. for each line, generate an overlay & add the image
    (measure-time add-overlays
     (overlay-recenter (point-max))
     (cl-loop
      for (line position columns) in lines
      ;; when (<= window-start-line line window-end-line)
      do
      (let ((overlay (make-overlay position position))
            (text    (copy-sequence "x\n"))
            (hats    (gethash line line-hats '()))
            (image (cursorless-line-svg columns
                                        (gethash line line-hats '())
                                        font-width font-height)))
        (put-text-property 0 (length text) 'line-height t text)
        (put-text-property 0 1 'display image text)
        (overlay-put overlay 'cursorless t)
        (overlay-put overlay 'before-string text))))))

(defun cursorless-line-svg (columns hats font-width font-height)
  (let* ((w font-width)
         (dia (* w 0.5)) (h (* 0.6 font-height)) (r (/ dia 2)) (ypos (- h (* r 2)))
         ;; (dia (* w .45)) (h (* w 0.8)) (r (/ dia 2)) (ypos (- h (* r 1.5)))
         (dia (* w .44)) (r (* 0.5 dia)) (h (+ 2 (round dia))) (ypos (- h (* r 1) 1))
         ;; if columns = 0, we still want a 1-pixel-wide image, otherwise we get
         ;; a weird 'empty box' image out of emacs.
         (svg (svg-create (max 1 (* w columns)) h)))
    ;; hats is a list of plists with at least the properties :column & :color.
    (dolist (hat hats)
      (svg-circle svg
                  (+ (/ w 2.0) (* w (plist-get hat :column)))
                  ypos
                  r
                  :fill (plist-get hat :color)))
    ;; scale 1 because we've already accounted for pixel sizes correctly.
    (svg-image svg :scale 1)))


;; ;; what is hats? a list ((color line offset) (color line offset) ...)
;; (defun cursorless-update-overlays-obsolete (hats)
;;   (cursorless-clear-overlays)
;;   (dolist (h hats)
;;     (cl-destructuring-bind (color line offset) h
;;       (let* ((color (or (cdr (assoc color cursorless-color-alist))
;;                         (symbol-name color)))
;;              (position (line-and-column-to-offset line offset))
;;              (o (make-overlay position (+ 1 position) (current-buffer) t nil))
;;              (o2 (make-overlay position position (current-buffer)))
;;              (string (copy-sequence "x"))
;;              (svg (let* ((existing-face (get-text-property (point) 'face))
;;                          (svg (svg-create (window-font-width) (window-font-height))))
;;                     (ignore svg "x" :fill "black" :x 0 :y  0)
;;                     (svg-circle svg 0 0 15 :fill "black" :stroke "red")
;;                     svg))
;;              )
;;         (overlay-put o 'cursorless t)

;;         (overlay-put o2 'cursorless t)
;;         (put-text-property 0 (length string) 'display (svg-image svg) string)
;;         (overlay-put o2 'before-string string)

;;         ;; (overlay-put o2 'display (svg-image svg))

;;         ;; TODO: if we have multiple working strategies (eg foreground+underline
;;         ;; vs background-color), we can use these as "shapes/styles"

;;         ;; change background color
;;         (overlay-put o 'face `((:background ,color)))

;;         ;; change foreground color & underline
;;         ;(overlay-put o 'face `((:foreground ,color) (:underline ,color)))

;;         ;(overlay-put o 'face `((:box (:line-width 2 :color ,color))))
;;         ;(overlay-put o 'face `((:overline ,color)))
;;         ))))
