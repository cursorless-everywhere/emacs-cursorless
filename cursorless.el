; a very simple test test test test test
(require 'cl-macs)

(defvar cursorless-serial-number 0)

(defconst cursorless-directory "~/.cursorless/")
(defconst cursorless-editor-state-file
  (concat cursorless-directory "editor-state.json"))
(defconst cursorless-hats-file
  (concat cursorless-directory "vscode-hats.json"))

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


;; DUMPING OUR STATE TO CURSORLESS ;;
(make-variable-buffer-local 'cursorless-temporary-file)
;; maybe this should bepermanent-local? see 'make-variable-buffer-local

(defun cursorless-temporary-file-path ()
  ;; try to look at buffer-local variable and make one if not existent
  ;; TODO: treat minibuffer specially
  (if (and (local-variable-p 'cursorless-temporary-file)
           ;; if file has been deleted we probably want to make a new one.
           (file-exists-p cursorless-temporary-file))
      cursorless-temporary-file
    (let* ((file-name (buffer-file-name))
           (extension (and file-name (concat "." (file-name-extension file-name))))
           (temporary-file-directory (concat temporary-file-directory "cursorless.el"))
           (prefix (replace-regexp-in-string "[*/\\\\]" "_" (buffer-name))))
      (make-directory temporary-file-directory t)
      (setq cursorless-temporary-file (make-temp-file prefix nil extension)))))

(defun cursorless-dump-state ()
  (interactive)
  (write-region nil nil (cursorless-temporary-file-path) nil 'ignore-message)
  (let ((state (get-state))
        (buffer (current-buffer)))
    (with-temp-file cursorless-editor-state-file
      (json-insert state)
      (json-pretty-print-buffer) ;; optional, for human consumption
      )))

(defun cursorless-send-state ()
  ;; TODO: maybe figure out how to avoid dumping state if it didn't change?
  ;; but when will that happen?
  (unless (minibufferp) ;; don't do anything when in minibuffer
    (message "cursorless sending state")
    ;(setq cursorless-send-state-timer nil)
    (setq cursorless-serial-number (+ 1 cursorless-serial-number))
    (cursorless-dump-state)))

(defvar cursorless-enabled t)
(defvar cursorless-send-state-timer
  (let ((timer (timer-create)))
    (timer-set-function  timer 'cursorless-send-state)
    ;; idle time 1ms, gotta go fast
    (timer-set-idle-time timer 0.001 nil) ; don't repeat automatically
    timer))

(defun cursorless-enable ()
  (interactive)
  (setq cursorless-enabled t)
  (cursorless-send-state))

(defun cursorless-disable ()
  (interactive)
  (setq cursorless-enabled nil)
  (cancel-timer cursorless-send-state-timer))

;;; Scrolling seems janky, but it doesn't look like we're causing it?
;;; that is, removing this hook doesn't seem to fix the issue.
(defun cursorless-send-state-callback ()
  (when cursorless-enabled
    (if cursorless-send-state-timer
        (timer-activate-when-idle cursorless-send-state-timer t)
      (error "no idle timer")))
  ;; (when (and cursorless-enabled
  ;;            (null cursorless-send-state-timer))
  ;;   (setq cursorless-send-state-timer
  ;;         (run-with-idle-timer 0.1 nil 'cursorless-send-state)))
  )

(add-hook 'post-command-hook 'cursorless-send-state-callback)
;(remove-hook 'post-command-hook 'cursorless-send-state-callback)


;; DRAWING & READING HATS FROM CURSORLESS ;;
(defun cursorless-clear-overlays ()
  (interactive)
  (remove-overlays nil nil 'cursorless t))

;; TODO: defcustom
(defparam cursorless-color-alist
  '((default . "#999")
    (blue . "#04f")
    (red . "dark red")
    (pink . "coral")
    (green . "#0b0")
    ))

(defparam cursorless-color-alist ; dark theme
  '((default . "#999")
    (blue . "#0af")
    (red . "#f00")
    (pink . "#fa8072")
    (green . "#0a0")
    ))

(defun cursorless-update-overlays (hats)
  (cursorless-clear-overlays)
  (maphash 'cursorless-add-overlay-to-line (cursorless-hat-images hats)))

(defun cursorless-add-overlay-to-line (line svg)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (let* ((overlay (make-overlay (point) (point)))
           (text (copy-sequence "x\n"))
           (image (svg-image svg :scale 1)))
      (put-text-property 0 (length text) 'line-height t text)
      (put-text-property 0 1 'display image text)
      (overlay-put overlay 'cursorless t)
      (overlay-put overlay 'before-string text))))

(defvar cursorless-hat-images (make-hash-table))
(defun cursorless-hat-images (hats)
  (clrhash cursorless-hat-images)
  (let* ((w (window-font-width))
         (dia (* w 0.5)) (h (* 0.6 (window-font-height))) (r (/ dia 2)) (ypos (- h (* r 2)))
         ;; (dia (* w .45)) (h (* w 0.8)) (r (/ dia 2)) (ypos (- h (* r 1.5)))
         (dia (* w .44)) (r (* 0.5 dia)) (h (+ 2 (round dia))) (ypos (- h (* r 1) 1))
         )
    ;; hats is a list ((color line offset) (color line offset) ...)
    (dolist (hat hats)
      (cl-destructuring-bind (color line column) hat
        (let* ((color (or (cdr (assoc color cursorless-color-alist))
                          (symbol-name color)))
               (svg (or (gethash line cursorless-hat-images)
                        (let ((columns (save-excursion
                                         (save-restriction
                                           (widen)
                                           (goto-char (point-min))
                                           (forward-line line)
                                           (let ((before (point)))
                                             (end-of-line)
                                             (- (point) before))))))
                          (puthash line (svg-create (* w columns) h)
                                   cursorless-hat-images)))))
          (svg-circle svg (+ (* w column) (/ w 2.0)) ypos r :fill color))))
    cursorless-hat-images))

;; what is hats? a list ((color line offset) (color line offset) ...)
(defun cursorless-update-overlays-obsolete (hats)
  (cursorless-clear-overlays)
  (dolist (h hats)
    (cl-destructuring-bind (color line offset) h
      (let* ((color (or (cdr (assoc color cursorless-color-alist))
                        (symbol-name color)))
             (position (line-and-column-to-offset line offset))
             (o (make-overlay position (+ 1 position) (current-buffer) t nil))
             (o2 (make-overlay position position (current-buffer)))
             (string (copy-sequence "x"))
             (svg (let* ((existing-face (get-text-property (point) 'face))
                         (svg (svg-create (window-font-width) (window-font-height))))
                    (ignore svg "x" :fill "black" :x 0 :y  0)
                    (svg-circle svg 0 0 15 :fill "black" :stroke "red")
                    svg))
             )
        (overlay-put o 'cursorless t)

        (overlay-put o2 'cursorless t)
        (put-text-property 0 (length string) 'display (svg-image svg) string)
        (overlay-put o2 'before-string string)

        ;; (overlay-put o2 'display (svg-image svg))

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
   (cursorless-clear-overlays))
  (setq hats-buffer nil))

(defun show-hats ()
  (interactive)
  (setq hats-buffer (current-buffer))
  (cursorless-update-overlays (read-hats cursorless-hats-file)))

(defvar hats-buffer nil)

(defun hats-change-callback (event)
  (when hats-buffer
    (with-current-buffer hats-buffer
      (cursorless-update-overlays (read-hats cursorless-hats-file))
      ;; (message "Updated hats in %s" hats-buffer)
      )))

(defvar vscode-hats-watcher
  (file-notify-add-watch cursorless-hats-file '(change) 'hats-change-callback))
