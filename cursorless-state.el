;; SYNCING OUR STATE TO CURSORLESS

(defvar cursorless-serial-number 0)
(defconst cursorless-editor-state-file
  (concat cursorless-directory "editor-state.json"))

(make-variable-buffer-local 'cursorless-temporary-file)
;; permanent-local --> survives major mode change
(put 'cursorless-temporary-file 'permanent-local t)
;; TODO: remove temporary files when the buffer is closed
;; need buffer-list-update-hook

(defvar cursorless-sync-state t)
(defvar cursorless-send-state-timer (timer-create))
(timer-set-function cursorless-send-state-timer 'cursorless-send-state)
(timer-set-idle-time cursorless-send-state-timer 0 nil)

;; TODO: this doesn't work for comint buffers, which can update without the user
;; issuing a command. Use after-change-functions instead/in addition?
(add-hook 'post-command-hook 'cursorless-send-state-callback)
;(remove-hook 'post-command-hook 'cursorless-send-state-callback)

;; TODO: do we really need cursorless-{enable,disable}-sync?
(defun cursorless-enable-sync ()
  (interactive)
  (setq cursorless-sync-state t)
  (cursorless-send-state))

(defun cursorless-disable-sync ()
  (interactive)
  (setq cursorless-sync-state nil)
  (cancel-timer cursorless-send-state-timer))

;;; Scrolling seems janky, but it doesn't look like we're causing it?
;;; that is, removing this hook doesn't seem to fix the issue.
(defun cursorless-send-state-callback ()
  (when cursorless-sync-state
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
  (let ((state (cursorless-get-state)))
    (with-temp-file cursorless-editor-state-file
      (json-insert state))))

;; Serialize editor state to file, at the moment:
;; - a serial number
;; - current file path
;; - top & bottom visible lines
;;   edge case: long lines without wrapping, does cursorless hat them?
;;   looks like it does.
;; - where the cursors/selections are
(defun cursorless-get-state ()
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
