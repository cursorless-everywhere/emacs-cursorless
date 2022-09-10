;; SYNCING OUR STATE TO CURSORLESS

(defvar cursorless-serial-number 0)
(defconst cursorless-editor-state-file
  (concat cursorless-directory "editor-state.json"))

;; Maps from temporary file paths (strings) to their buffers.
(defvar cursorless-temporary-file-buffers (make-hash-table :test 'equal))
(make-variable-buffer-local 'cursorless-temporary-file)
;; permanent-local --> survives major mode change
(put 'cursorless-temporary-file 'permanent-local t)

;; Call if cursorless-temporary-file-buffers gets out of sync. Shouldn't happen
;; in normal use.
(defun cursorless-refresh-temporary-file-buffers ()
  (interactive)
  (clrhash cursorless-temporary-file-buffers)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (and (local-variable-p 'cursorless-temporary-file))
        (puthash cursorless-temporary-file b cursorless-temporary-file-buffers)))))

(defun cursorless-kill-buffer-callback ()
  (when (local-variable-p 'cursorless-temporary-file)
    (remhash cursorless-temporary-file cursorless-temporary-file-buffers)
    (delete-file cursorless-temporary-file)))

;; need buffer-list-update-hook or kill-buffer-hook
(add-hook 'kill-buffer-hook 'cursorless-kill-buffer-callback)

(defvar cursorless-sync-state t)

;; TODO: this doesn't work for comint buffers, which can update without the user
;; issuing a command. Use after-change-functions instead/in addition?
(add-hook 'post-command-hook 'cursorless--send-state-when-idle)

;; TODO: do we really need cursorless-{enable,disable}-sync?
(defun cursorless-enable-sync ()
  (interactive)
  (setq cursorless-sync-state t)
  (cursorless-send-state))

(defun cursorless-disable-sync ()
  (interactive)
  (setq cursorless-sync-state nil))

;;; Scrolling seems janky, but it doesn't look like we're causing it?
;;; that is, removing this hook doesn't seem to fix the issue.
(defun cursorless--send-state-when-idle ()
  (run-with-idle-timer 0 nil 'cursorless-send-state))

(defun cursorless--should-draw-hats-p ()
  (and (not (minibufferp))
       (not (-contains? '(magit-log-mode magit-status-mode magit-revision-mode helpful-mode debugger-mode +doom-dashboard-mode) major-mode))
       (not (s-equals? (buffer-name) "*cursorless-log*" ))))

(defun cursorless-send-state ()
  ;; TODO: maybe figure out how to avoid dumping state if it didn't change?
  ;; but when will that happen?
  (when cursorless-sync-state
    (when (cursorless--should-draw-hats-p)
      (progn
        (setq cursorless-serial-number (+ 1 cursorless-serial-number))
        (cursorless-dump-state)))))

(defun cursorless-dump-state ()
  (interactive)
  ;; TODO: only write if buffer contents have changed since last write!
  ;; TODO: check if file is too damn big.
  ;; Use utf-8 and avoid auto-compression etc based on file extension.
  (let ((coding-system-for-write 'utf-8)
        (file-name-handler-alist '()))
    (write-region (point-min) (point-max) (cursorless-temporary-file-path) nil 'ignore-message))
  (let ((state (cursorless-get-state)))
    (cursorless-log (format "dumping state for %S \n %S" (current-buffer) (cursorless--json-pretty-print (json-encode state ) ) ))
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
    :lastVisibleLine  (line-number-at-pos (window-end))
    ;; where the cursors are. in emacs, only one cursor, so a singleton vector.
    ;; note that cursorless wants line/column, not offset.
    ;; TODO: if transient-mark-mode is enabled, represent the whole selection.
    :cursors (vector (cursorless-line-and-column (point))))
   ))

(defun cursorless-temporary-file-path ()
  (unless (and (local-variable-p 'cursorless-temporary-file)
               ;; If file has been deleted we must make a new one.
               (file-exists-p cursorless-temporary-file))
    (let* ((file-extension (and (buffer-file-name)
                                (file-name-extension (buffer-file-name))))
           (suffix (if file-extension (concat "." file-extension) ""))
           (dirname (concat (file-name-as-directory temporary-file-directory)
                            "cursorless.el/"))
           (name (replace-regexp-in-string "[*/\\\\]" "_" (buffer-name)))
           (prefix (concat dirname name "-")))
      (make-directory dirname t)
      ;; make-temp-file-internal because it doesn't try to do magic with file names
      (setq cursorless-temporary-file (make-temp-file-internal prefix nil suffix nil))
      (puthash cursorless-temporary-file (current-buffer)
               cursorless-temporary-file-buffers)))
  cursorless-temporary-file)

(provide 'cursorless-state)
