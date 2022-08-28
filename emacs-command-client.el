;; Implements a command client for emacs.

(defcustom command-server-directory-name "emacs-command-server"
  "Name of directory to use for the emacs command server. Will be suffixed with the user's real UID.")

(defun command-server-directory ()
  ;; TODO: on windows suffix should be empty, I think, assuming that
  ;; (temporary-file-directory) returns the appropriate user-specific temp dir
  ;; on windows.
  (let ((suffix (format "-%s" (user-real-uid))))
    (expand-file-name
     (concat command-server-directory-name suffix)
     (temporary-file-directory))))

(defun command-server-start ()
  (make-directory (command-server-directory)))

(defun command-server-quit ()
  ;; TODO: remove the command server directory
  )

(defun command-server-trigger ()
  "Trigger command execution."
  (interactive)
  (let* ((command-directory (command-server-directory))
         (request-path (expand-file-name "request.json" command-directory))
         (response-path (expand-file-name "response.json" command-directory))
         ;; Read request.json
         (request (if (not (file-exists-p request-path))
                      (error "No such file: %s" request-path)
                    (with-temp-buffer
                      (insert-file-contents-literally request-path)
                      ;(message "-- COMMAND SERVER received request: %s" (buffer-string))
                      (json-parse-buffer))))
         (command-id (gethash "commandId" request))
         (args (gethash "args" request))
         (wait-for-finish (gethash "waitForFinish" request))
         (return-command-output (gethash "returnCommandOutput" request))
         (uuid (gethash "uuid" request)))
    ;; TODO: Eventually I'd like to make it possible to run arbitrary emacs lisp
    ;; code via the command server. For now, though, I'm just going to
    ;; special-case cursorless.
    (unless (string-equal command-id "cursorless.command")
      ;; TODO: write an error response.
      (error "Unrecognized command id %S" command-id))

    ;; Forward to vscode. TODO: When wait-for-finish is true, we should wait
    ;; _asynchronously_ to hear back from vscode. So we have to set up a
    ;; callback which writes to response-path. Maybe fork a thread? or have a
    ;; dedicated thread?
    (let ((payload (make-hash-table)))
      (puthash "command" "cursorless" payload)
      (puthash "cursorlessArgs" (json-serialize args) payload)
      (setq payload (json-serialize payload))
      (cursorless-send payload))

    ;; For now write an empty response. FIXME.
    (with-temp-file response-path
      (json-insert `(:uuid ,uuid
                     :warnings []
                     :error :null
                     :returnValue :null))
      (insert "\n"))))

;; TODO: at shutdown, call command-server-quit.
