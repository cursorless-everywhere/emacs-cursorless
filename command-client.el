;; Implements a command client for emacs, forwarding cursorless commands over a
;; socket to the VSCode sidecar.

(defconst command-server-directory-name "emacs-command-server"
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
  (interactive)
  (let ((d (command-server-directory)))
   (unless (and (file-exists-p d) (file-directory-p d))
     (make-directory d))))

(defun command-server-quit ()
  (interactive)
  (delete-directory (command-server-directory) t))

(command-server-start)
(add-hook 'kill-emacs-hook 'command-server-quit)

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
         (wait-for-finish (null (eq :false (gethash "waitForFinish" request))))
         (return-command-output (null (eq :false (gethash "returnCommandOutput" request))))
         (uuid (gethash "uuid" request)))
    (cl-flet ((respond (value)
                (with-temp-file response-path
                  (json-insert `(:uuid ,uuid
                                 :warnings []
                                 :error :null
                                 :returnValue ,value))
                  (insert "\n"))))
     ;; TODO: Eventually I'd like to make it possible to run arbitrary emacs lisp
     ;; code via the command server. For now, though, I'm just going to
     ;; special-case cursorless.
     (cond

      ;; -- CURSORLESS COMMANDS --
      ((string-equal command-id "cursorless.command")
       ;; Forward to vscode. TODO: When wait-for-finish is true, we should wait
       ;; _asynchronously_ to hear back from vscode. So we have to set up a
       ;; callback which writes to response-path. Maybe fork a thread? or have a
       ;; dedicated thread?
       (let ((payload (make-hash-table :size 2)))
         (puthash "command" "cursorless" payload)
         (puthash "cursorlessArgs" (json-serialize args) payload)
         (setq payload (json-serialize payload))
         (cursorless-send payload))
       ;; For now write an empty response.
       (respond :null))

      ;; -- ELISP EVAL --
      ((string-equal command-id "eval")
       (unless (eql 1 (seq-length args)) (error "eval takes only one argument"))
       (let* ((code-string (elt args 0))
              (res (read-from-string code-string))
              (code (car res))
              (_ (unless (eql (cdr res) (length code-string))
                   (error "code contained unparsed junk"))))
         ;; Assume the result is json-encodable. TODO: handle case it's not.
         (respond (eval code))))

      ;; -- ELISP CALL --
      ((string-equal command-id "call")
       (let* ((func (intern (elt args 0)))
              (args (seq-into (seq-drop args 1) 'list)))
         (respond (apply func args))))

      ;; -- INTERACTIVE ELISP CALL --
      ((string-equal command-id "call-interactively")
       (when return-command-output
         (warn "Requested command output of call-interactively; that is likely to time out, ignoring."))
       (let ((func (intern (elt args 0))))
         (unless (fboundp func) (error "Function not bound: %S" func))
         ;; We issue the response _first_, so that we don't hang if the
         ;; interactive call takes a while.
         (respond :null)
         (call-interactively (intern (elt args 0)))))

      ;; -- UNRECOGNIZED --
      (t
       ;; TODO: write an error response.
       (error "Unrecognized command id %S" command-id))))))


;;; ---------- emacs -> vscode over cursorless socket ----------
(defvar cursorless-socket-buffer (generate-new-buffer "*cursorless-vscode-socket*"))

(defun cursorless-sentinel (proc event)
  ;(message "cursorless-sentinel: %s(%s) %s" proc (process-status proc) event)
  (let ((status (process-status proc)))
    (if (not (and (equal status 'closed)
                  (equal event "connection broken by remote peer\n")))
        (warn "Cursorless: unexpected error on communicating with vscode: %s, %s" status event)
      (cursorless-receive
       (with-current-buffer cursorless-socket-buffer
         ;; (message "-- CURSORLESS received: %s"
         ;;          (buffer-substring-no-properties (point-min) (point-max)))
         (goto-char (point-min)) ;; json-parse-buffer parses forward from point.
         (json-parse-buffer))))))

(defun cursorless-send (cmd)
  ;; TODO: need to figure out what to do if we issue another cursorless-send
  ;; before the response for the first send is received.
  (with-current-buffer cursorless-socket-buffer
    (erase-buffer))
  (let ((p (make-network-process
            :name "cursorless"
            :family 'local
            :remote (expand-file-name "~/.cursorless/vscode-socket")
            :buffer cursorless-socket-buffer
            :sentinel 'cursorless-sentinel)))
    ;; (message "-- CURSORLESS sending: %s" cmd)
    (process-send-string p cmd)))

(defun cursorless-receive (response)
  ;; TODO: handle replies like "pong" which don't give a new state.

  ;; TODO: The command finished, process its results. We should (a) propagate
  ;; results back across the command server to talon; (b) apply changes using
  ;; the "newState" field.
  ;;
  ;; To apply changes:
  ;; - figure out which buffer to update from "path"
  ;; - diff the "contentsPath" against buffer (or temporary file?) contents & apply updates
  ;; - update the cursor(s) from "cursors"
  (let* ((new-state (gethash "newState" response))
         (path (gethash "path" new-state))
         (contents-path (gethash "contentsPath" new-state)))
    ;; Find the buffer to update. For now, we just check it's the current buffer.
    (unless (and (local-variable-p 'cursorless-temporary-file)
                 (string-equal path cursorless-temporary-file))
      (error "Update to non-current buffer, ignoring!"))
    ;; Ideally we'd do a diff and then apply the minimal update. Instead I'm
    ;; just going to replace the whole buffer.
    (unless (file-exists-p contents-path) (error "No contents file!"))
    (insert-file-contents-literally contents-path nil nil nil t)
    ;; Update cursor & selection.
    ;; assume 1 cursor for now.
    (let* ((cursor (elt (gethash "cursors" new-state) 0))
           (active (gethash "active" cursor))
           (anchor (gethash "anchor" cursor))
           (line   (gethash "line" active))
           (column (gethash "character" active))
           (anchor-line (gethash "line" anchor))
           (anchor-column (gethash "character" anchor))
           (selection (not (and (eql line anchor-line)
                                (eql column anchor-column)))))
      ;; Update the selection.
     (when selection
       (goto-char (point-min))
       (forward-line anchor-line)
       (forward-char anchor-column)
       ;; location = (point), nomsg = t
       (push-mark (point) t))
     ;; Update cursor position.
     (goto-char (point-min))
     (forward-line line)
     (forward-char column)
     (when selection (setq transient-mark-mode '(only))))

    ;; Update state for cursorless to read.
    (cursorless-send-state-callback)))

;; ping, state, stateWithContents, applyPrimaryEditorState (?),
;; command, cursorless, pid
;;
;; command: runs a command
;; cursorless: runs a command then serializes state afterward
;;
;; what is 'applyPrimaryEditorState'?
;(cursorless-send "{\"command\": \"ping\"}")

;; ;; see also
;; (accept-process-output p 1) ; semi-blocking interface
;; (all-threads) ; emacs has (cooperative) threads! could use them? nah.


(global-set-key (kbd "<C-f17>") 'command-server-trigger)

(provide 'command-client)
