(setq cursorless-socket-buffer (generate-new-buffer "*cursorless-vscode-socket*"))

(defun cursorless-sentinel (proc event)
  ;(message "cursorless-sentinel: %s(%s) %s" proc (process-status proc) event)
  (let ((status (process-status proc)))
    (if (not (and (equal status 'closed)
                  (equal event "connection broken by remote peer\n")))
        (warn "Cursorless: unexpected error on communicating with vscode: %s, %s" status event)
      ;; TODO: The command finished, process its results. We should probably
      ;; just propagate the results back across the command server to talon?
      (message "Received %s"
               (with-current-buffer cursorless-socket-buffer
                 (buffer-substring-no-properties (point-min) (point-max)))))))

(defun cursorless-send (cmd)
  (with-current-buffer cursorless-socket-buffer
    (erase-buffer))
  (let ((p (make-network-process
            :name "cursorless"
            :family 'local
            :remote (expand-file-name "~/.cursorless/vscode-socket")
            :buffer cursorless-socket-buffer
            :sentinel 'cursorless-sentinel)))
    (process-send-string p cmd)))

;; ping, state, stateWithContents, applyPrimaryEditorState (?),
;; command, cursorless, pid
;;
;; command: runs a command
;; cursorless: runs a command then serializes state afterward
;;
;; what is 'applyPrimaryEditorState'?
(cursorless-send "{\"command\": \"ping\"}")

;; ;; see also
;; (accept-process-output p 1) ; semi-blocking interface
;; (all-threads) ; emacs has (cooperative) threads! could use them? nah.
