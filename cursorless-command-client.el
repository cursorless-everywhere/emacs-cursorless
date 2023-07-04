;;; cursorless-command-client.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;; Implements a command client for Emacs, forwarding cursorless commands over a
;; socket to the VSCode sidecar.
;;
;;; Code:

(require 'dash)

(require 'command-server)

(defconst command-server-directory-name "emacs-command-server"
  "Name of directory to use for the Emacs command server. Will be suffixed with the user's real UID.")

(defvar cursorless--last-response-processed nil
  "Store the last time a response was processed. This is useful for ensuring multiple
commands don't stomp on each other.")

(defvar cursorless-running-command nil
  "Store whether a command is currently running to avoid certain actions (updating hats).")

(defun cursorless--time-in-milliseconds ()
  ;; TODO: there's probably a better way to do this.
  (string-to-number (format-time-string "%s.%3N")))

(defun cursorless-command-server-directory ()
  ;; TODO: on windows suffix should be empty, I think, assuming that
  ;; (temporary-file-directory) returns the appropriate user-specific temp dir
  ;; on windows.
  (let ((suffix (format "-%s" (user-real-uid))))
    (expand-file-name
     (concat command-server-directory-name suffix)
     (temporary-file-directory))))

(defun cursorless-command-server-start ()
  (interactive)
  (let ((d (cursorless-command-server-directory)))
    (unless (and (file-exists-p d) (file-directory-p d))
      (make-directory d))))

(defun cursorless-command-server-quit ()
  (interactive)
  (delete-directory (cursorless-command-server-directory) t))

(cursorless-command-server-start)
(add-hook 'kill-emacs-hook 'cursorless-command-server-quit)


(defvar cursorless--current-command-uuid nil)

(defun cursorless--command-server-handler (command-uuid wait-for-finish args)
  ;; TODO: we ignore wait-for-finish
  (setq cursorless-running-command t)
  (setq cursorless--current-command-uuid command-uuid)
  (let ((payload (make-hash-table :size 2)))
    (puthash "command" "cursorless" payload)
    (puthash "cursorlessArgs" (json-serialize args) payload)
    (cursorless-log (format  "sending command: %s" (cursorless--json-pretty-print (json-encode payload))))
    (setq payload (json-serialize payload))
    (cursorless-send payload)))

(add-to-list 'command-server-command-handlers
             '("cursorless.command" . cursorless--command-server-handler))

;;; ---------- emacs -> vscode over cursorless socket ----------
(defvar cursorless-socket-buffer (generate-new-buffer "*cursorless-vscode-socket*"))


(defun cursorless-sentinel (proc event)
  (let ((status (process-status proc)))
    (if (not (and (equal status 'closed)
                  (equal event "connection broken by remote peer\n")))
        (warn "Cursorless: unexpected error on communicating with vscode: %s, %s" status event)
      (cursorless-receive (with-current-buffer cursorless-socket-buffer
                            (goto-char (point-min)) ;; json-parse-buffer parses forward from point.
                            (json-parse-buffer))))))

(defun cursorless-send (cmd)
  (with-current-buffer cursorless-socket-buffer
    (erase-buffer))
  (let ((p (make-network-process
            :name "cursorless"
            :family 'local
            :remote (expand-file-name "~/.cursorless/vscode-socket")
            :buffer cursorless-socket-buffer
            :sentinel 'cursorless-sentinel)))
    ;; send the command 350ms after the last command was processed (or now).
    ;; this adds a bit of latency to chaining commands, but they work.
    (run-at-time (if cursorless--last-response-processed
                     (+ (- cursorless--last-response-processed (cursorless--time-in-milliseconds)) .35)) nil 'process-send-string p cmd)))

(defun cursorless--apply-selections (selections)
  (when selections
    ;; assume 1 cursor for now.
    (let* ((cursor (elt selections 0))
           (active (gethash "active" cursor))
           (anchor (gethash "anchor" cursor))
           (line   (gethash "line" active))
           (column (gethash "character" active))
           (anchor-line (gethash "line" anchor))
           (anchor-column (gethash "character" anchor))
           (no-selection (and (eql line anchor-line) (eql column anchor-column))))
      ;; Update the selection.
      (unless no-selection
        (goto-char (point-min))
        (forward-line anchor-line)
        (forward-char anchor-column)
        ;; location = (point), nomsg = t
        (push-mark (point) t))
      ;; Update cursor position.
      (cursorless-goto-line-column line column)
      (if no-selection (deactivate-mark)
        (activate-mark t)
        (setq-local transient-mark-mode (cons 'only transient-mark-mode))))))

(defun cursorless--get-buffer-from-temporary-file (temporary-file)
  (-find (lambda (buffer)
           (with-current-buffer buffer
             (and (local-variable-p 'cursorless-temporary-file)
                  (string-equal temporary-file cursorless-temporary-file)))) (buffer-list)))

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
  (cursorless-log (format "receiving response: %s" (cursorless--json-pretty-print (json-encode response))))
  (if-let ((command-exception (gethash "commandException" response)))
      (progn
        (message command-exception)
        (setq cursorless-running-command nil))
    (let* ((new-state (gethash "newState" response))
           (path (gethash "path" new-state))
           (contents-path (gethash "contentsPath" new-state))
           ;; Find the buffer to update.
           (buffer-to-update (cursorless--get-buffer-from-temporary-file path)))
      (if (not buffer-to-update)
          (error "Couldn't find buffer to update, ignoring!"))
      (with-current-buffer buffer-to-update
        ;; Ideally we'd do a diff and then apply the minimal update. Instead I'm
        ;; just going to replace the whole buffer.
        (unless (file-exists-p contents-path) (error "No contents file!"))
        (let ((coding-system-for-read 'utf-8)
              (file-name-handler-alist '()))
          (insert-file-contents contents-path nil nil nil t))
        ;; Update cursor & selection.
        (cursorless--apply-selections (gethash "cursors" new-state))
        (cursorless-log (format "applied response"))
        (setq cursorless-running-command nil)
        ;; This keeps various things up-to-date, eg. hl-line-mode.
        ;; This also runs our send-state function.
        (command-server--write-response cursorless--current-command-uuid)
        (run-hooks 'post-command-hook)
        (setq cursorless--last-response-processed (cursorless--time-in-milliseconds))))))


(provide 'cursorless-command-client)
;;; cursorless-command-client.el ends here
