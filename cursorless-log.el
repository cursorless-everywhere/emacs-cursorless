;;; cursorless-log.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;  Description
;;
;;; Code:

(require 'json)
(require 's)

(defconst cursorless-log-buffer "*cursorless-log*")

(defun cursorless--json-pretty-print (s)
  (with-temp-buffer
    (insert s)
    (json-pretty-print-buffer)
    (buffer-string)))

(defun cursorless--truncate-log-buffer ()
  (interactive)
  (save-excursion
    (with-current-buffer (get-buffer-create cursorless-log-buffer)
      (goto-char (point-max))
      (forward-line (- 5000))
      (beginning-of-line)
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point))))))

(defun cursorless-log (message)
  (cursorless--truncate-log-buffer)
  (let ((buffer-logged-from (current-buffer))
        (line-col (cursorless-line-and-column (point))))
    (with-current-buffer (get-buffer-create cursorless-log-buffer)
      (goto-char (point-max))
      (insert (make-string 70 ?=) "\n"
              (format-time-string "%s.%3N") "\n"
              (format "  current-buffer: %S" buffer-logged-from) "\n"
              (format "  line/col: %S" line-col) "\n"
              (s-join "\n" (-map (lambda(value)
                                   (format "  %20s: %S" value (symbol-value value)))
                                 '(cursorless-running-command cursorless-updating-hats cursorless--last-response-processed))) "\n\n"
              "  " message "\n")
      (goto-char (point-max))
      (let ((windows (get-buffer-window-list (current-buffer) nil t)))
        (while windows
          (set-window-point (car windows) (point-max))
          (setq windows (cdr windows)))))))

(provide 'cursorless-log)
;;; cursorless-log.el ends here
