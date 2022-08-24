- issue a cursorless command
- goes to emacs (HOW?)
- emacs forwards to vscode through socket
- vscode does it, returns state to emacs
- emacs updates state, tells talon it's done (HOW)

reason for this weird ping-pong: synchronisation. makes sure everything goes
through my editor, thus no race conditions.

PROBLEM: how to get talon to wait for emacs to be done processing command?
SOLUTION: I dunno, make emacs write a file?
maybe I want an emacs command-server implementation!

# NEXT STEPS

talon -> emacs -> vscode -> emacs -> talon

a. get talon talking synchronously to emacs (command server? something simpler?)
b. (MOSTLY DONE)
   get emacs talking real cursorless commands to vscode & listening back
   we get back the cursor position & path to temp file w/ file contents

# SAMPLE CODE
(setq buf (get-buffer "asdf"))
(setq p (make-network-process :name "test" :family 'local
                              :remote "/home/rntz/.cursorless/vscode-socket"
                              :buffer buf
                              :sentinel 'my-sentinel))
(process-send-string p "{\"command\": \"state\"}")
(defun my-sentinel (proc event)
  (message "magic-sentinel: %s(%s) %s" proc (process-status proc) event))

;; see also
(accept-process-output p 1) ; semi-blocking interface
(all-threads) ; emacs has (cooperative) threads! could use them?


# MY NEXT STEPS

NEXT:
- get emacs writing its state
  - get-state function mostly done
  - use hooks to write on each change
  - probably want serial numbers

phil sez: get emacs writing its state first

- install vscode & cursorless (DONE?)
- dummy-prototype rendering half in emacs:
  throw overlays on text

- get dumping state to file working
  what hooks will this need:
  - buffer contents change: after-change-functions
    https://www.gnu.org/software/emacs/manual/html_node/elisp/Change-Hooks.html

  - view changes
  - current buffer changes
  - cursor/selection change
    post-command-hook?

