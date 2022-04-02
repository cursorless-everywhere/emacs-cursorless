- serialize editor state to file, at the moment:
  - a serial number
  - current file path
  - top & bottom visible lines
    edge case: long lines without wrapping, does cursorless hat them?
    looks like it does.
  - where the cursors/selections are

- watch file from talon or vscode
- vscode draws hats and serializes to a file
- editor watches hat-file and draws hats

problem: how to draw hats in emacs? what if hats are out of date wrt file?

- issue a cursorless command
- goes to vscode via a command server
- vscode edits the file and saves, tells talon it's done
- editor either watches the file or is told to reload

TODO: use temp file instead of actual file

problem: synchrony problems when chaining between normal and cursorless commands?

eg. "air bat cap <cursorless command> drum each fine"

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

