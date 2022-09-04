app: emacs
app: Emacs
-

emacs talon enable: user.emacs_command("talon-enable")
emacs talon disable: user.emacs_command("talon-disable")

show hats: user.emacs_command("show-hats")
hide hats: user.emacs_command("hide-hats")
(cursor less | cursorless) start [sync]: user.emacs_command("cursorless-enable-sync")
(cursor less | cursorless) stop [sync]: user.emacs_command("cursorless-disable-sync")
command server start: user.emacs_command("command-server-start")
command server (stop|quit): user.emacs_command("command-server-quit")
command server trigger: key(ctrl-f17)

emacs rpc test:
  mode = user.run_rpc_command_get("eval", "isearch-mode")
  bufname = user.run_rpc_command_get("eval", "(buffer-name (current-buffer))")
  print("buffer name: {bufname}")
  print("mode: {mode}")
