from talon import Context, actions

ctx = Context()

ctx.matches = r"""
app: emacs
app: Emacs
"""

ctx.tags = ["user.command_client"]


@ctx.action_class("user")
class UserActions:
    def command_server_directory() -> str:
        return "emacs-command-server"

    def trigger_command_server_command_execution():
        # PROBLEM: pressing ctrl-f17 in isearch-mode cancels it :(
        actions.key("ctrl-f17")
