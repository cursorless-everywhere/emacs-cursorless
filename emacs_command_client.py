import json

from talon import Context, actions, clip

# from user.knausj.apps.vscode.command_client import command_client
# NotSet = command_client.NotSet

ctx = Context()

ctx.matches = r"""
app: emacs
app: Emacs
"""

ctx.tags = ["user.command_client"]


@ctx.action_class("user")
class UserActions:
    def command_server_directory() -> str:
        return "emacs-command-server" # unused/unimplemented for now

    def trigger_command_server_command_execution():
        actions.key("ctrl-f17")
