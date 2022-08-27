import json

from talon import Context, actions, clip

from user.knausj.apps.vscode.command_client import command_client
NotSet = command_client.NotSet

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

    # run_rpc_command('cursorless.command',
    #                 {'version': 1,
    #                  'spokenForm': 'pre two',
    #                  'action': 'setSelectionBefore',
    #                  'targets': [{'type': 'primitive',
    #                               'mark': {'type': 'decoratedSymbol', 'symbolColor': 'default', 'character': '2'}}],
    #                  'extraArgs': [],
    #                  'usePrePhraseSnapshot': False})

    def run_rpc_command_and_wait(command_id, arg1 = None, arg2 = None, arg3 = None, arg4 = None, arg5 = None):
        # TODO: invoke M-x or M-: to run this command in emacs
        args = [x for x in (arg1, arg2, arg3, arg4, arg5) if x is not NotSet]
        print(f"run_rpc_command({command_id!r}, {', '.join(map(str, args))})")
        if command_id == 'cursorless.command':
            assert len(args) == 1
            payload = {"command": "cursorless", "cursorlessArgs": json.dumps(args)}
            actions.key("alt-:")
            with clip.revert():
                clip.set_text(json.dumps(payload))
                actions.sleep("300ms")
                # FIXME: cursorless-send needs to be synchronous
                # also I'm not sure this works reliably.
                actions.insert("(cursorless-send (gui-get-selection 'CLIPBOARD))")
                actions.key("enter")
                actions.sleep("300ms")
            
            
