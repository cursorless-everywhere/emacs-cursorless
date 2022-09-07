from talon import Context, actions, ui, Module, app, clip
from user.knausj.code import dictation

mod = Module()
ctx = Context()
ctx.matches = r'''
os: linux
app: Emacs
app: emacs
'''

INSERT_THRESHOLD = 8

@ctx.action_class('user')
class UserActions:
    def dictation_peek(left, right):
        try:
            return tuple(actions.user.run_rpc_command_get("call", "talon-peek-both"))
        except:
            return actions.next(left, right)

    def insert_between(before, after):
        if len(before) + 2 * len(after) <= INSERT_THRESHOLD:
            return actions.next(before, after)
        try: actions.user.run_rpc_command("call", "talon-insert-between", before, after)
        except: actions.next(before, after)

    # def paste(text):
    #     try: actions.user.run_rpc_command("call", "insert", text)
    #     except: actions.next(text)

    def emacs_command(name, shortname = None):
        try: actions.user.run_rpc_command("call-interactively", name)
        except: actions.next(name, shortname)

@ctx.action_class('main')
class MainActions:
    # FUCK. this doesn't work while isearch-ing D: D: D:
    def insert(text):
        #if len(text) <= INSERT_THRESHOLD: return actions.next(text)
        try: actions.user.run_rpc_command("call", "talon-insert", text)
        except: actions.next(text)


