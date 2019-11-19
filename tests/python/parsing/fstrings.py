# https://www.python.org/dev/peps/pep-0498/
# https://realpython.com/python-f-strings/

def foo():
  a = 1
  s = f"this is {a}"
  print (s)

foo()

def test():
   foo(f"this is {1+2 if True else 4}")
   echo_error(f"Error while running {tool_id}: {findings}")
   # what is that?
   echo_warning(f"{len(collapsed_findings)} findings in {elapsed:.2f} s\n")

   click.secho(
                f"""
You  error in `{bento.constants.DEFAULT_LOG_PATH}`.
""",
                err=True,
            )
