# https://www.python.org/dev/peps/pep-0498/
# https://realpython.com/python-f-strings/

def foo():
  a = 1
  s = f"this is {a}"
  print (s)

foo()

def test():
   echo_error(f"Error while running {tool_id}: {findings}")
   # what is that?
   echo_warning(f"{len(collapsed_findings)} findings in {elapsed:.2f} s\n")

   click.secho(
                f"""-------------------------------------------------------------------------------------------------
This may be due to a corrupted tool installation. You might be able to fix this issue by running:

  bento init --clean

You can also view full details of this error in `{bento.constants.DEFAULT_LOG_PATH}`.
-------------------------------------------------------------------------------------------------
""",
                err=True,
            )
