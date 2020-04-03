global_ = "global"

def foo(param1, param2):
  local = "local"
  # actually a new local!
  global_ = "local"
  return local
  return param1

def foo2():
  global global_
  # now it references the global
  global_ = "global2"
  return global_


def foo3(x, y):
   def bar(x):
     nonlocal y
     return x+y
   yield bar(x)
