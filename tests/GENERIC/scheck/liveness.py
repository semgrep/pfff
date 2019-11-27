def basic_liveness():
  x = 1
  print(x)

  y = 1
  y = y + 1
  print(y)

  useless_z = 1
  #ERROR: useless assignement; the value is not used after
  useless_z = useless_z + 1


def test_underscore():
  # using _ means we know this is unused and should not be reported as an error
  a, _ = bar()
  if a:
     return 1
  else:
     return 2

def test_objaccess():
  # obj is actually not an lvalue, obj.fld1 is, so we should not
  # report a useless assignement here
  obj.fld1 = 1
  obj.fld2 = 2
  print(obj.fld1)

def test_arraccess():
  obj = []
  # same here
  obj[1] = 1
  obj[2] = 2
  print(obj[2])

def test_foreach():
  a = []
  # lrvalue.ml must understand foreach loops
  for i in a:
    print (i)


def test_with():
  a = 1
  ctx = 2
  # the CFG must understand with statements
  with ctx:
     print(a)

def test_call_comprehension():
  client = bar()
  any(i for i in client)

def test_call_comprehension2():
  client = bar()
  # ERRORTODO: it should report that one!
  any(0 for i in client)


def test_global():
  global foo
  # this is ok
  foo = 1

def test_lambda():
  a = 1
  iter(lambda c: a + c)


def test_nested_function():
  a = 1
  def bar():
      return a
  bar()

def test_nested_function2():
  a = 1
  def foo():
    b = 2
    return a+b
  foo(2)
