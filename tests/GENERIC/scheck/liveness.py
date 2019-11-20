def basic_liveness():
  x = 1
  print(x)

  y = 1
  y = y + 1
  print(y)

  useless_z = 1
  #ERROR: useless assignement
  useless_z = useless_z + 1


def test_underscore():
  # using _ means we know this is unused and should not be an error
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
  obj[1] = 1
  obj[2] = 2
  print(obj[2])

def test_foreach():
  a = []
  for i in a:
    print (i)



#def test_with():
#  a = 1
#  ctx = 2
#  with ctx:
#     print(a)

