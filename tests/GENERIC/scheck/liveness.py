def foo():
  x = 1
  print(x)

  y = 1
  y = y + 1
  print(y)

  z = 1
  #ERROR: useless assignement
  z = z + 1


def foo2():
  # using _ means we know this is unused and should not be an error
  a, _ = bar()
  if a:
     return 1
  else:
     return 2
