# lots of FPs

a  = ()

# this introduce a newvar: even if what seems an rvalue context
for k in foo():
  # this is not an lvalue for k!
  a[k] = 1

