
def test_dead_after_return():
  a = 1
  echo(a)
  return 1
  #ERROR: dead statement
  echo(a)


def test_dead_after_exn():
  # we have a few code like this. It's an error but a less important one
  raise Exception()
  #ERROR: dead statement
  return 1

# no switch in Python, see cfg.c

def test_dead_ifdef_like():
  return null
  
  #ERROR: less important; people abuse return as some kind of #if 0
  a = 1
  return a


#def test_dynamic_break():
#  while(1):
#    x = 1
#    #ERRORX: dynamic break
#    break x
  

#def test_dynamic_break_ok():
#  while(1):
#    #this is ok
#    break 1
#  
#
#  while(1):
#    #this is ok too
#    break (1)
  

def test_for_continue_ok():
  # this is ok, there is no deadcode, the i++ is reached by the 'continue'
  for i in []:
    if(i == 1):
      continue
    
    return 1
