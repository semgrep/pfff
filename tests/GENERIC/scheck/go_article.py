# adaptation of https://blog.trailofbits.com/2019/11/07/attacking-go-vr-ttps/ 
# for Python

def A():
  return (False, "A")

def B():
  return (True, "B")

def main():
  aSuccess, err = A()
  bSuccess, err = B()
  if err is not None:
    print(err)
  print(aSuccess, ":", bSuccess)

main()



