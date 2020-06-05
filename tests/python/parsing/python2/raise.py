try:
    foo()
except Exception, ex:
    print ex

try:
    foo()
except Exception as ex:
    print ex
