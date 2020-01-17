import bblfsh
import sys
import json

client = bblfsh.BblfshClient("localhost:9432")
ctx = str(client.parse(str(sys.argv[1])))
print(ctx)