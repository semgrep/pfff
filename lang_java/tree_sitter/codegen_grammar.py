#!/usr/bin/python3

import json
import templates

with open("grammar.json", 'r') as fs:
    data = json.load(fs)

codegen = templates.ast_header

PREC = ['PREC_LEFT', 'PREC_RIGHT', 'PREC_DYNAMIC']

# Finding type from field

def findType(dict):
    type = dict['type']
    if type == "FIELD":
        return handleField(dict)
    elif type == "TOKEN":
        return handleToken(dict)
    elif type == "STRING":
        return handleString(dict)
    elif type == "SEQ":
        return handleSeq(dict)
    elif type == "CHOICE":
        return handleChoice(dict)
    elif type in ["REPEAT", 'REPEAT1']:
        return handleRepeat(dict)
    elif type == "SYMBOL":
        return handleSymbol(dict)
    elif type == "PATTERN":
        return ""
    elif type == "PREC":
        return handlePrec(dict)
    elif type in PREC:
        return handlePrecDir(dict)
    elif type == "BLANK":
        return handleBlank(dict)
    else:
        return ""

# Handling types 

def handleField(dict):
    localcode = ""
    localcode+="  {field}: ".format(field=dict['name'])
    if dict['name'] == 'operator':
        return localcode + "string wrap;\n"
    else:
        for a, aval in dict['content'].items():
            if a == 'name':
                localcode+='''{param}'''.format(field=dict['name'],param=aval)
            elif a == "members":
                for b in aval:
                    if b['type'] != "BLANK":
                        localcode+="{param} * ".format(param=b['name']) if 'name' in b else "{param} * ".format(param=b['value'])
                localcode = localcode[:-2]
        return localcode.strip() + ";\n"

# string, leaf
def handleToken(dict):
    tmp = ""
    for a, aval in dict['content'].items():
        if a == "members":
            for b in aval:
                tmp += findType(b)
    return tmp

# synatax, we don't care about these
def handleString(dict):
    return ""

# synatax, we don't care about these
def handleBlank(dict):
    return "BLANK"

# tuple (type x = A * B * C ...)
def handleSeq(dict): 
    tmp = ""
    for a in dict['members']:
        tmp += findType(a) 
    return tmp  # + ";\n"

# union type (type x = A ... | B ... | C ...)
def handleChoice(dict):
    tmp = ""
    for a in dict['members']:
        if len(findType(a)) > 0:
            tmp += "\n| {field}".format(field=findType(a))
    return tmp

# list (a list)
def handleRepeat(dict):
    tmp = "("
    if 'members' in dict['content'].items():
        for a in dict['content']['members']:
            tmp += findType(a)
    else:
        tmp += findType(dict['content'])
    return tmp + " list)"

# leaf
def handleSymbol(dict):
    return " {param}".format(param=dict['name'])

def handlePrec(dict):
    return findType(dict['content'])

def handlePrecDir(dict):
    return " string wrap"

def makeConstructor(b):
    if 'name' in b:
        constr = b['name'].lstrip("_") if b['name'].startswith("_") else b['name']
    else:
        constr = b['value'].lstrip("_") if b['value'].startswith("_") else b['value']
    constr = constr.split("_")[0]
    return constr.capitalize()

def removeToken(b):
    return b[:-2] if b[-2] in ['|', '*'] else b

# Handling top-level types 

for a, aval in data["rules"].items():
    if aval["type"] == "CHOICE":
        codegen += "\nand {type} =\n".format(type=a)
        for b in aval["members"]:
            if b["type"] not in IGNORE_TYPES:
                codegen += "| {constructor} of {field}\n".format(constructor=makeConstructor(b),field=b['name'])
            else:
                codegen += '''| {field}\n'''.format(field=findType(b))
        codegen += "\n"



    elif aval["type"] == "SEQ":
        codegen +="\nand {type} = {{\n".format(type=a) 
        for b in aval["members"]:
            codegen += findType(b)
        codegen += "\n}\n"



    elif aval["type"] == "ALIAS":
        codegen += "\nand {type} = {val}\n".format(type=a,val=aval['value'])

    elif aval["type"] in ['TOKEN', 'STRING']:
        codegen += "\nand {type} = string wrap\n".format(type=a)

    elif aval["type"] in ["PREC", "PREC_RIGHT"]:
        if aval['content']['type'] == 'CHOICE':
            codegen += "\nand {type} =\n".format(type=a)
            codegen += findType(aval["content"])
        elif aval['content']['type'] in ['SEQ','REPEAT1']:
            codegen += "\nand {type} = {{\n".format(type=a)
            codegen += findType(aval["content"])
            codegen += "}\n"
        else:
            pass

    elif aval["type"] in PREC:
        codegen += "\nand {type} =\n".format(type=a)
        codegen += findType(aval)

    elif aval["type"] == "REPEAT1":
        codegen += "\nand {type} =\n".format(type=a)
        for b, bval in aval["content"].items():
            if b == "members":
                for c in bval:
                    field_n = c['name'] if 'name' in c else c['value']
                    codegen += "\n| {constructor} of {field}".format(constructor=makeConstructor(c), field=field_n)
        codegen += "\n"

    else:
        pass


with open("tree_sitter_ast_java_grammar.ml", "w") as f:
    f.write(codegen)