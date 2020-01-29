#!/usr/bin/python3

import json
import templates

with open("node-types.json", 'r') as fs:
    node_types = json.load(fs)

codegen = templates.ast_header

constructors = {}
mod_count = 0
interm_count = 0
bool_init = False

def makeConstructor(b):
    constr = b['type'].lstrip("_") if b['type'].startswith("_") else b['type']
    constr = constr.split("_")[0]
    return constr.capitalize()

def makeConstructor_h(b):
    constr = b.lstrip("_") if b.startswith("_") else b
    constr = constr.split("_")[0]
    return constr.capitalize()

def handleChoice(b):
    constr = makeConstructor(b)
    if constr in constructors.keys():
        constructors[constr] += 1
        constr += str(constructors[constr])
    else:
        constructors[constr] = 0
    global bool_init
    if b['named'] == True:
        if b['type'] not in ['false', 'true']:
            return'''
| {constructor} of {field}'''.format(constructor=constr,field=b['type'])
        elif bool_init == False:
            bool_init = True
            return "\n| Bool of string wrap"
        else:
            return ""
    else:
        return ""

def handleOptionList(a):
    tmp=""
    tmp += " list" if a['multiple'] == True else ""
    tmp += " option" if a['required'] != False else ""
    return tmp

for node in node_types:
    if 'subtypes' in node:
        codegen += '''
and {type} ='''.format(type=node['type'])
        for subtype in node['subtypes']:
            codegen += handleChoice(subtype)
        codegen += "\n"
    
    elif 'fields' in node and len(node['fields']) > 0:
        codegen += '''
and {type} = {{'''.format(type=node['type'])
        intermediates = {}
        for b, bval in node['fields'].items():
            if b == "operator":
                codegen +='''
  operator: string wrap;'''
            else:
                codegen += '''
  {field}:'''.format(field="t_"+ b)
                if bval["multiple"] == False and len(bval['types']) > 1:
                    interm_count += 1
                    codegen += " interm{num}{options};".format(num=interm_count, options=handleOptionList(bval))
                    intermediates["interm" + str(interm_count)] = []
                    for c in bval['types']:
                        intermediates["interm" + str(interm_count)].append(c['type']) 
                else:
                    for c in bval['types']:
                        codegen +=" {param}".format(param=c['type'])
                        codegen += handleOptionList(bval) 
                        codegen += " *"
                    codegen = codegen[:-2] + ";"
            
        if 'children' in node:
            codegen +='''
  mods: mods{num}{option};
}}
  
and mods{num} ='''.format(num=mod_count, option=handleOptionList(node['children']))
            mod_count += 1

            for b in node['children']['types']:
                codegen += handleChoice(b)
            codegen += "\n"
               
        else:
            codegen += "\n}\n"
        
        if len(intermediates) > 0:
            for name, val in intermediates.items():
                codegen +='''
and {field} ='''.format(field=name)
                for choice in val:
                    codegen +='''
| {constructor} of {param}'''.format(constructor=makeConstructor_h(choice),param=choice)
                codegen += "\n"
    
    
    elif 'children' in node: 
            codegen+='''
and {param} ='''.format(param=node['type'])

            if node['children']['multiple'] == False:
                for b in node['children']['types']:
                    codegen += handleChoice(b)
                    codegen += handleOptionList(node['children'])
                codegen += "\n"
            else:
                codegen += "("
                for b in node['children']['types']:
                    codegen += " {param}".format(param=b['type'])
                    codegen += handleOptionList(node['children']) + " *"
                codegen = codegen[:-2] + ")\n"

    
    elif 'named' in node and node['named'] == True and node['type'] not in ['false', 'true']:
        codegen += "\nand {type} = string wrap\n".format(type=node['type'])
    
    else:
        pass


with open("tree_sitter_ast_java_n.ml", "w") as f:
    f.write(codegen)