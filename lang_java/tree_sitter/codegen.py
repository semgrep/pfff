#!/usr/bin/python3

import json

with open("node-types.json", 'r') as fs:
    data = json.load(fs)

codegen = '''
(* 
 * Yoann Padioleau, Sharon Lin
 * 2020 initial draft
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * An AST for Java.
 *
 *)
 (*****************************************************************************)
(* The Tree-sitter AST java related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type 'a wrap  = 'a * string
'''

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

for a in data:
    if 'subtypes' in a:
        codegen += '''
and {type} ='''.format(type=a['type'])
        for b in a['subtypes']:
            codegen += handleChoice(b)
        codegen += "\n"
    
    elif 'fields' in a and len(a['fields']) > 0:
        codegen += '''
and {type} = {{'''.format(type=a['type'])
        intermediates = {}
        for b, bval in a['fields'].items():
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
            
        if 'children' in a:
            codegen +='''
  mods: mods{num}{option};
}}
  
and mods{num} ='''.format(num=mod_count, option=handleOptionList(a['children']))
            mod_count += 1

            for b in a['children']['types']:
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
    
    
    elif 'children' in a: 
            codegen+='''
and {param} ='''.format(param=a['type'])

            if a['children']['multiple'] == False:
                for b in a['children']['types']:
                    codegen += handleChoice(b)
                    codegen += handleOptionList(a['children'])
                codegen += "\n"
            else:
                codegen += "("
                for b in a['children']['types']:
                    codegen += " {param}".format(param=b['type'])
                    codegen += handleOptionList(a['children']) + " *"
                codegen = codegen[:-2] + ")\n"

    
    elif 'named' in a and a['named'] == True and a['type'] not in ['false', 'true']:
        codegen += "\nand {type} = string wrap\n".format(type=a['type'])
    
    else:
        pass


with open("tree_sitter_ast_java_n.ml", "w") as f:
    f.write(codegen)