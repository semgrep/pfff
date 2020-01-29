import json

with open("grammar.json", 'r') as fs:
    data = json.load(fs)

codegen = '''
(* Yoann Padioleau, Sharon Lin
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

open Ast_java
module J = Json_type
module PI = Parse_info

(*****************************************************************************)
(* Call external program *)
(*****************************************************************************)
let json_of_filename_with_external_prog _file =
  (* should call tree-sitter or babelfish on _file, generate
   * temporary JSON output, and read this JSON output
   * (or use a pipe to the external program to avoid using an
   *  intermediate temporary file)
   *)
  let lines = "node ../tree-sitter/tree-sitter-parser.js " ^ _file ^ " > results.json"
  Sys.command lines
  raise Todo

(*****************************************************************************)
(* JSON boilerplate (we should find a way to generate this code) *)
(*****************************************************************************)

let error str json =
  pr2 (spf "bad json: was expecting a %s (got %s)" str 
        (Json_io.string_of_json json));
  failwith "BAD JSON"

let todo str json = 
  pr2 (spf "TODO: in %s need to handle this JSON: %s" str 
    (Json_io.string_of_json json));
  failwith "TODO"

(* this can not be in the recursive 'let rec ... and ...' below because
 * of weird OCaml restrictions too long to explain
 *)
let list str f = function
    | J.Array xs -> List.map f xs
    | J.String "None" -> []
    | x -> error (spf "list[%s]" str) x
let option str _f = function
    | J.String "None" -> None
    | x -> todo (spf "option[%s]" str) x

let wrap str = 
  let tok = Parse_info.fake_info str in
  str, tok

'''

interm_count = 0
PREC = ['PREC_LEFT', 'PREC_RIGHT', 'PREC_DYNAMIC']

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
    elif type == "REPEAT":
        return handleRepeat(dict)
    elif type == 'REPEAT1':
          return handleRepeat1(dict)
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
        
#################################################
#
# Handling types 
#
#################################################

def handleField(dict):
  tmp = ""
  for a in dict['content']:
    tmp += '''
    {field}: {params};
    '''.format(field=dict['name'],params=dict['name'])
  return tmp

# string, leaf
def handleToken(dict):
  return ""

# synatax, we don't care about these
def handleString(dict):
    return ""

# synatax, we don't care about these
def handleBlank(dict):
    return ""

# tuple (type x = A * B * C ...)
def handleSeq(dict): 
  tmp = ""
  for a in dict['members']:
    if 'name' in a:
      tmp += '''
    {field}: {param} params;
    '''.format(field=a['name'], param=findType(a))
  return tmp 

# union type (type x = A ... | B ... | C ...)
def handleChoice(dict):
  global interm_count
  tmp = '''
   | J.Object [
     "type", J.String "{type}";
     "startPosition", _;
      "endPosition", _;
     "children", params
    ] -> Some (interm{num} params) 

    (and interm{num} = '''.format(type=dict['type'], num=interm_count)
  for a in dict["members"]:
    tmp += findType(a)
  tmp +='''
    )'''
  interm_count += 1
  return tmp

# list (a list)
def handleRepeat(dict):
  tmp = ""
  for a in dict['content']:
    print(dict)
    tmp += findType(a)
  return tmp

def handleRepeat1(dict):
  tmp = ""
  tmp += findType(dict['content'])
  return tmp

# leaf
def handleSymbol(dict):
  return '''
| J.Object [
    "type", J.String "{type}";
    "startPosition", t_start;
    "endPosition", t_end;
    "children", J.List [];
  ] -> wrap t_start t_end
'''.format(type=dict['name'])

def handlePrec(dict):
    return findType(dict['content'])

def handlePrecDir(dict):
  return '''
| J.Object [
    "type", J.String "{type}";
    "startPosition", t_start;
    "endPosition", t_end;
    "children", J.List [];
  ] -> wrap t_start t_end
'''.format(type=dict['type'])

#################################################
#
# Higher levels in the JSON
#
#################################################

def handleProgram(dict):
    return '''
let program_of_tree_sitter_json _file json = 
  let rec program = function
  | J.Object [
    "type", J.String "program";
    "startPosition", _;
    "endPosition", _;
    "children", J.Object xs;
  ] -> { package = Some (package xs);
         imports = Some (imports xs);
         decls = Some (decls xs) }
  | x -> error "program" x
'''

def handleGeneric(a, aval):
  return '''
  and {type} = function
   | J.Object [
     "type", J.String "{type}";
     "startPosition", _;
      "endPosition", _;
     "children", params
    ] -> params
   | x -> error "{type}" x
'''.format(type=a)

def handleRepeat1_h(a, aval):
  tmp = ""
  for b in aval['content']['members']:
    if 'name' in b:
      tmp += '''
  and {type} = function
   | J.Object [
     "type", J.String "{type}";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _
    ] -> wrap t_start t_end
   | x -> error "{type}" x
'''.format(type=b['name'])
  return tmp

def handleChoice_h(a, aval):
  global interm_count
  tmp = '''
  and {type} = function
   | J.Object [
     "type", J.String "{type}";
     "startPosition", _;
      "endPosition", _;
     "children", params
    ] -> Some (interm{num} params) 
   | x -> error "{type}" x

   and interm{num} = function
'''.format(type=a, num=interm_count)
  for a in aval["members"]:
    tmp += findType(a)
  interm_count += 1
  return tmp

def handleSeq_h(a, aval):
  global interm_count
  tmp = '''
  and {type} = function
   | J.Object [
     "type", J.String "{type}";
     "startPosition", _;
      "endPosition", _;
     "children", params
    ] -> {{'''.format(type=a)

  for b in aval['members']:
    if 'name' in b:
      tmp += '''
      {field}: {field} params;'''.format(field=b['name'])
    elif 'members' in b:
      tmp += findType(b)
          
  tmp += '''
    }}
   | x -> error "{type}" x
'''.format(type=a)
  return tmp

def handleToken_h(a, aval):
      return '''
  and {type} = function
   | J.Object [
     "type", J.String "{type}";
     "startPosition", t_start;
      "endPosition", t_end;
     "children", _;
    ] -> wrap t_start t_end
   | x -> error "{type}" x
'''.format(type=a)

def handlePrec_h(a, aval):
  global interm_count
  tmp = '''
  and {type} = function
   | J.Object [
     "type", J.String "{type}";
     "startPosition", _;
      "endPosition", _;
     "children", params
    ] -> {{'''.format(type=a)
   
  if 'members' in aval['content']:
    for b in aval['content']['members']:
      tmp += findType(b)
          
  tmp += '''
    }}
   | x -> error "{type}" x
'''.format(type=a)
  return tmp


#################################################
#
# Parsing the JSON 
#
#################################################


for a, aval in data["rules"].items():
    if a == "program":
      codegen += handleProgram(aval)
    else:
      if aval['type'] == 'CHOICE':
        codegen += handleChoice_h(a, aval)
      elif aval['type'] == 'SEQ':
        codegen += handleSeq_h(a, aval)
      elif aval['type'] == 'REPEAT1':
        codegen += handleRepeat1_h(a, aval)
      elif aval['type'] in ['PREC','PREC_LEFT','PREC_RIGHT', 'PREC_DYNAMIC']:
        codegen += handlePrec_h(a, aval)
      elif aval['type'] in ['TOKEN', 'ALIAS', 'STRING', 'PATTERN']:
        codegen += handleToken_h(a, aval)
      else: 
        codegen += handleGeneric(a, aval)


codegen += '''
(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse file = 
  let json = json_of_filename_with_external_prog file in
  (* to decide wether this or program_of_babelfish_json *)
  let program = program_of_tree_sitter_json file json in
  program
'''

with open("parse_with_external_n.ml", "w") as f:
    f.write(codegen)