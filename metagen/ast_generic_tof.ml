(* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_tof.cmo  pr_o.cmo /tmp/xxx.ml  *)

let tof_tok = OCaml.add_new_type "tok" (OCaml.TTODO "")

let tof_wrap =
  OCaml.add_new_type "wrap" (OCaml.Tuple [ OCaml.Poly "a"; OCaml.Var "tok" ])

let tof_name =
  OCaml.add_new_type "name" (OCaml.Apply ("wrap", OCaml.String))

let tof_dotted_name =
  OCaml.add_new_type "dotted_name" (OCaml.List (OCaml.Var "name"))

let tof_qualified_name =
  OCaml.add_new_type "qualified_name" (OCaml.Var "dotted_name")

let tof_module_name =
  OCaml.add_new_type "module_name"
    (OCaml.Sum
       [ ("FileName", [ OCaml.Apply ("wrap", OCaml.String) ]);
         ("DottedName", [ OCaml.Var "dotted_name" ]) ])

let tof_resolved_name =
  OCaml.add_new_type "resolved_name"
    (OCaml.Sum
       [ ("Local", []); ("Param", []);
         ("Global", [ OCaml.Var "qualified_name" ]); ("NotResolved", []);
         ("Macro", []); ("EnumConstant", []); ("ImportedModule", []) ])

let tof_xml = OCaml.add_new_type "xml" (OCaml.List (OCaml.Var "any"))

let tof_any =
  OCaml.add_new_type "any"
    (OCaml.Sum
       [ ("N", [ OCaml.Var "name" ]); ("En", [ OCaml.Var "entity" ]);
         ("E", [ OCaml.Var "expr" ]); ("S", [ OCaml.Var "stmt" ]);
         ("T", [ OCaml.Var "type_" ]); ("P", [ OCaml.Var "pattern" ]);
         ("D", [ OCaml.Var "definition" ]);
         ("Di", [ OCaml.Var "directive" ]); ("I", [ OCaml.Var "item" ]);
         ("Pa", [ OCaml.Var "parameter" ]); ("Ar", [ OCaml.Var "argument" ]);
         ("At", [ OCaml.Var "attribute" ]);
         ("Dk", [ OCaml.Var "definition_kind" ]);
         ("Pr", [ OCaml.Var "program" ]) ])


and tof_program =
  OCaml.add_new_type "program" (OCaml.List (OCaml.Var "item"))
and tof_item =
  OCaml.add_new_type "item"
    (OCaml.Sum
       [ ("IStmt", [ OCaml.Var "stmt" ]);
         ("IDef", [ OCaml.Var "definition" ]);
         ("IDir", [ OCaml.Var "directive" ]) ])
and tof_other_directive_operator =
  OCaml.add_new_type "other_directive_operator"
    (OCaml.Sum
       [ ("OI_Export", []); ("OI_ImportCss", []); ("OI_ImportEffect", []);
         ("OI_Package", []); ("OI_Define", []); ("OI_Macro", []);
         ("OI_Prototype", []); ("OI_Namespace", []) ])
and tof_alias =
  OCaml.add_new_type "alias"
    (OCaml.Tuple [ OCaml.Var "name"; OCaml.Option (OCaml.Var "name") ])
and tof_directive =
  OCaml.add_new_type "directive"
    (OCaml.Sum
       [ ("Import",
          [ OCaml.Var "module_name"; OCaml.List (OCaml.Var "alias") ]);
         ("ImportAll",
          [ OCaml.Var "module_name"; OCaml.Option (OCaml.Var "name") ]);
         ("OtherDirective",
          [ OCaml.Var "other_directive_operator";
            OCaml.List (OCaml.Var "any") ]) ])
and tof_class_kind =
  OCaml.add_new_type "class_kind"
    (OCaml.Sum [ ("Class", []); ("Interface", []); ("Trait", []) ])
and tof_class_definition =
  OCaml.add_new_type "class_definition"
    (OCaml.Dict
       [ ("ckind", `RO, (OCaml.Var "class_kind"));
         ("cextends", `RO, (OCaml.List (OCaml.Var "type_")));
         ("cimplements", `RO, (OCaml.List (OCaml.Var "type_")));
         ("cbody", `RO, (OCaml.List (OCaml.Var "field"))) ])
and tof_other_type_definition_operator =
  OCaml.add_new_type "other_type_definition_operator"
    (OCaml.Sum [ ("OTDO_Struct", []); ("OTDO_Union", []); ("OTDO_Enum", []) ])
and tof_constructor_definition =
  OCaml.add_new_type "constructor_definition"
    (OCaml.Tuple [ OCaml.Var "name"; OCaml.List (OCaml.Var "type_") ])
and tof_other_type_kind_operator =
  OCaml.add_new_type "other_type_kind_operator"
    (OCaml.Sum [ ("OTKO_EnumWithValue", []) ])
and tof_type_definition_kind =
  OCaml.add_new_type "type_definition_kind"
    (OCaml.Sum
       [ ("OrType", [ OCaml.List (OCaml.Var "constructor_definition") ]);
         ("AndType", [ OCaml.List (OCaml.Var "field") ]);
         ("AliasType", [ OCaml.Var "type_" ]);
         ("OtherTypeKind",
          [ OCaml.Var "other_type_kind_operator";
            OCaml.List (OCaml.Var "any") ]) ])
and tof_type_definition =
  OCaml.add_new_type "type_definition"
    (OCaml.Dict
       [ ("tbody", `RO, (OCaml.Var "type_definition_kind"));
         ("tother", `RO, (OCaml.Var "other_type_definition_operator")) ])
and tof_field =
  OCaml.add_new_type "field"
    (OCaml.Sum
       [ ("FieldVar",
          [ OCaml.Var "entity"; OCaml.Var "variable_definition" ]);
         ("FieldMethod",
          [ OCaml.Var "entity"; OCaml.Var "function_definition" ]);
         ("FieldDynamic",
          [ OCaml.Var "expr"; OCaml.List (OCaml.Var "attribute");
            OCaml.Var "expr" ]);
         ("FieldSpread", [ OCaml.Var "expr" ]);
         ("FieldStmt", [ OCaml.Var "stmt" ]) ])
and tof_variable_definition =
  OCaml.add_new_type "variable_definition"
    (OCaml.Dict
       [ ("vinit", `RO, (OCaml.Option (OCaml.Var "expr")));
         ("vtype", `RO, (OCaml.Option (OCaml.Var "type_"))) ])
and tof_other_parameter_operator =
  OCaml.add_new_type "other_parameter_operator"
    (OCaml.Sum [ ("OPO_KwdParam", []); ("OPO_Ref", []) ])
and tof_parameter_classic =
  OCaml.add_new_type "parameter_classic"
    (OCaml.Dict
       [ ("pname", `RO, (OCaml.Var "name"));
         ("pdefault", `RO, (OCaml.Option (OCaml.Var "expr")));
         ("ptype", `RO, (OCaml.Option (OCaml.Var "type_")));
         ("pattrs", `RO, (OCaml.List (OCaml.Var "attribute"))) ])
and tof_parameter =
  OCaml.add_new_type "parameter"
    (OCaml.Sum
       [ ("ParamClassic", [ OCaml.Var "parameter_classic" ]);
         ("ParamPattern", [ OCaml.Var "pattern" ]);
         ("OtherParam",
          [ OCaml.Var "other_parameter_operator";
            OCaml.List (OCaml.Var "any") ]) ])
and tof_parameters =
  OCaml.add_new_type "parameters" (OCaml.List (OCaml.Var "parameter"))
and tof_function_definition =
  OCaml.add_new_type "function_definition"
    (OCaml.Dict
       [ ("fparams", `RO, (OCaml.Var "parameters"));
         ("frettype", `RO, (OCaml.Option (OCaml.Var "type_")));
         ("fbody", `RO, (OCaml.Var "stmt")) ])
and tof_type_parameter_constraint =
  OCaml.add_new_type "type_parameter_constraint"
    (OCaml.Sum [ ("Extends", [ OCaml.Var "type_" ]) ])
and tof_type_parameter_constraints =
  OCaml.add_new_type "type_parameter_constraints"
    (OCaml.List (OCaml.Var "type_parameter_constraint"))
and tof_type_parameter =
  OCaml.add_new_type "type_parameter"
    (OCaml.Tuple [ OCaml.Var "name"; OCaml.Var "type_parameter_constraints" ])
and tof_definition_kind =
  OCaml.add_new_type "definition_kind"
    (OCaml.Sum
       [ ("FuncDef", [ OCaml.Var "function_definition" ]);
         ("VarDef", [ OCaml.Var "variable_definition" ]);
         ("ClassDef", [ OCaml.Var "class_definition" ]);
         ("TypeDef", [ OCaml.Var "type_definition" ]);
         ("ModuleDef", [ OCaml.Var "module_definition" ]);
         ("MacroDef", [ OCaml.Var "macro_definition" ]);
         ("Signature", [ OCaml.Var "type_" ]);
       ])

and tof_macro_definition =
  OCaml.add_new_type "macro_definition"
    (OCaml.Dict
       [ ("macroparams", `RO, (OCaml.List (OCaml.Var "ident")));
         ("macrobody", `RO, (OCaml.List (OCaml.Var "any"))) ])
and tof_other_module_operator =
  OCaml.add_new_type "other_module_operator"
    (OCaml.Sum [ ("OMO_Functor", []) ])
and tof_module_definition_kind =
  OCaml.add_new_type "module_definition_kind"
    (OCaml.Sum
       [ ("ModuleAlias", [ OCaml.Var "name" ]);
         ("ModuleStruct",
          [ OCaml.Option (OCaml.Var "dotted_ident");
            OCaml.List (OCaml.Var "item") ]);
         ("OtherModule",
          [ OCaml.Var "other_module_operator"; OCaml.List (OCaml.Var "any") ]) ])
and tof_module_definition =
  OCaml.add_new_type "module_definition"
    (OCaml.Dict [ ("mbody", `RO, (OCaml.Var "module_definition_kind")) ])

and tof_entity =
  OCaml.add_new_type "entity"
    (OCaml.Dict
       [ ("name", `RO, (OCaml.Var "name"));
         ("attrs", `RO, (OCaml.List (OCaml.Var "attribute")));
         ("type_", `RO, (OCaml.Option (OCaml.Var "type_")));
         ("tparams", `RO, (OCaml.List (OCaml.Var "type_parameter"))) ])
and tof_definition =
  OCaml.add_new_type "definition"
    (OCaml.Tuple [ OCaml.Var "entity"; OCaml.Var "definition_kind" ])
and tof_other_pattern_operator =
  OCaml.add_new_type "other_pattern_operator"
    (OCaml.Sum [ ("OP_Expr", []); ("OP_Var", []) ])

and tof_field_pattern =
  OCaml.add_new_type "field_pattern"
    (OCaml.Tuple [ OCaml.Var "name"; OCaml.Var "pattern" ])

and tof_pattern =
  OCaml.add_new_type "pattern"
    (OCaml.Sum
       [ ("PatVar", [ OCaml.Var "name" ]);
         ("PatLiteral", [ OCaml.Var "literal" ]);
         ("PatConstructor",
          [ OCaml.Var "name"; OCaml.List (OCaml.Var "pattern") ]);
         ("PatTuple", [ OCaml.List (OCaml.Var "pattern") ]);
         ("PatRecord", [ OCaml.List (OCaml.Var "field_pattern") ]);
         ("PatList", [ OCaml.List (OCaml.Var "pattern") ]);
         ("PatKeyVal", [ OCaml.Var "pattern"; OCaml.Var "pattern" ]);
         ("PatUnderscore", [ OCaml.Var "tok" ]);
         ("PatDisj", [ OCaml.Var "pattern"; OCaml.Var "pattern" ]);
         ("PatTyped", [ OCaml.Var "pattern"; OCaml.Var "type_" ]);
         ("OtherPat",
          [ OCaml.Var "other_pattern_operator"; OCaml.List (OCaml.Var "any") ]) ])
and tof_other_stmt_operator =
  OCaml.add_new_type "other_stmt_operator"
    (OCaml.Sum
       [ ("OS_Delete", []); ("OS_Print", []); ("OS_ForOrElse", []);
         ("OS_WhileOrElse", []); ("OS_TryOrElse", []); ("OS_With", []);
         ("OS_ThrowFrom", []); ("OS_ThrowNothing", []); ("OS_Global", []);
         ("OS_NonLocal", []); ("OS_Pass", []); ("OS_Sync", []);
         ("OS_Asm", []) ])
and tof_for_var_or_expr =
  OCaml.add_new_type "for_var_or_expr"
    (OCaml.Sum
       [ ("ForInitVar",
          [ OCaml.Var "entity"; OCaml.Var "variable_definition" ]);
         ("ForInitExpr", [ OCaml.Var "expr" ]) ])
and tof_for_header =
  OCaml.add_new_type "for_header"
    (OCaml.Sum
       [ ("ForClassic",
          [ OCaml.List (OCaml.Var "for_var_or_expr"); OCaml.Var "expr";
            OCaml.Var "expr" ]);
         ("ForEach", [ OCaml.Var "pattern"; OCaml.Var "expr" ]) ])
and tof_label = OCaml.add_new_type "label" (OCaml.Var "name")
and tof_finally = OCaml.add_new_type "finally" (OCaml.Var "stmt")
and tof_catch =
  OCaml.add_new_type "catch"
    (OCaml.Tuple [ OCaml.Var "pattern"; OCaml.Var "stmt" ])
and tof_case =
  OCaml.add_new_type "case"
    (OCaml.Sum [ ("Case", [ OCaml.Var "expr" ]); ("Default", []) ])
and tof_case_and_body =
  OCaml.add_new_type "case_and_body"
    (OCaml.Tuple [ OCaml.List (OCaml.Var "case"); OCaml.Var "stmt" ])
and tof_stmt =
  OCaml.add_new_type "stmt"
    (OCaml.Sum
       [ ("ExprStmt", [ OCaml.Var "expr" ]);
         ("LocalDef", [ OCaml.Var "definition" ]);
         ("LocalDirective", [ OCaml.Var "directive" ]);
         ("Block", [ OCaml.List (OCaml.Var "stmt") ]);
         ("If", [ OCaml.Var "expr"; OCaml.Var "stmt"; OCaml.Var "stmt" ]);
         ("While", [ OCaml.Var "expr"; OCaml.Var "stmt" ]);
         ("DoWhile", [ OCaml.Var "stmt"; OCaml.Var "expr" ]);
         ("For", [ OCaml.Var "for_header"; OCaml.Var "stmt" ]);
         ("Switch",
          [ OCaml.Var "expr"; OCaml.List (OCaml.Var "case_and_body") ]);
         ("Return", [ OCaml.Var "expr" ]);
         ("Continue", [ OCaml.Option (OCaml.Var "expr") ]);
         ("Break", [ OCaml.Option (OCaml.Var "expr") ]);
         ("Label", [ OCaml.Var "label"; OCaml.Var "stmt" ]);
         ("Goto", [ OCaml.Var "label" ]); ("Throw", [ OCaml.Var "expr" ]);
         ("Try",
          [ OCaml.Var "stmt"; OCaml.List (OCaml.Var "catch");
            OCaml.Option (OCaml.Var "finally") ]);
         ("Assert", [ OCaml.Var "expr"; OCaml.Option (OCaml.Var "expr") ]);
         ("OtherStmt",
          [ OCaml.Var "other_stmt_operator"; OCaml.List (OCaml.Var "any") ]) ])
and tof_other_attribute_operator =
  OCaml.add_new_type "other_attribute_operator"
    (OCaml.Sum
       [ ("OA_StrictFP", []); ("OA_Transient", []); ("OA_Synchronized", []);
         ("OA_Native", []); ("OA_AnnotJavaOther", [ OCaml.String ]);
         ("OA_AnnotThrow", []); ("OA_Expr", []) ])
and tof_attribute =
  OCaml.add_new_type "attribute"
    (OCaml.Sum
       [ ("Static", []); ("Volatile", []); ("Extern", []); ("Public", []);
         ("Private", []); ("Protected", []); ("Abstract", []); ("Final", []);
         ("Var", []); ("Let", []); ("Const", []); ("Generator", []);
         ("Async", []); ("Ctor", []); ("Dtor", []); ("Getter", []);
         ("Setter", []); ("Variadic", []);
         ("NamedAttr", [ OCaml.Var "name"; OCaml.List (OCaml.Var "any") ]);
         ("OtherAttribute",
          [ OCaml.Var "other_attribute_operator";
            OCaml.List (OCaml.Var "any") ]) ])
and tof_other_type_operator =
  OCaml.add_new_type "other_type_operator"
    (OCaml.Sum
       [ ("OT_Expr", []); ("OT_Arg", []); ("OT_StructName", []);
         ("OT_UnionName", []); ("OT_EnumName", []); ("OT_Shape", []);
         ("OT_Variadic", []) ])
and tof_other_type_argument_operator =
  OCaml.add_new_type "other_type_argument_operator"
    (OCaml.Sum [ ("OTA_Question", []) ])
and tof_type_argument =
  OCaml.add_new_type "type_argument"
    (OCaml.Sum
       [ ("TypeArg", [ OCaml.Var "type_" ]);
         ("OtherTypeArg",
          [ OCaml.Var "other_type_argument_operator";
            OCaml.List (OCaml.Var "any") ]) ])
and tof_type_arguments =
  OCaml.add_new_type "type_arguments"
    (OCaml.List (OCaml.Var "type_argument"))
and tof_type_ =
  OCaml.add_new_type "type_"
    (OCaml.Sum
       [ ("TyBuiltin", [ OCaml.Apply ("wrap", OCaml.String) ]);
         ("TyFun", [ OCaml.List (OCaml.Var "type_"); OCaml.Var "type_" ]);
         ("TyApply", [ OCaml.Var "name"; OCaml.Var "type_arguments" ]);
         ("TyVar", [ OCaml.Var "name" ]);
         ("TyArray", [ OCaml.Option (OCaml.Var "expr"); OCaml.Var "type_" ]);
         ("TyPointer", [ OCaml.Var "type_" ]);
         ("TyTuple", [ OCaml.List (OCaml.Var "type_") ]);
         ("TyQuestion", [ OCaml.Var "type_" ]);
         ("OtherType",
          [ OCaml.Var "other_type_operator"; OCaml.List (OCaml.Var "any") ]) ])
and tof_other_expr_operator =
  OCaml.add_new_type "other_expr_operator"
    (OCaml.Sum
       [ ("OE_Exports", []); ("OE_Module", []); ("OE_Define", []);
         ("OE_Arguments", []); ("OE_NewTarget", []); ("OE_Delete", []);
         ("OE_YieldStar", []); ("OE_Encaps", []); ("OE_Require", []);
         ("OE_UseStrict", []); ("OE_ObjAccess_PN_Computed", []);
         ("OE_ExprClass", []); ("OE_Imag", []); ("OE_Is", []);
         ("OE_IsNot", []); ("OE_In", []); ("OE_NotIn", []);
         ("OE_Invert", []); ("OE_Slice", []); ("OE_SliceIndex", []);
         ("OE_SliceRange", []); ("OE_CompForIf", []); ("OE_CompFor", []);
         ("OE_CompIf", []); ("OE_CmpOps", []); ("OE_Repr", []);
         ("OE_NameOrClassType", []); ("OE_ClassLiteral", []);
         ("OE_RecordPtAccess", []); ("OE_SizeOf", []);
         ("OE_ArrayInitDesignator", []); ("OE_GccConstructor", []);
         ("OE_Unpack", []) ])
and tof_action =
  OCaml.add_new_type "action"
    (OCaml.Tuple [ OCaml.Var "pattern"; OCaml.Var "expr" ])
and tof_other_argument_operator =
  OCaml.add_new_type "other_argument_operator"
    (OCaml.Sum [ ("OA_ArgPow", []); ("OA_ArgComp", []) ])
and tof_argument =
  OCaml.add_new_type "argument"
    (OCaml.Sum
       [ ("Arg", [ OCaml.Var "expr" ]);
         ("ArgKwd", [ OCaml.Var "name"; OCaml.Var "expr" ]);
         ("ArgOther",
          [ OCaml.Var "other_argument_operator"; OCaml.List (OCaml.Var "any") ]) ])
and tof_arguments =
  OCaml.add_new_type "arguments" (OCaml.List (OCaml.Var "argument"))
and tof_arithmetic_operator =
  OCaml.add_new_type "arithmetic_operator"
    (OCaml.Sum
       [ ("Plus", []); ("Minus", []); ("Mult", []); ("Div", []); ("Mod", []);
         ("Pow", []); ("FloorDiv", []); ("LSL", []); ("LSR", []);
         ("ASR", []); ("BitOr", []); ("BitXor", []); ("BitAnd", []);
         ("BitNot", []); ("And", []); ("Or", []); ("Not", []); ("Eq", []);
         ("NotEq", []); ("PhysEq", []); ("NotPhysEq", []); ("Lt", []);
         ("LtE", []); ("Gt", []); ("GtE", []) ])
and tof_special =
  OCaml.add_new_type "special"
    (OCaml.Sum
       [ ("This", []); ("Super", []); ("Self", []); ("Parent", []);
         ("Eval", []); ("Typeof", []); ("Instanceof", []); ("New", []);
         ("Concat", []); ("Spread", []);
         ("ArithOp", [ OCaml.Var "arithmetic_operator" ]);
         ("IncrDecr", [ OCaml.Bool; OCaml.Bool ]) ])
and tof_id_info =
  OCaml.add_new_type "id_info"
    (OCaml.Dict
       [ ("id_qualifier", `RO, (OCaml.Option (OCaml.Var "dotted_name")));
         ("id_typeargs", `RO, (OCaml.Option (OCaml.Var "type_arguments")));
         ("id_resolved", `RO,
          (OCaml.Apply (("ref", (OCaml.Var "resolved_name")))));
         ("id_type", `RO,
          (OCaml.Apply (("ref", (OCaml.Option (OCaml.Var "type_")))))) ])
and tof_container_operator =
  OCaml.add_new_type "container_operator"
    (OCaml.Sum [ ("Array", []); ("List", []); ("Set", []); ("Dict", []) ])
and tof_literal =
  OCaml.add_new_type "literal"
    (OCaml.Sum
       [ ("Unit", [ OCaml.Var "tok" ]);
         ("Bool", [ OCaml.Apply ("wrap", OCaml.Bool) ]);
         ("Int", [ OCaml.Apply ("wrap", OCaml.String) ]);
         ("Float", [ OCaml.Apply ("wrap", OCaml.String) ]);
         ("Char", [ OCaml.Apply ("wrap", OCaml.String) ]);
         ("String", [ OCaml.Apply ("wrap", OCaml.String) ]);
         ("Regexp", [ OCaml.Apply ("wrap", OCaml.String) ]);
         ("Null", [ OCaml.Var "tok" ]); ("Undefined", [ OCaml.Var "tok" ]) ])
and tof_expr =
  OCaml.add_new_type "expr"
    (OCaml.Sum
       [ ("L", [ OCaml.Var "literal" ]);
         ("Container",
          [ OCaml.Var "container_operator"; OCaml.List (OCaml.Var "expr") ]);
         ("Tuple", [ OCaml.List (OCaml.Var "expr") ]);
         ("Record", [ OCaml.List (OCaml.Var "field") ]);
         ("Constructor", [ OCaml.Var "name"; OCaml.List (OCaml.Var "expr") ]);
         ("Lambda", [ OCaml.Var "parameters"; OCaml.Var "stmt" ]);
         ("Nop", []); ("Id", [ OCaml.Var "name"; OCaml.Var "id_info" ]);
         ("IdSpecial", [ OCaml.Var "special" ]);
         ("Call", [ OCaml.Var "expr"; OCaml.Var "arguments" ]);
         ("Xml", [ OCaml.Var "xml" ]);
         ("Assign", [ OCaml.Var "expr"; OCaml.Var "expr" ]);
         ("AssignOp",
          [ OCaml.Var "expr"; OCaml.Var "arithmetic_operator";
            OCaml.Var "expr" ]);
         ("LetPattern", [ OCaml.Var "pattern"; OCaml.Var "expr" ]);
         ("ObjAccess", [ OCaml.Var "expr"; OCaml.Var "name" ]);
         ("ArrayAccess", [ OCaml.Var "expr"; OCaml.Var "expr" ]);
         ("Conditional",
          [ OCaml.Var "expr"; OCaml.Var "expr"; OCaml.Var "expr" ]);
         ("MatchPattern",
          [ OCaml.Var "expr"; OCaml.List (OCaml.Var "action") ]);
         ("Yield", [ OCaml.Var "expr" ]); ("Await", [ OCaml.Var "expr" ]);
         ("Cast", [ OCaml.Var "type_"; OCaml.Var "expr" ]);
         ("Seq", [ OCaml.List (OCaml.Var "expr") ]);
         ("Ref", [ OCaml.Var "expr" ]); ("DeRef", [ OCaml.Var "expr" ]);
         ("Ellipses", [ OCaml.Var "tok" ]);
         ("OtherExpr",
          [ OCaml.Var "other_expr_operator"; OCaml.List (OCaml.Var "any") ]) ])

