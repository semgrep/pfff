open Ast_ruby

  let fixnum i pos = E_Literal(Lit_FixNum i, pos)
  let float f pos = E_Literal(Lit_Float(string_of_float f, f), pos)

  let ltrue pos = E_Literal(Lit_True,pos)
  let lfalse pos = E_Literal(Lit_False,pos)
  let lself pos = E_Literal(Lit_Self,pos)
  let lnil pos = E_Literal(Lit_Nil,pos)

  let ident x pos =
    let len = String.length x in
      if len = 0 then
        E_Identifier(ID_Uppercase, "", pos)
      else
        let kind = match x.[0] with
	  | 'a'..'z' | '_' -> ID_Lowercase
	  | '@' -> if x.[1] = '@' then ID_Class else ID_Instance
	  | '$' -> ID_Global
	  | 'A'..'Z' -> ID_Uppercase
	  | _ -> raise (Invalid_argument "ast_id")
        in
	  if x.[len-1] = '=' then
	    E_Identifier(ID_Assign kind, String.sub x 0 (len-1), pos)
	  else      
	    E_Identifier(kind, x, pos)

  let str kind pos = E_Literal((Lit_String kind),pos)
  let single_str s = str (String_Single s)
  let double_str s = str (String_Double [StrChars s])
  let tick_str s = str (String_Tick [StrChars s])
  let regexp s m pos = E_Literal(Lit_Regexp([StrChars s],m),pos)
  let atom s pos = E_Literal(Lit_Atom([StrChars s]),pos)

  let scoped_ident lst pos = 
    let rec work = function
      | [] -> assert false
      | [x] -> ident x pos
      | x::(_::_ as rest) -> 
          E_Binop(work rest,Op_SCOPE,ident x pos,pos)
    in work (List.rev lst)

  let mcall targ args ?cb pos = E_MethodCall(targ,args,cb,pos)

  let cb ?args body pos = E_CodeBlock(true,args,body,pos)


  let dp = Lexing.dummy_pos

