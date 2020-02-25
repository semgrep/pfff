open Ast_ruby

  let fixnum i pos = Literal(FixNum i, pos)
  let float f pos = Literal(Float(string_of_float f, f), pos)

  let ltrue pos = Literal(True,pos)
  let lfalse pos = Literal(False,pos)
  let lself pos = Literal(Self,pos)
  let lnil pos = Literal(Nil,pos)

  let ident x pos =
    let len = String.length x in
      if len = 0 then
        Identifier(ID_Uppercase, "", pos)
      else
        let kind = match x.[0] with
	  | 'a'..'z' | '_' -> ID_Lowercase
	  | '@' -> if x.[1] = '@' then ID_Class else ID_Instance
	  | '$' -> ID_Global
	  | 'A'..'Z' -> ID_Uppercase
	  | _ -> raise (Invalid_argument "ast_id")
        in
	  if x.[len-1] = '=' then
	    Identifier(ID_Assign kind, String.sub x 0 (len-1), pos)
	  else      
	    Identifier(kind, x, pos)

  let str kind pos = Literal((String kind),pos)
  let single_str s = str (Single s)
  let double_str s = str (Double [StrChars s])
  let tick_str s = str (Tick [StrChars s])
  let regexp s m pos = Literal(Regexp([StrChars s],m),pos)
  let atom s pos = Literal(Atom([StrChars s]),pos)

  let scoped_ident lst pos = 
    let rec work = function
      | [] -> assert false
      | [x] -> ident x pos
      | x::(_::_ as rest) -> 
          Binop(work rest,Op_SCOPE,ident x pos,pos)
    in work (List.rev lst)

  let mcall targ args ?cb pos = MethodCall(targ,args,cb,pos)

  let cb ?args body pos = CodeBlock(true,args,body,pos)


  let dp = Lexing.dummy_pos

