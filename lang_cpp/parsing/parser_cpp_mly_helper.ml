open Common
open Ast_cpp

module Ast = Ast_cpp
module Flag = Flag_parsing

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common2.mk_pr2_wrappers Flag.verbose_parsing

let warning s v =
  if !Flag.verbose_parsing
  then Common2.warning ("PARSING: " ^ s) v
  else v

exception Semantic of string * Ast_cpp.tok

let fake s = Parse_info.fake_info s

(*****************************************************************************)
(* Parse helpers functions *)
(*****************************************************************************)

(*-------------------------------------------------------------------------- *)
(* Type related *)
(*-------------------------------------------------------------------------- *)

type storage_opt = NoSto | StoTypedef of tok | Sto of storage wrap

type shortLong = Short | Long | LongLong

(* TODO: delete *)
type 'a wrapx  = 'a * tok list

(* note: have a full_info: parse_info list; to remember ordering
 * between storage, qualifier, type? well this info is already in
 * the Ast_c.info, just have to sort them to get good order
*)
type decl = {
  storageD: storage_opt;
  typeD: (sign option * shortLong option * typeC option) wrapx;
  qualifD: type_qualifiers;
  modifierD: modifier list;
}

let noii = []

let nullDecl = {
  storageD = NoSto;
  typeD = (None, None, None), noii;
  qualifD = Ast.nQ;
  modifierD = [];
}

let addStorageD x decl  =
  match decl with
  | {storageD = NoSto; _} -> { decl with storageD = x }
  | {storageD = (StoTypedef ii | Sto (_, ii)) as y; _} ->
      if x = y
      then decl |> warning "duplicate storage classes"
      else raise (Semantic ("multiple storage classes", ii))

let addModifierD x decl =
  { decl with modifierD = x::decl.modifierD }
(*
  match decl with
  | {inlineD = (false,[]); _} -> { decl with inlineD=(true,[ii])}
  | {inlineD = (true, _ii2); _} -> decl |> warning "duplicate inline"
  | _ -> raise Impossible
*)

let addTypeD ty decl =
  match ty, decl with
  | (Left3 Signed,_ii), {typeD = ((Some Signed,  _b,_c),_ii2); _} ->
      decl |> warning "duplicate 'signed'"
  | (Left3 UnSigned,_ii), {typeD = ((Some UnSigned,_b,_c),_ii2); _} ->
      decl |> warning "duplicate 'unsigned'"
  | (Left3 _,ii),        {typeD = ((Some _,_b,_c),_ii2); _} ->
      raise (Semantic ("both signed and unsigned specified", List.hd ii))
  | (Left3 x,ii),        {typeD = ((None,b,c),ii2); _} ->
      { decl with typeD = (Some x,b,c),ii @ ii2}
  | (Middle3 Short,_ii),  {typeD = ((_a,Some Short,_c),_ii2); _} ->
      decl |> warning "duplicate 'short'"


  (* gccext: long long allowed *)
  | (Middle3 Long,ii),   {typeD = ((a,Some Long,c),ii2); _}->
      { decl with typeD = (a, Some LongLong, c),ii@ii2 }
  | (Middle3 Long,_ii),  {typeD = ((_a,Some LongLong,_c),_ii2); _} ->
      decl |> warning "triplicate 'long'"

  | (Middle3 _,ii),     {typeD = ((_a,Some _,_c),_ii2); _} ->
      raise (Semantic ("both long and short specified", List.hd ii))
  | (Middle3 x,ii),      {typeD = ((a,None,c),ii2); _} ->
      { decl with typeD = (a, Some x,c),ii@ii2}

  | (Right3 _t,ii),     {typeD = ((_a,_b,Some _),_ii2); _} ->
      raise (Semantic ("two or more data types", List.hd ii))
  | (Right3 t,ii),       {typeD = ((a,b,None),ii2); _}   ->
      { decl with typeD = (a,b, Some t),ii@ii2}


let addQualif tq1 tq2 =
  (* TODO
     match tq1, tq2 with
     | {const=Some _; _},   {const=Some _; _} ->
        tq2 |> warning "duplicate 'const'"
     | {volatile=Some _; _}, {volatile=Some _; _} ->
         tq2 |> warning "duplicate 'volatile'"
         | {const=Some x; _},   _ ->
         { tq2 with const = Some x}
         | {volatile=Some x; _}, _ ->
         { tq2 with volatile = Some x}
         | _ -> Common2.internal_error "there is no noconst or novolatile keyword"
       *)
  tq1::tq2

let addQualifD qu qu2 =
  { qu2 with qualifD = addQualif qu qu2.qualifD }


(*-------------------------------------------------------------------------- *)
(* Declaration/Function related *)
(*-------------------------------------------------------------------------- *)

(* stdC: type section, basic integer types (and ritchie)
 * To understand the code, just look at the result (right part of the PM)
 * and go back.
*)
let type_and_storage_from_decl
    {storageD = st;
     qualifD = qu;
     typeD = (ty,iit);
     modifierD = mods;
    }  =
  (qu,
   (match ty with
    | (None, None, None)     ->
        (* c++ext: *)
        (match st with
         | Sto (Auto, ii) -> TBase (Void ii)(* TODO AST *)
         | _ ->
             (* mine (originally default to int, but this looks like bad style) *)
             raise (Semantic ("no type (could default to 'int')", List.hd iit))
        )
    | (None, None, Some t)   ->
        t
    | (Some sign,  None, None) ->
        TBase(IntType (Si (sign, CInt), List.hd iit))
    | (Some sign,  None, Some (TBase (IntType (Si (_,CInt), t1)))) ->
        TBase(IntType (Si (sign, CInt), t1))

    | ((None|Some Signed), Some x, None) ->
        TBase(IntType (Si (Signed,
                           [Short,CShort; Long, CLong; LongLong, CLongLong] |> List.assoc x),
                       List.hd iit))
    | ((None|Some Signed), Some x, Some(TBase(IntType (Si (_,CInt), t1)))) ->
        TBase(IntType (Si (Signed,
                           [Short,CShort; Long, CLong; LongLong, CLongLong] |> List.assoc x), t1))

    | (Some UnSigned, Some x, None) ->
        TBase(IntType (Si (UnSigned,
                           [Short,CShort; Long, CLong; LongLong, CLongLong] |> List.assoc x),
                       List.hd iit))

    | (Some UnSigned, Some x, Some (TBase (IntType (Si (_,CInt), t1))))->
        TBase(IntType (Si (UnSigned,
                           [Short,CShort; Long, CLong; LongLong, CLongLong] |> List.assoc x), t1))

    | (Some sign,   None, (Some (TBase (IntType (CChar, ii)))))   ->
        TBase(IntType (Si (sign, CChar2), ii))

    | (None, Some Long,(Some(TBase(FloatType (CDouble, ii)))))    ->
        TBase (FloatType (CLongDouble, ii))

    | (Some _,_, Some _) ->
        raise (Semantic("signed, unsigned valid only for char and int", List.hd iit))
    | (_,Some _,(Some(TBase(FloatType ((CFloat|CLongDouble), _))))) ->
        raise (Semantic ("long or short specified with floatint type", List.hd iit))
    | (_,Some Short,(Some(TBase(FloatType (CDouble, _))))) ->
        raise (Semantic ("the only valid combination is long double", List.hd iit))

    | (_, Some _, Some _) ->
        (* mine *)
        raise (Semantic ("long, short valid only for int or float", List.hd iit))

    (* if do short uint i, then gcc say parse error, strange ? it is
     * not a parse error, it is just that we dont allow with typedef
     * either short/long or signed/unsigned. In fact, with
     * parse_typedef_fix2 (with et() and dt()) now I say too parse
     * error so this code is executed only when do short struct
     * {....} and never with a typedef cos now we parse short uint i
     * as short ident ident => parse error (cos after first short i
     * pass in dt() mode) *)
   )), st, mods


let id_of_dname_for_typedef dname =
  match dname with
  | DN (None, [], IdIdent id) -> id
  | _ ->
      raise
        (Parse_info.Other_error
           ("expecting an ident for typedef", ii_of_dname dname))


let make_onedecl ~v_namei ~mods ~sto v_type : onedecl =
  let specs = mods |> List.map (fun m -> M m) in
  match v_namei with
  (* less: could check sto, because typedef can't be anonymous since c++17
   * lesS: use mods?
  *)
  | None -> EmptyDecl v_type
  | Some (dn, iniopt) ->
      (match sto with
       | StoTypedef t ->
           (* less: use mods? *)
           let id = id_of_dname_for_typedef dn in
           TypedefDecl (t, v_type, id)
       | NoSto | Sto _ ->
           let more_specs =
             match sto with
             | NoSto -> []
             | Sto sto -> [ST sto]
             | _ -> raise Impossible
           in
           let v_specs = specs @ more_specs in
           match dn, iniopt with
           | DN n, _ -> V { v_name = n; v_init = iniopt; v_specs; v_type }
           | DNStructuredBinding ids, Some ini ->
               StructuredBinding (v_type, ids, ini)
           | DNStructuredBinding _ids, None ->
               raise
                 (Parse_info.Other_error
                    ("expecting an init for structured_binding", ii_of_dname dn))
      )

let type_and_specs_from_decl decl =
  let {storageD = st; _} = decl in
  let (t,_storage, _inline) = type_and_storage_from_decl decl in
  match st with
  | NoSto -> t, []
  | Sto (Register, ii) ->
      t, [ST (Register, ii)]
  | StoTypedef ii | Sto (_, ii) ->
      raise (Semantic ("storage class specified for parameter of function", ii))

let fixNameForParam (name, ftyp) =
  match name with
  | None, [], IdIdent id -> id, ftyp
  | _ ->
      let ii =  Lib_parsing_cpp.ii_of_any (Name name) |> List.hd in
      raise (Semantic ("parameter have qualifier", ii))

let type_and_storage_for_funcdef_from_decl decl =
  let (returnType, storage, _inline) = type_and_storage_from_decl decl in
  (match storage with
   | StoTypedef tok ->
       raise (Semantic ("function definition declared 'typedef'", tok))
   | _x -> (returnType, storage)
  )

(*
 * this function is used for func definitions (not declarations).
 * In that case we must have a name for the parameter.
 * This function ensures that we give only parameterTypeDecl with well
 * formed Classic constructor.
 *
 * todo?: do we accept other declaration in ?
 * so I must add them to the compound of the deffunc. I dont
 * have to handle typedef pb here cos C forbid to do VF f { ... }
 * with VF a typedef of func cos here we dont see the name of the
 * argument (in the typedef)
 *)
let (fixOldCDecl: type_ -> type_) = fun ty ->
  match snd ty with
  | TFunction ({ft_params=params;_}) ->
      (* stdC: If the prototype declaration declares a parameter for a
       * function that you are defining (it is part of a function
       * definition), then you must write a name within the declarator.
       * Otherwise, you can omit the name. *)
      (match Ast.unparen params with
       | [P {p_name = None; p_type = ty2;_}] ->
           (match Ast.unwrap_typeC ty2 with
            | TBase (Void _) -> ty
            | _ ->
                (* less: there is some valid case actually, when use interfaces
                 * and generic callbacks where specific instances do not
                 * need the extra parameter (happens a lot in plan9).
                 * Maybe this check is better done in a scheck for C.
                   let info = Lib_parsing_cpp.ii_of_any (Type ty2) +> List.hd in
                   pr2 (spf "SEMANTIC: parameter name omitted (but I continue) at %s"
                         (Parse_info.string_of_info info)
                   );
                *)
                ty
           )
       | params ->
           (params |> List.iter (fun (param) ->
              match param with
              | P {p_name = None; p_type = _ty2; _} ->
                  (* see above
                     let info = Lib_parsing_cpp.ii_of_any (Type ty2) +> List.hd in
                     (* if majuscule, then certainly macro-parameter *)
                     pr2 (spf "SEMANTIC: parameter name omitted (but I continue) at %s"
                         (Parse_info.string_of_info info)
                     );
                  *)
                  ()
              | _ -> ()
            ));
           ty
      )
  (* todo? can we declare prototype in the decl or structdef,
   *  ... => length <> but good kan meme
  *)
  | _ ->
      (* gcc says parse error but I dont see why *)
      let ii = Lib_parsing_cpp.ii_of_any (Type ty) |> List.hd in
      raise (Semantic ("seems this is not a function", ii))

(* TODO: this is ugly ... use record! *)
let fixFunc ((name, ty, _stoTODO), cp) =
  match ty with
  | (aQ,(TFunction ({ft_params=params; _} as ftyp))) ->
      (* it must be nullQualif, cos parser construct only this *)
      assert (aQ =*= nQ);

      (match Ast.unparen params with
       | [P {p_name= None; p_type = ty2;_}] ->
           (match Ast.unwrap_typeC ty2 with
            | TBase (Void _) -> ()
            (* failwith "internal errror: fixOldCDecl not good" *)
            | _ -> ()
           )
       | params ->
           params |> List.iter (function
             | (P {p_name = Some _s;_}) -> ()
             (* failwith "internal errror: fixOldCDecl not good" *)
             | _ -> ()
           )
      );
      let ent = { name; specs = [] } in
      ent, { f_type = ftyp; (* TODO move in f_specs f_storage = sto; *) f_body = cp; f_specs = [] }
  | _ ->
      let ii = Lib_parsing_cpp.ii_of_any (Type ty) |> List.hd in
      raise (Semantic ("function definition without parameters", ii))

let fixFieldOrMethodDecl (xs, semicolon) =
  match xs with
  | [FieldDecl(V {
    v_name = name;
    v_init = ini_opt;
    v_type = (_q, (TFunction ft));
    v_specs = specs;
  })] ->
      (* todo? define another type instead of onedecl? *)
      let ent = { name; specs = [] } in
      let fbody =
        match ini_opt with
        | None -> FBDecl semicolon
        | Some (EqInit(tokeq, InitExpr(C(Int (Some 0, iizero))))) ->
            FBZero (tokeq, iizero, semicolon)
        | _ ->
            raise (Semantic ("can't assign expression to method decl", semicolon))
      in
      let def =
        { f_type = ft; f_body = fbody; f_specs = specs } in
      MemberDecl (Func (ent, def))

  | _ -> FieldList (xs, semicolon)

(*-------------------------------------------------------------------------- *)
(* shortcuts *)
(*-------------------------------------------------------------------------- *)
(* used only in the .dyp now *)
let mk_e e = e

let mk_funcall e1 args =
  Call (e1, args)

let mk_constructor id (lp, params, rp) cp =
  let params = Common.optlist_to_list params in
  let ftyp = {
    ft_ret = nQ, (TBase (Void (fake (snd id) "void")));
    ft_params= (lp, params, rp);
    ft_specs = [];
    (* TODO *)
    ft_const = None;
    ft_throw = [];
  }
  in
  let name = name_of_id id in
  let ent = { name; specs = [] } in
  ent, { f_type = ftyp; f_body = cp; f_specs = [] }

let mk_destructor tilde id (lp, _voidopt, rp) exnopt cp =
  let ftyp = {
    ft_ret = nQ, (TBase (Void (fake (snd id) "void")));
    ft_params= (lp,  [], rp);
    ft_specs = [];
    ft_const = None;
    ft_throw = opt_to_list exnopt;
  }
  in
  let name = None, noQscope, IdDestructor (tilde, id) in
  let ent = { name; specs = [] } in
  ent, { f_type = ftyp; f_body = cp; f_specs = [] }
