(* The compiler entry points *)

open Cl
open ClMisc
open ClInterntl
open ClLexer
open ClParser
open ClLocation
open ClSyntax
open ClModules
open ClError
open ClTyping
open ClTy_decl
open ClPr_decl
open ClTy_intf
open ClFront
open ClBack
open ClEmit_phr

(* Parsing functions *)

let parse_phrase parsing_fun lexing_fun lexbuf =
  let rec skip () =
    try
      match lexing_fun lexbuf with
        EOF -> ()
      | SEMISEMI -> ()
      | _ -> skip()
    with ClLexer.Lexical_error(_,_,_) ->
      skip() in
  let skip_maybe () =
    if Parsing.is_current_lookahead EOF
    || Parsing.is_current_lookahead SEMISEMI
    then () else skip() in
  try
    parsing_fun lexing_fun lexbuf
  with Parsing.Parse_error ->
         let pos1 = Lexing.lexeme_start lexbuf in
         let pos2 = Lexing.lexeme_end lexbuf in
         skip_maybe();
         eprintf "%aSyntax error.\n" output_location (Loc(pos1, pos2));
         raise Toplevel
     | ClLexer.Lexical_error(errcode, pos1, pos2) ->
         let l = Loc(pos1, pos2) in
         begin match errcode with
           ClLexer.Illegal_character ->
             eprintf "%aIllegal character.\n" output_location l
         | ClLexer.Unterminated_comment ->
             eprintf "%aComment not terminated.\n" output_location l
         | ClLexer.Bad_char_constant ->
             eprintf "%aIll-formed character literal.\n"
                             output_location l
         | ClLexer.Unterminated_string ->
             eprintf "%aString literal not terminated.\n"
                             output_location l
         end;
         skip();
         raise Toplevel
     | Toplevel ->
         skip_maybe();
         raise Toplevel
;;

let parse_impl_phrase = parse_phrase ClParser.implementation ClLexer.main
and parse_intf_phrase = parse_phrase ClParser.interface ClLexer.main
;;

(* Executing directives *)

let do_directive loc = function
    Zdir("open", name) ->
      open_module name
  | Zdir("close", name) ->
      close_module name
  | Zdir("infix", name) ->
      add_infix name
  | Zdir("uninfix", name) ->
      remove_infix name
  | Zdir("directory", dirname) ->
      load_path := dirname :: !load_path
  | Zdir(d, name) ->
      eprintf
        "%aWarning: unknown directive \"#%s\", ignored.\n"
        output_location loc d;
      flush stderr
;;

(* Warn for unused #open *)

let check_unused_opens () =
  if !ClTyping.warnings then
   hashtbl__do_table
     (fun name used ->
       if not !used && not (mem name !default_used_modules)
       then unused_open_warning name)
     !used_opened_modules
;;

(* Compiling an interface *)

let verbose = ref false;;

let compile_intf_phrase phr =
  begin match phr.in_desc with
    Zvaluedecl decl ->
      type_valuedecl phr.in_loc decl; ()
  | Ztypedecl decl ->
      let ty_decl = type_typedecl phr.in_loc decl in
      if !verbose then print_typedecl ty_decl
  | Zexcdecl decl ->
      let ex_decl = type_excdecl phr.in_loc decl in
      if !verbose then print_excdecl ex_decl
  | Zintfdirective dir ->
      do_directive phr.in_loc dir
  end
;;

let compile_interface modname filename =
  let source_name = filename ^ ".mli"
  and intf_name = filename ^ ".zi" in
  let ic = open_in_bin source_name (* See compile_impl *)
  and oc = open_out_bin intf_name in
    try
      start_compiling_interface modname;
      let lexbuf = lexing__create_lexer_channel ic in
      input_name := source_name;
      input_chan := ic;
      input_lexbuf := lexbuf;
      external_types := [];
      while true do
        compile_intf_phrase(parse_intf_phrase lexbuf)
      done
    with End_of_file ->
      close_in ic;
      write_compiled_interface oc;
      close_out oc;
      check_unused_opens()
    | x ->
      close_in ic;
      close_out oc;
      remove_file intf_name;
      raise x
;;

(* Compiling an implementation *)

let lambdas = ref []

let compile_impl_phrase outstream phr =
  reset_type_expression_vars();
  begin match phr.im_desc with
    Zexpr expr ->
      let ty = type_expression phr.im_loc expr in
(*
      emit_phrase outstream
                  (expr_is_pure expr)
                  (compile_lambda false (translate_expression expr));
*)
      lambdas := (false, expr_is_pure expr,
        translate_expression expr) :: !lambdas;
      if !verbose then print_expr ty
  | Zletdef(rec_flag, pat_expr_list) ->
      let env = type_letdef phr.im_loc rec_flag pat_expr_list in
(*
      emit_phrase outstream
         (letdef_is_pure pat_expr_list)
         (if rec_flag then
            compile_lambda true (translate_letdef_rec phr.im_loc pat_expr_list)
          else
            compile_lambda false (translate_letdef phr.im_loc pat_expr_list));
*)
      lambdas :=
         (if rec_flag then
         (true, letdef_is_pure pat_expr_list,
            translate_letdef_rec phr.im_loc pat_expr_list)
          else
         (false, letdef_is_pure pat_expr_list,
            translate_letdef phr.im_loc pat_expr_list)) :: !lambdas;

      if !verbose then print_valdef env
  | Ztypedef decl ->
      let ty_decl = type_typedecl phr.im_loc decl in
      if !verbose then print_typedecl ty_decl
  | Zexcdef decl ->
      let ex_decl = type_excdecl phr.im_loc decl in
      if !verbose then print_excdecl ex_decl
  | Zimpldirective dir ->
      do_directive phr.im_loc dir
  end
;;

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let ppf = Format.err_formatter

let (++) x f = f x

module ToLambda = struct

  open Asttypes

  open ClConst
  open ClLambda
  open ClPrim

  module L = Lambda

(* Beware: no float array and record array optimization.
   What about exceptions ?
*)

  let lunit = L.Lconst (L.Const_pointer 0)

  type env = {
    vars : Ident.t list;
    static : int;
  }

  let find_var n env =
    let rec find_var n list =
    match n, list with
    | 0, id :: _ -> id
    | _, _ :: tail -> find_var (n-1) tail
    | _ -> assert false
    in
    find_var n env.vars

  let push id env = { env with vars = id :: env.vars }

  let translate_to_lambda () =

    let qualified_idents = Hashtbl.create 113 in
    let getglobal q =
      try
        Hashtbl.find qualified_idents q
      with Not_found ->
        let id = Ident.create_persistent (q.qual ^ "." ^ q.id) in
        Hashtbl.add qualified_idents q id;
        id
    in
    let rec map_lambdas lambdas cont =
      match lambdas with
        [] -> cont
      | (rec_flag, is_pure, l) :: ls ->
        let l = map_lambda { vars = []; static = -1 } l in
        let l = L.Lsequence( l, cont ) in
        map_lambdas ls l

    and map_lambda env l =
      match l with

      | Lapply (lam, lams) -> L.Lapply( map_lambda env lam,
                                List.map (map_lambda env) lams,
                                Location.none
                              )
      | Lletrec (decl_x_size_s , body) -> assert false (* TODO *)
      | Lcond (expr, atomic_constant_x_handler_s) -> assert false (* TODO *)
      | Lswitch (span, path, constr_tag_x_handler_s) -> assert false (* TODO *)

      | Lhandle (body, handler) ->
        let exn = Ident.create "exn" in
        let new_env = push exn env in
        L.Ltrywith( map_lambda env body, exn, map_lambda new_env handler)

      | Lstaticfail npops ->
        assert (env.static <> -1);
        L.Lstaticraise (env.static, [])

      | Lstatichandle (body, handler) ->
        let static = env.static + 1 in
        let new_env = { env with static } in
        L.Lstaticcatch( map_lambda new_env body,
          (static, []), map_lambda env handler )

      | Lfunction body ->
        let arg = Ident.create "arg" in
        let new_env = push arg env in
        L.Lfunction (L.Curried, [arg], map_lambda new_env body)

      | Lprim (primitive, args) ->
        let prim = match primitive with
            Pidentity -> L.Pidentity
          | Pget_global qident -> L.Pgetglobal (getglobal qident)
          | Pset_global qident -> L.Psetglobal (getglobal qident)
          | Pmakeblock ( ConstrRegular (tag, nconstrs) ) ->
            L.Pmakeblock (tag, Mutable)
          | Pfield int -> L.Pfield int
          | Psetfield int -> L.Psetfield ( int, true )
          | Praise -> L.Praise
          | Pnot -> L.Pnot
          | Psequand -> L.Psequand
          | Psequor -> L.Psequor
          | Pnegint -> L.Pnegint

          | Pdummy int -> assert false (* TODO *)
          | Ptest bool_test -> assert false (* TODO *)
          | Pmakeblock ( ConstrExtensible(ident, stamp) ) ->
            assert false (* TODO *)
          | Pfloatprim float_primitive -> assert false (* TODO *)
          | Pccall (string, int) -> assert false (* TODO *)
          | Pupdate -> assert false (* TODO *)
          | Ptag_of -> assert false (* TODO *)
          | Psuccint -> assert false (* removed *)
          | Ppredint -> assert false (* removed *)
          | Pincr -> assert false (* TODO *)
          | Pdecr -> assert false (* TODO *)

          | Paddint -> L.Paddint
          | Psubint -> L.Psubint
          | Pmulint -> L.Pmulint
          | Pdivint -> L.Pdivint
          | Pmodint -> L.Pmodint
          | Pandint -> L.Pandint
          | Porint -> L.Porint
          | Pxorint -> L.Pxorint
          | Pshiftleftint -> L.Plslint
          | Pshiftrightintsigned -> L.Pasrint (* verify *)
          | Pshiftrightintunsigned -> L.Plsrint
          | Pintoffloat -> L.Pintoffloat

          | Pstringlength -> L.Pstringlength
          | Pgetstringchar -> L.Pstringrefs
          | Psetstringchar -> L.Pstringsets
          | Pmakevector -> L.Pmakearray L.Pgenarray
          | Pvectlength -> L.Parraylength L.Pgenarray
          | Pgetvectitem -> L.Parrayrefs L.Pgenarray
          | Psetvectitem -> L.Parraysets L.Pgenarray

(*
and float_primitive =
    Pfloatofint
  | Pnegfloat | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat

and bool_test =
    Peq_test
  | Pnoteq_test
  | Pint_test of int prim_test
  | Pfloat_test of float prim_test
  | Pstring_test of string prim_test
  | Pnoteqtag_test of constr_tag

and 'a prim_test =
    PTeq
  | PTnoteq
  | PTnoteqimm of 'a
  | PTlt
  | PTle
  | PTgt
  | PTge
;;
*)
        in
        L.Lprim(prim, List.map (map_lambda env) args)

      | Llet (decls, body) ->

        let decls = List.map (fun expr ->
            Ident.create "x", map_lambda env expr) decls in
        let new_env = { env with
                        vars = (List.map fst decls) @ env.vars } in
        map_lambda new_env body

      | Lvar n -> L.Lvar (find_var n env)
      | Lconst s -> L.Lconst (map_struct_constant s)
      | Lifthenelse (cond, ifso, ifnot) ->
        L.Lifthenelse(map_lambda env cond,
          map_lambda env ifso, map_lambda env ifnot)
      | Lsequence (e1, e2) ->
        L.Lsequence(map_lambda env e1, map_lambda env e2)
      | Lwhile (cond, body) ->
        L.Lwhile(map_lambda env cond, map_lambda env body)
      | Lfor (from_lam, to_lam, up_flag, body) ->
        let index = Ident.create "i" in
        let from_v = Ident.create "from" in
        let to_v = Ident.create "to" in
        let new_env = { env with vars = index :: from_v :: to_v :: env.vars }
        in
        L.Lfor(index,
          map_lambda env from_lam,
          map_lambda env to_lam,
          (if up_flag then Upto else Downto),
          map_lambda new_env body)
      | Lshared (lam, lab_ref) -> map_lambda env lam
      | Levent (event, lam) -> map_lambda env lam

    and map_struct_constant s =
      match s with
      | SCatom (ACint n)  -> L.Const_base (Const_int n)
      | SCatom (ACfloat f)  -> L.Const_base (Const_float (string_of_float f))
      | SCatom (ACstring s)  -> L.Const_base (Const_string s)
      | SCatom (ACchar c)  -> L.Const_base (Const_char c)
      | SCblock (ConstrExtensible (ident, stamp), struct_constants) ->
        assert false (* TODO *)
          (* used for encoding of exceptions *)
      | SCblock (ConstrRegular (tag, nconstrs), struct_constants) ->
        L.Const_block (tag, List.map map_struct_constant struct_constants)


(*
type qualified_ident =
  { qual: string;
    id: string }
;;

type constr_tag =
    ConstrExtensible of qualified_ident * int (* name of constructor & stamp *)
  | ConstrRegular of int * int             (* tag number & number of constrs *)
;;
*)

    in
    map_lambdas !lambdas lunit

end

let compile_impl modname filename suffix =
  lambdas := [];
  let source_name = filename ^ suffix
  and obj_name = filename ^ ".zo" in
  let ic = open_in_bin source_name
  (* The source file must be opened in binary mode, so that the absolute
     seeks in print_location work. The lexer ignores both \n and \r,
     so this is OK on the Mac and on the PC. *)
  and oc = open_out_bin obj_name in
  let lexbuf = lexing__create_lexer_channel ic in
    input_name := source_name;
    input_chan := ic;
    input_lexbuf := lexbuf;
    start_emit_phrase oc;
    try
      while true do
        compile_impl_phrase oc (parse_impl_phrase lexbuf)
      done
    with End_of_file ->
      end_emit_phrase oc;
      close_in ic;
      close_out oc;

      let modulename = Filename.basename filename in
      let modulename = Filename.chop_suffix modulename ".ml" in
      let modulename = String.capitalize modulename in
      let objfile = filename ^ ".cmo" in
      let oc = open_out_bin objfile in
      ToLambda.translate_to_lambda ()
     ++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
      ++ Simplif.simplify_lambda
      ++ print_if ppf Clflags.dump_lambda Printlambda.lambda
      ++ Bytegen.compile_implementation modulename
      ++ print_if ppf Clflags.dump_instr Printinstr.instrlist
      ++ Emitcode.to_file oc modulename;
      Warnings.check_fatal ();
      close_out oc;


      check_unused_opens()
    | x ->
      close_in ic;
      close_out oc;
      remove_file obj_name;
      raise x
;;

let write_extended_intf = ref false;;

let compile_implementation modname filename suffix =
  external_types := [];
  if file_exists (filename ^ ".mli") then begin
    try
      let intfname =
        try
          find_in_path (modname ^ ".zi")
        with Cannot_find_file _ ->
          eprintf
            "Cannot find file %s.zi. Please compile %s.mli first.\n"
            modname filename;
          raise Toplevel in
      let intf = read_module modname intfname in
      start_compiling_implementation modname intf;
      enter_interface_definitions intf;
      compile_impl modname filename suffix;
      check_interface intf;
      if !write_extended_intf then begin
        let ext_intf_name = filename ^ ".zix" in
        let oc = open_out_bin ext_intf_name in
        try
          write_compiled_interface oc;
          close_out oc
        with x ->
          close_out oc;
          remove_file (ext_intf_name);
          raise x
      end;
      kill_module modname
    with x ->
      remove_file (filename ^ ".zo");
      raise x
  end else begin
    let intf_name = filename ^ ".zi" in
    let oc = open_out_bin intf_name in
    try
      start_compiling_interface modname;
      compile_impl modname filename suffix;
      check_nongen_values();
      write_compiled_interface oc;
      close_out oc
    with x ->
      close_out oc;
      remove_file intf_name;
      raise x
  end
;;
