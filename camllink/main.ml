(* The Caml Light linker. Command-line parsing. *)

open Cl
open ClConfig
open ClMisc
open ClSymtable
open ClLink

let object_files = ref ([] : string list)
and exec_file = ref default_exec_name
and prim_file = ref ""
;;

let anonymous s =
  let name =
    if filename__check_suffix s ".ml" then
      filename__chop_suffix s ".ml" ^ ".zo"
    else
      s in
  object_files := name :: !object_files
;;
let set_stdlib p =
  path_library := p;
  load_path := [!path_library]
and add_include d =
  load_path := d :: !load_path
and set_debug () =
  write_debug_info := true
and set_exec_file e =
  exec_file := e
and set_custom f =
  custom_runtime := true;
  prim_file := f
and show_version () =
  ClVersion.print_banner(); exit 0
and process_include filename =
  do_list anonymous (ClReadword.from_file filename)
and process_require filename =
  let rec require = function
    [] ->
      ()
  | "val"::qual::id::rest ->
      require_qualid qual id; require rest
  | "prim"::name::rest ->
      let _n = get_num_of_prim name in require rest
  | _ ->
      eprintf "Syntax error in \"-require\" file %s.\n" filename;
      raise Toplevel in
  require (ClReadword.from_file filename)
and set_language lang =
  ClInterntl.language := lang
;;

let main() =
try
  Sys.catch_break true;
  load_path := [!path_library];
  reset_linker_tables();
  arg__parse ["-stdlib", Arg.String set_stdlib;
              "-I", Arg.String add_include;
              "-include", Arg.String add_include;
              "-g", Arg.Unit set_debug;
              "-debug", Arg.Unit set_debug;
              "-o", Arg.String set_exec_file;
              "-exec", Arg.String set_exec_file;
              "-custom", Arg.String set_custom;
              "-v", Arg.Unit show_version;
              "-version", Arg.Unit show_version;
              "-files", Arg.String process_include;
              "-require", Arg.String process_require;
              "-lang", Arg.String set_language;
              "-", Arg.String anonymous]
             anonymous;
  link (rev !object_files) !exec_file;
  if !custom_runtime then begin
    let oc = open_out !prim_file in
    output_primitives oc;
    close_out oc
  end;
  exit 0

with Toplevel -> exit 2
   | Sys.Break -> exit 3
   | Sys_error msg ->
      eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

let _ = Printexc.catch main ()
