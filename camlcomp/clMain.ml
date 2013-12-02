(* The Caml Light compiler. Command-line parsing. *)

open Cl
open ClConfig
open ClMisc
open ClModules
open ClCompiler

let anonymous s =
  if filename__check_suffix s ".ml" then
    let filename = filename__chop_suffix s ".ml" in
    compile_implementation (filename__basename filename) filename ".ml"
  else if filename__check_suffix s ".mlt" then (* profiler temp files *)
    let filename = filename__chop_suffix s ".mlt" in
    compile_implementation (filename__basename filename) filename ".mlt"
  else if filename__check_suffix s ".mli" then
    let filename = filename__chop_suffix s ".mli" in
    compile_interface (filename__basename filename) filename
  else
    raise (Arg.Bad ("don't know what to do with " ^ s))
and set_stdlib p =
  path_library := p;
  load_path := [!path_library]
and add_include d =
  load_path := d :: !load_path
and open_set set =
  try
    default_used_modules := assoc set default_used_interfaces
  with Not_found ->
    raise (Arg.Bad ("unknown module set " ^ set))
and show_version () =
  ClVersion.print_banner(); flush std_err
and show_types_flag () =
  ClCompiler.verbose := true
and debug_option () =
  ClEvent.record_events := true; ClCompiler.write_extended_intf := true
and set_language lang =
  ClInterntl.language := lang
and warnings_option () =
  ClTyping.warnings := true
;;

let main() =
try
  Sys.catch_break true;
  default_used_modules := assoc "cautious" default_used_interfaces;
  load_path := [!path_library];
  arg__parse ["-stdlib", Arg.String set_stdlib;
              "-I", Arg.String add_include;
              "-include", Arg.String add_include;
              "-O", Arg.String open_set;
              "-open", Arg.String open_set;
              "-v", Arg.Unit show_version;
              "-version", Arg.Unit show_version;
              "-i", Arg.Unit show_types_flag;
              "-g", Arg.Unit debug_option;
              "-debug", Arg.Unit debug_option;
              "-lang", Arg.String set_language;
              "-", Arg.String anonymous;
              "-W", Arg.Unit warnings_option]
             anonymous;
  exit 0
with Toplevel -> exit 2
   | Sys.Break -> exit 2
   | Sys_error msg ->
      eprintf "Input/output error: %s.\n" msg;
      exit 2
   | Zinc s ->
      eprintf "Internal error: %s.\nPlease report it.\n" s;
      exit 100
;;

let _ =
  Printexc.catch main ()
