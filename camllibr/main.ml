(* The Caml Light libarian. Command-line parsing. *)

open Cl
open ClConfig
open ClMisc

let lib_files = ref ([] : string list)
and lib_name = ref "library.zo";;

let anonymous s =
  lib_files := s :: !lib_files;;

let set_output s =
  lib_name := s
and show_version () =
  ClVersion.print_banner(); exit 0
and process_include filename =
  do_list anonymous (ClReadword.from_file filename)
and set_stdlib p =
  path_library := p;
  load_path := [!path_library]
and add_include d =
  load_path := d :: !load_path
and set_language lang =
  ClInterntl.language := lang
;;

let main() =
  try
    load_path := [!path_library];
    arg__parse ["-stdlib", Arg.String set_stdlib;
                "-I", Arg.String add_include;
                "-o", Arg.String set_output;
                "-output", Arg.String set_output;
                "-v", Arg.Unit show_version;
                "-version", Arg.Unit show_version;
                "-files", Arg.String process_include;
                "-lang", Arg.String set_language;
                "-", Arg.String anonymous]
             anonymous;
    Librar.make_library (rev !lib_files) !lib_name;
    exit 0
  with Toplevel ->
        exit 2
     | Sys_error msg ->
        eprintf "Input/output error: %s.\n" msg;
        exit 2
     | Zinc s ->
        eprintf "Internal error: %s.\nPlease report it.\n" s;
        exit 100
;;

let _ = Printexc.catch main ()
