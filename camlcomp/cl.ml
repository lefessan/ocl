
(*
let sys__open = Unix.openfile
let sys__O_RDONLY = Unix.O_RDONLY
let sys__close = Unix.close
*)

let sys__remove = Sys.remove


let filename__is_absolute f = not (Filename.is_relative f)
let filename__concat = Filename.concat
let filename__basename = Filename.basename
let filename__check_suffix = Filename.check_suffix
let filename__chop_suffix = Filename.chop_suffix

let make_string = String.make
let string_length = String.length
let create_string = String.create
let sub_string = String.sub
let nth_char = String.get
let set_nth_char = String.set
let fchar__char_of_int = char_of_int

let hashtbl__find_all = Hashtbl.find_all
let hashtbl__new = Hashtbl.create
let hashtbl__add = Hashtbl.add
let hashtbl__find = Hashtbl.find
let hashtbl__remove = Hashtbl.remove
let hashtbl__do_table = Hashtbl.iter
let hashtbl__do_table_rev f t =
  let list = ref [] in
  Hashtbl.iter (fun key ele -> list := (key, ele) :: !list) t;
  List.iter (fun (key, ele) -> f key ele) !list

type ('a,'b) hashtbl__t = ('a,'b) Hashtbl.t

type lexing__lexbuf = Lexing.lexbuf

let std_err = stderr
let std_out = stdout
let std_in = stdin
let obj__magic = Obj.magic

let fprintf = Printf.fprintf
let eprintf = Printf.eprintf


let except e list = List.filter (fun x -> x <> e) list
let flat_map f l = List.flatten (List.map f l)
let for_all = List.for_all
let do_list = List.iter
let rev = List.rev
let map = List.map
let do_list2 = List.iter2
let exists = List.exists
let list_length = List.length
let memq = List.memq
let assq = List.assq
let mem_assoc = List.mem_assoc
let assoc = List.assoc
let it_list = List.fold_left
let list_it = List.fold_right
let map2 = List.map2
let mem = List.mem

let vect_of_list = Array.of_list
let make_vect = Array.create
let vect_length = Array.length
let list_of_vect = Array.to_list
let do_vect = Array.iter

let map_vect_list f t = Array.to_list (Array.map f t)

type 'a vect = 'a array
let sort__sort f list = List.sort (fun x y ->
    if f x y then -1 else 1) list
type 'a stack__t = 'a Stack.t
let stack__new = Stack.create
let stack__push = Stack.push
let stack__pop = Stack.pop
let stack__clear = Stack.clear


let replace_string dst src start =
  String.blit src 0 dst start (String.length src)

let lshift_right s n = s lsr n
let lshift_left s n = s lsl n

let minus x = -x
let minus_int x = -x
let minus_float x = -. x
let blit_string = String.blit

let lexing__get_lexeme_char = Lexing.lexeme_char
let get_lexeme_char = Lexing.lexeme_char
let lexing__get_lexeme = Lexing.lexeme
let get_lexeme = Lexing.lexeme

let lexing__get_lexeme_start = Lexing.lexeme_start
let get_lexeme_start = Lexing.lexeme_start
let lexing__get_lexeme_end = Lexing.lexeme_end
let get_lexeme_end = Lexing.lexeme_end

let lexing__create_lexer_channel ic =
  Lexing.from_channel ic

let arg__parse list f = Arg.parse (List.map (fun (op, h) ->
      op, h,
      match h with
        Arg.String _ -> "STRING "
      | Arg.Int _ -> "INT "
      | _ -> " "
    ) list) f  ""

let sys__interactive = !Sys.interactive
let blit_vect = Array.blit

module StringSet = Set.Make(String)
let set__empty (compare : string -> string -> int) = StringSet.empty
