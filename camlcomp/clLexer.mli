

open Cl
open ClParser

type lexical_error =
    Illegal_character
  | Unterminated_comment
  | Bad_char_constant
  | Unterminated_string;;

exception Lexical_error of lexical_error * int * int;;

val main: lexing__lexbuf -> token
  val add_infix: string -> unit
  val remove_infix: string -> unit
;;
