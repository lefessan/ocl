

open Cl
open ClConst
open Obj
open ClSymtable

(* To translate a structured constant into an object. *)

let rec transl_structured_const = function
    SCatom(ACint i) -> repr i
  | SCatom(ACfloat f) -> repr f
  | SCatom(ACstring s) -> repr s
  | SCatom(ACchar c) -> repr c
  | SCblock(tag, comps) ->
      let res = Obj.new_block (get_num_of_tag tag) (list_length comps) in
      fill_structured_const 0 res comps;
      res

and fill_structured_const n obj = function
    [] -> ()
  | cst::rest ->
      Obj.set_field obj n (transl_structured_const cst);
      fill_structured_const (n+1) obj rest
;;
