open Core
open Nerode

type t = int list
  [@@deriving sexp]

module Generator = struct
  open Quickcheck
  open Generator.Let_syntax

let gen_int : int Generator.t =  Generator.doubleton 0 1


(*converts type int list to alphabet list*)
let rec binary_convert (r : t) : Word.t = match r with
  | [] -> []
  | h :: t -> Alphabet.sym_of_int h :: binary_convert t


let gen_t : t Generator.t =
  Generator.recursive_union 
    [
      return [];
      let%map n = gen_int in [n]
    ]
    ~f:(fun self -> 
      [
        (let%map n = self in n);
        (let%map ns = Generator.list_non_empty self and n = self in []);
      ])
  end