open Core

type t = int list
  [@@deriving sexp]

module Generator = struct
  open Quickcheck
  open Generator.Let_syntax

let in_range lower upper x = if x >= lower then if x <= upper then true else false else false 
let gen_int : int Generator.t = Generator.filter ~f:(in_range 0 1) (Generator.size)

let gen_t : t Generator.t =
  Generator.recursive_union
    [
      return [];
      let%map n = gen_int in [n]
    ]
    ~f:(fun self -> 
      [
        let%map n = self in n
      ])
  end