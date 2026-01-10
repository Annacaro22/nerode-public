(** Instance of NFA, where states are type [int * int] *)

open Stdlib

module PInt = struct
  module Pair = struct
    type t = int * int
    let compare (a,b) (c,d) =
      let ac = Int.compare a c in
      if ac = 0 then
        Int.compare b d
      else
        ac
  end
  module StateSet = Set.Make(Pair)
  include Pair
  let fresh (s: StateSet.t) =
    let (a,b) = StateSet.max_elt s in
    (a + 1, b + 1)
  let to_string (a,b) =
    "(" ^ (Int.to_string a) ^ ", " ^ (Int.to_string b) ^ ")"
end

include Nfa.Make(PInt)


let trans_to_json trans_list = failwith "unimplemented!!"

let json_to_trans json = failwith "unimplemented!!"

let of_json json = failwith "unimplemented!!"

let to_json (nfa: t) = failwith "nothing here!!!"
