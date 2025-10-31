open Core
open Nerode

type symbol = int
[@@deriving sexp]

type t =
  | Empty
  | Epsilon
  | Char of symbol
  | Seq of t list
  | Union of t list
  | Star of t
  | QMark of t
  | Intersect of t list
  | Neg of t
  [@@deriving sexp]



(*regex convert helpers*)
let usort (lst: Rx.t list) : Rx.t list =
  List.sort ~compare:Rx.compare lst |>
  List.fold_left ~f:(fun r x ->
    match r with
    | [] -> [x]
    | p::rem -> if Rx.equiv x p then r else x::r
    ) ~init:[] |>
  List.rev

let intersect (lst:Rx.t list) : Rx.t =
  let flatten (a: Rx.t list) (x : Rx.t) =
    match x with
    | Intersect i -> i @ a
    | _ -> x::a in
  let flat = List.fold_left ~f:flatten ~init:[] lst in
  match flat with
  | [] -> Empty
  | [r] -> r
  | _ -> if List.exists ~f:(fun x -> Rx.equiv x Empty) flat then Empty
          else Intersect (usort flat)

(*converts type t object to rx.t object*)
let rec regex_convert (r : t) : Rx.t = match r with
  | Empty -> Empty
  | Epsilon -> Epsilon
  | Char c -> Char (Alphabet.sym_of_int c)
  | Seq l -> Rx.seq (List.map l ~f:regex_convert)
  | Union l -> Rx.union (List.map l ~f:regex_convert)
  | Star r -> Rx.star (regex_convert r)
  | QMark r -> Rx.qmark (regex_convert r)
  | Intersect l -> intersect (List.map l ~f:regex_convert)
  | Neg r -> Rx.neg (regex_convert r)




module Generator = struct
  open Quickcheck
  open Generator.Let_syntax

let gen_symbol : symbol Generator.t =
  Generator.doubleton 0 1

let gen_t : t Generator.t =
  Generator.recursive_union
    [
      return Epsilon;
      let%map c = gen_symbol in Char c
    ]
    ~f:(fun self -> 
      [ (let%map xs = Generator.list_non_empty self in Seq xs);
        (let%map xs = Generator.list_non_empty self in Union xs); 
        (let%map x = self in Star x);
        (let%map x = self in QMark x);
        (let%map xs = Generator.list_non_empty self in Intersect xs);
        (let%map x = self in Neg x);
      ])

let gen_large_t min max : t Generator.t =
  let%bind n = Int.gen_incl min max in
  Generator.with_size gen_t ~size:n


end