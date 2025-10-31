open Nerode
open Nerodelearn


type symbol = Alphabet.symbol
type word = Word.t

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


let generator_to_int (gen : int Base_quickcheck.Generator.t) (sizey : int) : int =
  Base_quickcheck.Generator.generate gen ~size:sizey ~random:(Splittable_random.create Base.Random.State.default)

let quickcheck_generator = 
  let size = Base_quickcheck.Generator.int in
  let sizeint = generator_to_int size 10 in
  let listsize = sizeint*2 in
  Base_quickcheck.Generator.list (Base_quickcheck.Generator.int) 


let rx_empty = Rx.Empty

let rx_0 = Rx.of_word (Word.of_intlist [0])

let rx_1 = Rx.of_word (Word.of_intlist [1])
let safe_seq x y = if x = Rx.Empty then y else if y = Rx.Empty then x else Rx.seq_pair x y

let safe_union x y = if x = Rx.Empty then y else if y = Rx.Empty then x else Rx.union_pair x y  

let regex_next_step (curr: Rx.t) (action: int) (digit: int) =
  match action with
      |1 -> let digitrex = match digit with
            |1 -> rx_0
            |2 -> rx_1
            |_ -> rx_empty in
            safe_seq curr digitrex
      |2 -> let digitrex = match digit with
            |1 -> rx_0
            |2 -> rx_1
            |_ -> rx_empty in
            safe_union curr digitrex
      |3 ->  Rx.star curr
      |_ -> curr


let rec intlist_to_regex (l : int Base_quickcheck.Generator.t list) (curr : Rx.t) =
  let size = Base_quickcheck.Generator.int in
  let sizeint = generator_to_int size 10 in

  match l with
  | h1 :: h2 :: t -> let action = h1 in
      let actionint = generator_to_int action sizeint in
      let digit = h2 in
      let digitint = generator_to_int digit sizeint in
      let added = regex_next_step (curr) actionint digitint in
      intlist_to_regex t added
  | _ -> curr



let quickcheck_shrinker = Base_quickcheck.Shrinker.list (Base_quickcheck.Shrinker.int) 

let sexp_of_t (regex : t)= Sexplib0.Sexp.sexp_of_t ([])