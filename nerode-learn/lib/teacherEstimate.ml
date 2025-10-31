(** Angluin's minimally adequate teacher, using a PAC equivalence of up to size 10 strings to judge
DFA equivalence *)

open Nerode
open Alphabet

type word = Word.t

type t = { dfa: Dfa.t; query_count: int ref }

let make (dfa: Dfa.t) = { dfa = dfa; query_count = ref 0 }

(*checks if a word is processed differently by t and c*)
let different t c w : bool= match Dfa.accept t.dfa w with
  |true -> (match Dfa.accept c w with |false -> true | true -> false)
  |false -> (match Dfa.accept c w with | true -> true | false -> false )   

let prepend_letter w (l : Alphabet.symbol) : symbol list = [l] @ w

let rec binaryConvert num bitstring =
  if num > 0 then
    let divide = num mod 2 in
    if divide == 0 then
      let newnum = num / 2 in
      let newbitstring = prepend_letter bitstring (Alphabet.sym_of_int 0) in
      binaryConvert newnum newbitstring
    else
      let newnum = (num-1) / 2 in
      let newbitstring = prepend_letter bitstring (Alphabet.sym_of_int 1) in
      binaryConvert newnum newbitstring
  else match num with
    |0 ->
      if bitstring == Word.epsilon then
        [Alphabet.sym_of_int 0]
      else  
        bitstring
    | _ -> bitstring


let int2string int =
  let binary = binaryConvert int Word.epsilon in
  match binary with
  | h :: t -> t
  | x -> x

let randomints max_queries = 
  let maximum = min max_queries 2046 in
  let querylist = List.init maximum (fun x -> 0) in
  let numbers = List.map (fun _ -> Random.int_in_range ~min:2 ~max:2046) querylist in
  numbers

let firstints max_queries = 
  let maximum = min max_queries 2046 in
  let querylist = List.init maximum (fun x -> x + 2) in
  querylist

let conversion (l: int list list) : Alphabet.symbol list list = List.map (List.map Alphabet.sym_of_int) l

let generateTests (t : t) (c: Dfa.t) (epsilon : float) (delta : float) : (word list * float) =
  let i = t.query_count in
  let max_queries = Float.ceil((1.0 /. epsilon)*.(Float.log((1.0 /. delta)+.(Float.of_int(!i) *. Float.log(2.0))))) in
  let max_queries = Float.mul max_queries 1.0 in
  let binaryStrings = List.map int2string (randomints (Float.to_int max_queries)) in
  let withepsilon = [Word.epsilon] @ binaryStrings in
  (withepsilon, max_queries)

let conjecture (t: t) (c: Dfa.t) : word option =
  let epsilon = 0.1 in
  let delta = 0.1 in
  let withepsilon = fst (generateTests (t) (c) (epsilon) (delta)) in
  let differenty = different t c in
  (*filters out all non-different words, leaves only different ones (counterexamples)*)
  let counterexes = List.filter differenty withepsilon in
  match counterexes with
    | [] -> None
    | x :: l -> Some x 

let query (t: t) (w: word) =
  let () = t.query_count := !(t.query_count) + 1 in
  Some (Dfa.accept t.dfa w)

let distinguish _ _ _ = failwith "LStar teacher (MAT) with estimation does not support distinguish!"

let distinguish_concrete _ _ _ = failwith "LStar teacher (MAT) with estimation does not support distinguish!"

let number_queries t = !(t.query_count)
