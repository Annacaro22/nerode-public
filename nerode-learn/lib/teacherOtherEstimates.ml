(** Angluin's minimally adequate teacher, using equivalence strategies e.g. equivalence on all strings up to
length n, random m strings up to length n, etc, to evaluate DFA equivalence *)

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


let intToSize n : int =
  let output = Float.to_int ((2. ** (Float.of_int(n) +. 1.0)) -. 2.0) in output

(*m random strings up to length n*)
let randomints m n = 
  let max_quers = intToSize m in
  let querylist = List.init max_quers (fun x -> 0) in
  let numbers = List.map (fun _ -> Random.int_in_range ~min:2 ~max:(intToSize n)) querylist in
  numbers

(*all strings up to legnth n*)
let firstints n = 
  let max_quers = intToSize n in
  let querylist = List.init max_quers (fun x -> x + 2) in
  querylist

let conversion (l: int list list) : Alphabet.symbol list list = List.map (List.map Alphabet.sym_of_int) l

let generateTests_random (m : int) (n : int) : (word list * int) =
  let binaryStrings = List.map int2string (randomints m n) in
  let quers = intToSize m in
  let withepsilon = [Word.epsilon] @ binaryStrings in
  (withepsilon, quers)

let generateTests_all (n : int) : (word list * int) =
  let binaryStrings = List.map int2string (firstints n) in
  let quers = intToSize n in
  let withepsilon = [Word.epsilon] @ binaryStrings in
  (withepsilon, quers)

let conjecture_random (t: t) (c: Dfa.t) : word option =
  let m = 5 in
  let n = 10 in
  let withepsilon = fst (generateTests_random m n) in
  let differenty = different t c in
  (*filters out all non-different words, leaves only different ones (counterexamples)*)
  let counterexes = List.filter differenty withepsilon in
  match counterexes with
    | [] -> None
    | x :: l -> Some x 

let conjecture_all (t: t) (c: Dfa.t) : word option =
  let n = 10 in
  let withepsilon = fst (generateTests_all n) in
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
