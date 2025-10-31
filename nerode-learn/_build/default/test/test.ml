open OUnit2

open Nerode
open Nerodelearn


let test name ex_output input =
  name >:: fun _ -> assert_equal ex_output input 

let weps = Word.epsilon
let w1 = Word.of_intlist [0;1;2]
let w2 = Word.of_intlist [3;4;5]

let word_tests = [
  Word.(test "concat words" [0;1;2;3;4;5] (concat w1 w2 |> to_intlist));
  Word.(test "concat eps and word" [0;1;2] (concat weps w1 |> to_intlist));
  Word.(test "concat eps and word" [0;1;2] (concat w1 weps |> to_intlist));
  Word.(test "append letter" [0;1;2;0] 
    (0 |> Alphabet.sym_of_int |> append_letter w1 |> to_intlist));
  Word.(test "suffixes for epsilon" [weps] (suffixes weps));
  Word.(test "suffixes for non-empty word" [[3;4;5];[4;5];[5];[]] 
          (w2 |> suffixes |> List.map to_intlist));
]

module TI = TeacherIndifferent

let mk_teacher fn = 
  let () = Printf.printf "%s\n%!" (Unix.getcwd ()) in
  let (pos, neg, alpha) = InputReader.load_input fn in
  TI.make pos neg 

let teacher1 = mk_teacher "test/tomita_t1"
let teacher3 = mk_teacher "test/tomita_t3"

let alpha01 = Alphabet.intalph 2

let failed_conj = function
  | Some _ -> true
  | None -> false

let aw1111 = Word.of_intlist [1;1;1;1]
let aw0 = Word.of_intlist [0]
let aw1 = Word.of_intlist [1]
let aw00 = Word.of_intlist [0;0]
let aw010 = Word.of_intlist [0;1;0]
let aw10 = Word.of_intlist [1;0]
let aw1010 = Word.of_intlist [1;0;1;0]
let aw100 = Word.of_intlist [1;0;0]
let aw001 = Word.of_intlist [0;0;1]
let aweps = Word.epsilon

let teacher_TI_tests = [
  TI.(test "teacher query pos" (Some true) (query teacher1 aw1111));
  TI.(test "teacher query neg" (Some false) (query teacher1 aw00));
  TI.(test "teacher query blank" None (query teacher1 aw010));
  TI.(test "teacher query eps" (Some true) (query teacher1 aweps));
  TI.(test "teacher distiguish_concrete indistinguishable" (None) 
      (distinguish_concrete teacher3 aw10 aw1010));
  TI.(test "teacher distiguish_concrete distinguishable" (true) 
      (distinguish_concrete teacher3 aweps aw100 |> Option.is_some));
  TI.(test "teacher distiguish distinguishable" (true) 
      (distinguish teacher1 aw0 aw00 WordSet.empty |> Option.is_some));
]


let w001 = Word.of_intlist [0;0;1]
let w10 = Word.of_intlist [1;0]

let rx001 = Rx.of_word w001
let rx10 = Rx.of_word w10

let rx001star = Rx.star rx001
let rx_empty = Rx.Empty

let dfa_empty = Dfa.of_rx (Alphabet.intalph 2) rx_empty
let dfa001 = Dfa.of_rx (Alphabet.intalph 2) rx001
let dfa10 = Dfa.of_rx (Alphabet.intalph 2) rx10


module TE = TeacherEstimate
module TL = TeacherLStar



let teacherE001 = TE.make dfa001
let teacherL001 = TL.make dfa001


let int_list_to_symbols (l:int list) : Alphabet.symbol list = List.map Alphabet.sym_of_int l

let standardalpha = int_list_to_symbols ([0;1])

let standardalpha_separated = List.map int_list_to_symbols ([[0];[1]])

let alphabatize x = (Alphabet.symbols (Alphabet.of_string_array x))

let conversion (l: int list list) : Alphabet.symbol list list = List.map (List.map Alphabet.sym_of_int) l

let word_to_string (word: Alphabet.symbol list) = (List.map (Stdlib.string_of_int) (List.map Alphabet.sym_to_int word)) |> String.concat " "

let word_list_tostring (wordlist : Alphabet.symbol list list) = List.map word_to_string wordlist |> String.concat " | "

let teacher_TE_tests = [ (*Actually tests both regular LStar teacher as well as Estimate, for comparison*)
  test "teacherL query pos" (Some true) (TL.query teacherL001 aw001);
  test "teacherL query neg" (Some false) (TL.query teacherL001 aw00);
  test "teacherL query eps" (Some false) (TL.query teacherL001 aweps);
  test "teacherL conjecture pos" (None) (TL.conjecture teacherL001 dfa001);
  test "teacherL conjecture neg" (Some aw001) (TL.conjecture teacherL001 dfa10);
  test "teacherE query pos" (Some true) (TE.query teacherE001 aw001);
  test "teacherE query neg" (Some false) (TE.query teacherE001 aw00);
  let _ = print_endline(word_list_tostring (List.map TE.int2string (TE.firstints 1000000))) in
  test "teacherE query eps" (Some false) (TE.query teacherE001 aweps);
  (*let _ = print_endline(Word.to_string (Alphabet.of_string_array [|"0";"1"|]) (TE.int2string 5)) in*)
  (*test "teacherE conjecture pos" (None) (TE.conjecture teacherE001 dfa001);
  test "teacherE getallwords 2" (conversion [[0];[1];[0;0];[0;1];[1;0];[1;1]]) (TE.getallwords 2 dfa10);
  test "teacherE getallwords 3" (conversion [[0];[1];[0;0];[0;1];[1;0];[1;1];[0;0;0];[0;0;1];[0;1;0];[0;1;1];[1;0;0];[1;0;1];[1;1;0];[1;1;1]]) (TE.getallwords 3 dfa10);
  test "teacherE sizes/range" ([1; 2; 3; 4; 5; 6; 7; 8; 9; 10]) (TE.range 10 []);
  test "teacherE sizenwords" (standardalpha_separated) (TE.sizenwords 1 [] (standardalpha));
  test "teacherE conjecture neg" (Some _) (TE.conjecture teacherE001 dfa10);
  test "int2string test" (List.map Alphabet.sym_of_int [0;1;0;0;1]) (TE.int2string 9);
  test "int2string test 0" (List.map Alphabet.sym_of_int [0;0]) (TE.int2string 0);
  test "int2string test -1" (List.map Alphabet.sym_of_int [1;1]) (TE.int2string (-1));
  let _ = print_endline ("old: " ^ word_list_tostring (TE.generateStrings_old (ref 0) 0.1 0.1 dfa_empty)) in
  let _ = print_endline "" in
  let _ = print_endline ("new: " ^ word_list_tostring (TE.generateStrings_new (ref 0) 0.1 0.1)) in
  test "compare methods" (TE.generateStrings_old (ref 0) 0.1 0.1 dfa_empty) (TE.generateStrings_new (ref 0) 0.1 0.1)*)   
]


let query teacher (w : Word.t) : ObsTbl.entry = 
  match TI.query teacher w with
  | None -> Blank
  | Some true -> True
  | Some false -> False

let tbl_init1, e_map1 = ObsTbl.init_epsilon (Alphabet.intalph 2) (query teacher1)
let w_zero = Word.of_intlist [0]
let w_one = Word.of_intlist [1]

let tests =
  "test suite for nerode-learn"  >::: List.flatten [
    word_tests;
    teacher_TI_tests;
    teacher_TE_tests;
  ]

let _ = run_test_tt_main tests
