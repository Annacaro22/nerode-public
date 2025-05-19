(*A testing suite which compares regular LStar learning with LStar estimate learning.*)

open Nerode
open Nerodelearn

module KVD = Kv.Make(TeacherLStar)
module KVD_Est = Kv.Make(TeacherEstimate)

module Lstar_Normal = Lstarblanks.Make(TeacherLStar) (Worklist.WorklistDefault)
module Lstar_Est = Lstarblanks.Make(TeacherEstimate) (Worklist.WorklistDefault)


let test name ex_output input =
  if not (Dfa.equiv ex_output input)
    then let _ = print_endline "ERRORFUL ERROR" in
    let _ = Dfa.print ex_output in
    Dfa.print input

let alpha01 = Alphabet.intalph 2

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

let w001 = Word.of_intlist [0;0;1]
let w10 = Word.of_intlist [1;0]

let rx001 = Rx.of_word w001
let rx10 = Rx.of_word w10

let rx001star = Rx.star rx001

let rx0star = Rx.star (Rx.of_word aw0)
let rx1star = Rx.star (Rx.of_word aw1)

let rx0star1star = Rx.seq_pair rx0star rx1star
let rx0staror1star = Rx.union [rx0star;rx1star]

let complicated = Rx.seq [rx0staror1star; rx001; (Rx.star rx10)]

let rx_empty = Rx.Empty

let rx_0 = Rx.of_word (Word.of_intlist [0])

let rx_1 = Rx.of_word (Word.of_intlist [1])


let rec randomRx(curr : Rx.t) =
  let _ = Random.self_init () in
  let choose = Random.int_in_range ~min:1 ~max:8 in
  let op = choose mod 4 in
  let digit = choose mod 3 in
  (*let _ = print_endline ("curr regex: " ^ Rx.to_string alpha01 curr) in
  let _ = print_endline ("op, digit: " ^ string_of_int op ^ " " ^ string_of_int digit) in*)
  match op with
  |1 -> let digitrex = (match digit with
        |1 -> rx_0
        |2 -> rx_1
        |_ -> rx_empty) in
        randomRx(Rx.seq [curr; digitrex])
  |2 -> let digitrex = (match digit with
        |1 -> rx_0
        |2 -> rx_1
        |_ -> rx_empty) in
        randomRx(Rx.union [curr; digitrex])
  |3 -> randomRx(Rx.star curr)
  |_ -> curr


let time f x y (label : string) =
  let t = Sys.time() in
  let fxy = f x y in
  print_string (label);
  Printf.printf " Execution time: %fs\n" (Sys.time() -. t);
  fxy


let dfastarstar = Dfa.of_rx (Alphabet.intalph 2) rx0star1star


let maketeacherL_Lstar rx label=
  let alpha = Alphabet.intalph 2 in
  let teacher = TeacherLStar.make (Dfa.of_rx alpha rx) in
  let d = time Lstar_Normal.learn alpha teacher label
  in d

let maketeacherE_Lstar rx label=
  let alpha = Alphabet.intalph 2 in
  let teacher = TeacherEstimate.make (Dfa.of_rx alpha rx) in
  let d = time Lstar_Est.learn alpha teacher label
  in d

let accumTime f x y= 
  let t = Sys.time() in
  let fxy = f x y in
  let currtime = (Sys.time() -. t) in
  (fxy, currtime)

let oneTest_L rx=
  let alpha = Alphabet.intalph 2 in
  let teacher = (TeacherLStar.make (Dfa.of_rx alpha rx)) in
  let result, currtime = accumTime Lstar_Normal.learn alpha teacher in
  (result, currtime, teacher)  

let oneTest_E rx=
  let alpha = Alphabet.intalph 2 in
  let teacher = (TeacherEstimate.make (Dfa.of_rx alpha rx)) in
  let result, currtime = accumTime Lstar_Est.learn alpha teacher in
  (result, currtime, teacher)  

(*Randomly samples n words to test each regex on*)
let errorMeasure (eteacher : TeacherEstimate.t)(eresult: Dfa.t) (lresult: Dfa.t) : float =
  let lang, numquers = TeacherEstimate.generateTests eteacher eresult 0.1 0.1 in
  let singleError(x : Word.t) = if Dfa.accept(Dfa.symdiff lresult eresult) x then 1.0 else 0.0 in
  let counts = List.map singleError lang in
  let sum = List.fold_left (+.) 0.0 counts in
  let error = sum /. numquers in error

(*Tests each regex on all words up to length 10*)
let errorMeasure_full (eteacher : TeacherEstimate.t)(eresult: Dfa.t) (lresult: Dfa.t) : float =
  let binaryStrings = List.map (TeacherEstimate.int2string) (TeacherEstimate.firstints 2046) in
  let withepsilon = [Word.epsilon] @ binaryStrings in
  let singleError(x : Word.t) = if Dfa.accept(Dfa.symdiff lresult eresult) x then 1.0 else 0.0 in
  let counts = List.map singleError withepsilon in
  let sum = List.fold_left (+.) 0.0 counts in
  let error = sum /. 2047.0 in error

let oneTestRound index =
  let regex = randomRx(rx_empty) in
  (*let _ = print_endline ("current regex " ^ (Rx.to_string alpha01 regex)) in*)
  let lstar_output, lstar_time, _ = oneTest_L regex in
  let est_output, est_time, est_teacher = oneTest_E regex in
  let error = errorMeasure_full est_teacher est_output lstar_output in
  let numerrors = (if not (Dfa.equiv lstar_output est_output) then
    (*let _ = print_endline ("error on regex " ^ (Rx.to_string alpha01 regex)) in*)
    1 else 0) in
  (error, lstar_time, est_time, numerrors)


let fourTupleAdd fourTuple1 fourTuple2 =
  let f0, f1, f21, i0 = fourTuple1 in
  let f20, f2, f22, i1 = fourTuple2 in
  (f0 +. f20, Float.add f1 f2, Float.add f21 f22, i0+i1)

let allTests(numtests) =
  let testlist = List.init numtests (fun x -> 0) in
  let stats = List.map oneTestRound testlist in
  let acc = (0.0,0.0,0.0,0) in
  let consolidate : float * float * float* int = List.fold_left fourTupleAdd acc stats in
  let error_sum, lTime, eTime, numerrors = consolidate in
  let finalerror = error_sum /. (Float.of_int numtests) in
  let _ = print_endline("average error magnitude across randomized regexes on all words up to length 10 (epsilon): " ^ Stdlib.string_of_float finalerror) in
  let _ = print_endline("number of errorful tests yes/no across randomized regexes (delta): " ^ Stdlib.string_of_int numerrors ^ "/" ^ Stdlib.string_of_int numtests ^" = " ^ Stdlib.string_of_float (float_of_int numerrors /. float_of_int numtests)) in
  let _ = print_endline("L star time: " ^ Stdlib.string_of_float lTime) in
  print_endline("Estimate time: " ^ Stdlib.string_of_float eTime)


let maketeacherLstring_Lstar rx label =
  let regex = Parser.parse_string rx in
  maketeacherL_Lstar regex label
let maketeacherEstring_Lstar rx label =
  let regex = Parser.parse_string rx in
  maketeacherE_Lstar regex label

let _ = allTests 1000