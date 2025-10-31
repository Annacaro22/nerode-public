open Core
module Types = Types
module Binary = Binary

(* Print out 10 random regular expressions *)
let print () = 
  Quickcheck.iter Types.Generator.gen_t
    ~seed:`Nondeterministic ~trials:10
    ~f:(fun t -> print_s (Types.sexp_of_t t))

(*get one regex*)
let get_regex () = 
  Quickcheck.random_value
    ~seed:`Nondeterministic (Types.Generator.gen_large_t 5 10)

(*print one regex, for debug purposes*)
let print_one (t : Types.t) =
  print_s (Types.sexp_of_t t)



(*binary strings*)

(*get one binary string*)
let get_binary () =
  Quickcheck.random_value
    ~seed:`Nondeterministic Binary.Generator.gen_t


(*print one string, for debug purposes*)
let print_one (t : Binary.t) =
  print_s (Binary.sexp_of_t t)