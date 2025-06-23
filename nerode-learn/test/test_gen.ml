open Core
module Types = Types

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

