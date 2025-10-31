open Core
open Gen

(* Print out 10 random regular expressions *)
let () = 
  Quickcheck.iter Types.Generator.gen_t
    ~seed:`Nondeterministic ~trials:1000
    ~f:(fun t -> print_s (Types.sexp_of_t t))  