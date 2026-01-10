open Core
open Nerode

let () =
  let grammar = In_channel.input_all In_channel.stdin  in
  let cfg = (GrammarParser.parse_string grammar) in

  Printf.printf "Grammar:\n\n%s\n%!" (Rlcfg.to_string cfg);

  let dfa: Dfa.t = Rlcfg.to_dfa cfg in
  
  Printf.printf "Dfa:\n%!";
  Dfa.print dfa
