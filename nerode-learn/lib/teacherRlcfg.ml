(** A teacher based on right-linear grammars. The implementation is that we
    convert to DFA and answer queries accordingly. *)

open Nerode

type word = Word.t

type t = { dfa: Dfa.t; query_count: int ref }

let make (cfg: Rlcfg.t) = { dfa = Rlcfg.to_dfa cfg; query_count = ref 0 }

let conjecture (t: t) (c: Rlcfg.t) =
  let c' = Rlcfg.to_dfa c in
  let d1 = Dfa.diff t.dfa c' in
  if Dfa.is_empty d1 then
    let d2 = Dfa.diff c' t.dfa in
      if Dfa.is_empty d2 then
        None
      else
        Some (Dfa.rep d2)
  else
    Some (Dfa.rep d1)

let query (t: t) (w: word) =
  let () = t.query_count := !(t.query_count) + 1 in
  Some (Dfa.accept t.dfa w)

let distinguish _ _ _ = failwith "Does not support distinguish!"

let distinguish_concrete _ _ _ = failwith "Does not support distinguish!"

let number_queries t = !(t.query_count)
