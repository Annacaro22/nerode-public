(** Teacher which starts with a CFL for the positive example set
    and a regular language for the negative example set. *)

    open Nerode

    type word = Word.t
    
    type t = {
      pos: Pda.t;
      neg: Dfa.t;
      posrx: Pda.cfg; 
      negrx: Rx.t;
      d_tbl: (word * word, word option) Hashtbl.t;
      qcount: int ref;
    }
    
    let make (a: Alphabet.t) (p: Pda.cfg) (n: Rx.t) =
      let pos = Pda.pda_from_cfg p in
      let neg = Dfa.of_rx a n in
      if Pda.disjoint_from_dfa pos neg then
        { pos = pos; neg = neg; posrx = p; negrx = n; d_tbl = Hashtbl.create 101; qcount = ref 0 }
      else
        failwith "L+ and L- were not disjoint!"
    
    let conjecture (t: t) (c: Dfa.t) =
      let p = Pda.intersect_with_dfa t.pos (Dfa.complement c) in
      let n = Dfa.intersect t.neg c in
      match Pda.cfg_is_empty (Pda.cfg_from_pda p), Dfa.is_empty n with
      | true, true -> None
      | false, _ -> Some (Pda.rep p)
      | _, false -> Some (Dfa.rep n)
    
    let query (t: t) (w: word) =
      let () = t.qcount := !(t.qcount) + 1 in
      if Pda.accept t.pos w then
        Some true
      else if Dfa.accept t.neg w then
        Some false
      else
        None
  
    let number_queries (t: t) : int = !(t.qcount)
    
    let distinguish _ _ _ = failwith "Unsupported"

    let distinguish_concrete _ _ _ = failwith "Unsupported"

    