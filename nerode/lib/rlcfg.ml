type symbol = Alphabet.symbol

exception InvalidGrammar of string

module SMap = Map.Make(struct
  type t = symbol
  let compare = Alphabet.compare
end)

(* A righthand-side (rhs) is a list of terminal symbols followed by a possible
   nonterminal continuation *)
type rhs = symbol list * (symbol option)

type production = {
  lhs: symbol; (* Symbol in the nonterminal alphabet *)
  rhs: rhs;
}

type t = {
  nonterms: Alphabet.t;
  terms: Alphabet.t;
  start: symbol;
  prods: production list
}

let to_string (t:t) : string =
  List.fold_left (fun s prod ->
    Printf.sprintf "%s%s -> %s%s\n"
      s
      (Alphabet.sym_to_string t.nonterms prod.lhs)
      (String.concat "" (List.map (Alphabet.sym_to_string t.terms) (fst prod.rhs)))
      (match snd prod.rhs with
       | None -> ""
       | Some nt -> Alphabet.sym_to_string t.nonterms nt)
  ) "" t.prods

(* Produce a map from nonterminal symbols to pairs consisting of the index and the
   rhs of each rule for which they are the lhs. *)
let nt_rule_map (t:t) : ((int*rhs) list) SMap.t =
  List.fold_left (fun (m,i) prod ->
    let rule = i,prod.rhs in
    match SMap.find_opt prod.lhs m with
    | None -> SMap.add prod.lhs [rule] m, i+1
    | Some lst -> SMap.add prod.lhs (rule::lst) m, i+1
  ) (SMap.empty, 0) t.prods

  (* Finally discard the index. *)
  |> fst

let to_nfa (t: t) =
  let rule_map = nt_rule_map t in

  let start =
    match SMap.find_opt t.start rule_map with
    | None -> []
    | Some lst -> List.map (fun (i,_) -> i,0) lst in

  let final =
    (* The final states are the pairs (i,j) where rule i has length j and does
       not have a continuation nonterminal. *)
    List.fold_left (fun (f,i) prod -> 
      let seq,next = prod.rhs in
      match next with
      | None -> (i, List.length seq)::f, i+1
      | Some _ -> f, i+1
    ) ([],0) t.prods
    |> fst in

  let trans =
    (* There are two types of transitions:
       (a) Consume a letter and advance in a single rule
       (b) Transition on Eps to go to a next nonterminal *)
    List.fold_left (fun (tr,i) prod -> 
      let seq,next = prod.rhs in
      let len = List.length seq in
      let type_a = List.mapi (fun j x -> ((i,j), Nfa.Char x, (i,j+1))) seq in
      let type_b =
        match next with
        | None -> []
        | Some nt ->
            match SMap.find_opt nt rule_map with
            | None -> []
            | Some lst -> List.map (fun (j,_) -> (i,len), Nfa.Eps, (j,0)) lst
      in
      List.concat [type_b; type_a; tr], i+1
    ) ([],0) t.prods
    |> fst in
  IntpairNfa.mk_nfa t.terms start final trans

module IntpairDeterm = Dfa.Determinizer(IntpairNfa)
let to_dfa (t: t) : Dfa.t = to_nfa t |> IntpairDeterm.determinize

module StringSet = Set.Make(String)

(* TODO: add error handling to make sure the grammar is right-linear *)
let of_pairs (pairs: (char * (char list)) list) : t =
  match pairs with
  | [] -> raise @@ InvalidGrammar "Grammar cannot be empty!"
  | (start, _)::_ ->

      let nt_list, t_list = List.fold_left (fun (nts, ts) (nt,seq) -> 
        (Char.escaped nt)::nts,
        (List.map Char.escaped seq)@ts
      ) ([],[]) pairs in

      let nt_set = StringSet.of_list nt_list in
      let t_set = StringSet.diff (StringSet.of_list t_list) nt_set in

      let nt_alpha = StringSet.elements nt_set
                     |> Array.of_list
                     |> Alphabet.of_string_array in

      let t_alpha = StringSet.elements t_set
                    |> Array.of_list
                    |> Alphabet.of_string_array in

      let nt_sym c = Option.get @@ Alphabet.sym_of_str nt_alpha (Char.escaped c) in
      let t_sym c = Option.get @@ Alphabet.sym_of_str t_alpha (Char.escaped c) in

      let prods = List.fold_left (fun ps (nt, seq) ->
        let nt' = nt_sym nt in
        match seq with
        | [] -> { lhs=nt'; rhs=[], None }::ps
        | _ -> match List.rev seq with
               | [] -> failwith "Impossible!"
               | next::rem ->
                   match Alphabet.sym_of_str nt_alpha (Char.escaped next) with
                   | None -> { lhs=nt'; rhs=List.map t_sym seq, None }::ps
                   | Some next' ->
                      let rem' = List.map t_sym rem in
                      { lhs=nt'; rhs=List.rev rem', Some next' }::ps
      ) [] pairs in

      let start' = Option.get @@ Alphabet.sym_of_str nt_alpha (Char.escaped start) in

      {
        nonterms = nt_alpha;
        terms = t_alpha;
        start = start';
        prods;
      }
