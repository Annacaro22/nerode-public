open Core
type symbol = Alphabet.symbol
type word = Word.t

type nonterm = |Char of char

type nontermOrTerm = |Char of char | Eps

type transy = (int * nontermOrTerm * nonterm) * (int * nontermOrTerm list)

type prod = nonterm * (nontermOrTerm list)

type state = int

module StateSet = Set.Make(Int)

type t = {
  states: StateSet.t;
  alpha: char list;
  stack_alpha: char list;
  trans : transy list;
  start_state : int;
}


let push_onto (l : nontermOrTerm list) (stack : char Stack.t) =
  let rev = List.rev l in
  List.iter rev ~f:(fun nonterm -> match nonterm with
    |Char c ->  Stack.push stack c
    | _ -> ())

let step (pda: t) (stack : char Stack.t) (a: char) =
  let stack_top = Stack.pop_exn stack in
  let next_step = List.filter (pda.trans) ~f:(fun (x,y) -> match x with
    |(0,Char c,Char st) -> if (Stdlib.compare c a = 0 && Stdlib.compare st stack_top = 0)
        then true else false
    | _ -> false) in
  match next_step with
  | [] -> false
  | _  -> let output = List.nth_exn next_step 0 in
          let (inn, outt) = output in
          let (state, pushing) = outt in (let _ = Stack.pop stack in (); push_onto pushing stack; true)

let run_pda (pda: t) (input : char list) =
  let stack = Stack.create () in
  let has_next_step = List.map input ~f:(step pda stack) in
  match List.filter has_next_step ~f:(fun x -> not x) with
  |[] -> if Stack.is_empty stack then true else false (*the string runs fully thru the pda
  and we check if we've emptied the stack at the end or not; return true (accept) iff empty stack*)
  | _ -> false (*at some point in the running of the string, 'step' returns false, meaning there is no
  transition you can continue with, so automatic reject*)

let charList_to_alpha (l : char list) : Alphabet.t =
  let string_list = List.map l ~f:(fun char -> Char.escaped char) in
  Alphabet.of_string_array (Array.of_list string_list)

let alpha_to_charlist (a : Alphabet.t) : char list =
  List.map (Array.to_list (Alphabet.to_string_array a)) ~f:(fun str -> String.get str 0)

let accept (pda: t) (w: word) =
  let alpha = pda.alpha in
  let input = Word.to_string (charList_to_alpha alpha) w in
  let input_charList = List.init (String.length input) ~f:(String.get input) in
  run_pda pda input_charList


let nonterm_compare (a: nonterm) (b: nonterm) = let Char x = a in let Char y = b in
  if Stdlib.compare x y = 0 then true else false

let get_trans (pda:t) (a: char) (n: nonterm) = 
  List.filter_map (pda.trans) ~f:(fun trans -> let ((_, b, m), (_, l)) = trans in
  match b with
    |Char x -> if Stdlib.compare x a = 0 then 
      (if (nonterm_compare n m) then Some l else None) else None
    |_ -> None)

let top_of_stack (stacky:nontermOrTerm list list) : nonterm option = match stacky with
  |[stack] -> (match (List.nth_exn stack 0) with
                | Char c -> Some (Char c)
                | _ -> None)
  | _ -> None


let next_steps (pda:t)(n:nonterm) : (symbol * nonterm) list = 
  let nexty : (char*nonterm) list =
    List.map pda.alpha ~f:(fun a -> match (top_of_stack (get_trans pda a n)) with
      |Some nont -> (a, nont)
      |_ -> failwith "terminal on stack") in
  List.map nexty ~f:(fun (c,n) ->
    let i,alph = (List.findi_exn (pda.alpha) ~f:(fun i2 y -> Stdlib.compare y c = 0)) in 
    ((Alphabet.sym_of_int i) , n))
  

(*essentially the nonterm on the top of the stack IS our state*)
let rec rep_helper (pda: t) (s: (word*nonterm) list) (v: nonterm list) =
      match s with
      | (w,q)::tail -> if accept pda w then w
                       else
                         let tr: (symbol * nonterm) list = next_steps pda q in
                         let next = List.filter_map tr ~f:(fun (a, qa) -> 
                           if (List.exists v ~f:(nonterm_compare qa)) then
                             None
                           else
                             Some (w@[a], qa)
                         ) in
                         let v' = List.fold next ~init:v ~f:(fun a (_,n) -> a@[n]) in
                         rep_helper pda (tail@next) v'
      | _ -> failwith "rep: Stack unexpectedly emptied"

let pdaword_to_tord (w : word) : symbol list = w

let rep (pda: t) : symbol list = rep_helper pda [([],Char (List.nth_exn pda.stack_alpha 0))] []





(*CFG STUFF*)


type cfg = {    nonterms: char list;
                terminals: char list;
                prods : prod list}


let input_to_cfg (terms:string) (nonterms:string) (rules: string list) : cfg =
  let t = String.to_list terms in
  let n = String.to_list nonterms in
  let prods = List.map rules ~f:(fun s -> 
    let clist = String.to_list s in
      match clist with
        |h :: t ->
          let nont : nonterm = Char h in 
          let res : nontermOrTerm list = List.map t ~f:(fun x -> Char x) in
          (nont, res)
        | _ -> failwith "no rule") in
  let cfg = {nonterms = n; terminals = t; prods = prods} in cfg


let prod_to_trans (prod : prod) =
  let nonterm, list = prod in
  let character = List.nth_exn list 0 in
  let push = List.tl_exn list in
  ((0, character, nonterm), (0, push))

(*Assuming given cfg prods are already in greibach normal form*)
let pda_from_cfg (g: cfg) =
  let transitions = List.map g.prods ~f:prod_to_trans in
  let p = {states = StateSet.singleton 0; alpha = g.terminals; stack_alpha = g.nonterms;
  trans = transitions; start_state = 0} in p

let cfg_from_pda (pda : t) : cfg = 
  let rules = List.map pda.trans ~f:(fun (x,y) -> let _, ch, nonterm = x in
  let _, list = y in
  (nonterm, ((ch) :: (list)))) in
  {nonterms = pda.stack_alpha; terminals = pda.alpha; prods = rules}


let nonterm_to_nontermOrTerm (x:nonterm) : nontermOrTerm = let Char c = x in Char c

let char_to_nonterm (c:char) : nonterm = Char c

let char_to_nontermOrTerm (c:char) : nontermOrTerm = Char c


let is_disjoint (l1:char list) (l2:char list) =
  let overlaps = List.filter (List.map l1 ~f:(fun x -> List.exists l2 ~f:(fun y -> Stdlib.compare x y = 0))) ~f:(fun x -> x) in
  if List.length overlaps = 0 then true else false


let valid_cfl c =
  not (is_disjoint (c.nonterms) (c.terminals))


let terminal_compare (t1:nontermOrTerm) (t2:nontermOrTerm) = match t1 with
  |Eps -> (match t2 with |Eps -> 0 |_ -> 1)
  |_ -> (match t2 with
          | Eps -> 1
          | _ -> Stdlib.compare t1 t2)

let rule_compare (r1: nonterm * nontermOrTerm list) (r2 : nonterm * nontermOrTerm list) =
  let n1, arr1 = r1 in
  let n2, arr2 = r2 in
  if not (let Char char1 = n1 in let Char char2 = n2 in Stdlib.compare char1 char2 = 0) then 1 else
    if List.compare (terminal_compare) arr1 arr2 = 0 then 0 else 1
let prod_compare (p1: (nonterm * nontermOrTerm list) list) (p2: (nonterm * nontermOrTerm list) list) =
  List.compare (rule_compare) p1 p2
let equiv (c1:cfg) (c2:cfg) =
  if not (List.compare (Stdlib.compare) c1.nonterms c2.nonterms = 0) then false else
  if not (List.compare (Stdlib.compare) c1.terminals c2.terminals = 0) then false else
    if prod_compare c1.prods c2.prods = 0 then true else false


let char_compare_bool (a:char)(b:char) : bool=
    if (Stdlib.compare a b) = 0 then true else false

let isnonTerminal (c:cfg) (x: nontermOrTerm) : bool =
  match x with
    |Eps -> false
    |Char x1 -> if (List.mem (c.nonterms) x1 ~equal:char_compare_bool) then true else false

let isnonTerminal_opt (c:cfg) (x: nontermOrTerm) : nonterm option =
  match x with
    |Eps -> None
    |Char x1 -> if (List.mem (c.nonterms) x1 ~equal:char_compare_bool) then Some (Char x1) else None

let justTerminals (c:cfg) (xs: nontermOrTerm list) : bool =
  match (List.filter xs ~f:(isnonTerminal c)) with
    |[] -> true
    |_ -> false

let rec findNewChar (candidate: char) (nonterms: char list) (terminals: char list) : char =
  if (List.mem nonterms candidate ~equal:(fun x y -> if Stdlib.compare x y = 0 then true else false)) then
    findNewChar (Char.of_int_exn ((Char.to_int candidate) + 1)) nonterms terminals
  else
    if (List.mem terminals candidate ~equal:(fun x y -> if Stdlib.compare x y = 0 then true else false)) then
      findNewChar (Char.of_int_exn ((Char.to_int candidate) + 1)) nonterms terminals
      else candidate

let makeNewChar (nonterms: char list) (terminals: char list) : char =
  let sorted = List.sort nonterms ~compare:(Stdlib.compare) in
  let final = List.nth_exn sorted (List.length sorted - 1) in
  let finalint = Char.to_int final in
  let newCandidate = Char.of_int_exn (finalint + 1) in findNewChar newCandidate nonterms terminals

let rec makeNewChars (num: int) (nonterms: char list) (terminals: char list) (acc : char list) : char list =
  match num with
    |0 -> acc
    |_ -> let newbie = makeNewChar nonterms terminals in
            makeNewChars (num-1) (nonterms @ [newbie]) terminals (acc @ [newbie])

(*function to preprocess: if any terminals are in start state's production rules, move them to be of a new non-starting nonterminal and just
  let S -> P for this new nonterminal P*)
let preprocess (c:cfg) :cfg =
  let nontermy = c.nonterms in
  let termy = c.terminals in  
  let startRules = List.filter c.prods
    ~f:(fun (nont,rule) -> let Char sym = nont in if char_compare_bool sym '0' then true else false) in
  let needsEditing = List.filter (startRules) ~f:(fun (nont,rule) -> justTerminals c rule) in
  let newVariables = makeNewChars (List.length needsEditing) (nontermy) (termy) [] in
  let newNonterms = nontermy @ newVariables in
  let unchanged = List.filter c.prods
    ~f:(fun rule -> if List.mem needsEditing rule ~equal:(fun x y -> if rule_compare x y = 0 then true else false)
        then false else true) in
  let addedProds:(nonterm * nontermOrTerm list) list =
    List.concat (List.map2_exn needsEditing newVariables ~f:(fun prod var -> let start, rule = prod in [(start, [Char var]);(Char var, rule)])) in
  let newProds:(nonterm * nontermOrTerm list) list = addedProds @ unchanged in
  let c2 = {nonterms = newNonterms; terminals = c.terminals; prods = newProds} in c2


let filter_out_existing (list: prod list) (total: prod list) : prod list =
  List.filter list
    ~f:(fun (x:prod) -> if (List.exists total 
      ~f:(fun (y:prod) -> if rule_compare x y = 0 then true else false))
    then false else true)

let nonterm_compare (a: nonterm) (b: nonterm) = let Char x = a in let Char y = b in
  if Stdlib.compare x y = 0 then true else false

let nontermOrTerm_nonterm_compare (t1:nontermOrTerm) (t2:nonterm) = if (match t1 with
|Eps -> (1)
|Char x -> (let Char y = t2 in Stdlib.compare x y)) = 0  then true else false


let nontermOrTerm_compare (t1:nontermOrTerm) (t2:nontermOrTerm) = if (match t1 with
  |Eps -> (match t2 with |Eps -> 0 |_ -> 1)
  |_ -> (match t2 with
          | Eps -> 1
          | _ -> Stdlib.compare t1 t2)) = 0 then true else false

let collect_rules (rules: prod list) (nonterm: nonterm) =
  List.filter rules ~f:(fun prod -> let nont, rule = prod in nonterm_compare nont nonterm)

let epsilon_transitions (curr : prod list) = List.filter curr
~f:(fun prod -> let nont, rule = prod in match rule with |[Eps] -> true | _ -> false)
let unit_transitions (curr : prod list) (cfg:cfg)= List.filter curr
~f:(fun prod -> let nont, rule = prod in match rule with
  |[Char x] -> isnonTerminal cfg (Char x)
  | _ -> false)

let find_epsilon_newprods (curr : prod list) (cfg:cfg) : prod list = 
  let epsilon_nonterms : nonterm list = List.map (epsilon_transitions curr)
    ~f:(fun prod -> let nont, rule = prod in nont) in
  let eps_to_edit_ll : prod list list = List.map epsilon_nonterms
    ~f:(fun nont -> List.filter curr ~f:(fun prod -> let nonty,rule = prod in
    (List.exists rule ~f:(fun elt -> match isnonTerminal_opt cfg elt with
      |Some elty -> nonterm_compare nont elty
      |None -> false)))) in
  let eps_to_edit : prod list = List.fold eps_to_edit_ll ~init:[] ~f:(fun l1 l2 -> l1 @ l2) in
  let eps_new_prods_all_ll : prod list list = List.map eps_to_edit ~f:(fun prod -> let nont, rule = prod in
    let to_do = List.filter rule ~f:(fun elt -> List.exists epsilon_nonterms
      ~f:(fun nonty -> nontermOrTerm_nonterm_compare elt nonty)) in List.map to_do
        ~f:(fun n -> (nont, List.filter rule ~f:(fun ntot -> not (nontermOrTerm_compare ntot n))))) in
  List.fold eps_new_prods_all_ll ~init:[] ~f:(fun l1 l2 -> l1 @ l2)

let find_unit_newprods (curr: prod list) (cfg:cfg) : prod list =
  let unit_pairs : ((nonterm * nonterm)) list = List.filter_map curr
    ~f:(fun prod -> let nont, rule = prod in match rule with
      |[Char x] -> (match isnonTerminal_opt cfg (Char x) with
          |Some nontyy -> Some (nont, nontyy)
          |None -> None)
      | _ -> None) in
  let unit_new_prods_all_ll : (prod) list list= List.map unit_pairs
  ~f:(fun (a,b) -> List.map (collect_rules curr b) ~f:(fun prod -> let n, r = prod in (a,r))) in
  List.fold unit_new_prods_all_ll ~init:[] ~f:(fun l1 l2 -> l1 @ l2)


let rec add_to_fp (curr : prod list) (cfg:cfg) : prod list =
  let eps_new_prods : prod list = filter_out_existing (find_epsilon_newprods curr cfg) curr in
  let unit_new_prods : prod list = filter_out_existing (find_unit_newprods curr cfg) curr in
  let new_prods = eps_new_prods @ unit_new_prods in

  match new_prods with
  | [] -> curr
  | _ -> add_to_fp (curr @ eps_new_prods @ unit_new_prods) cfg


let delete_epsilon_unit (l: prod list) (cfg:cfg) : prod list =
  let added_to_fixed_point = add_to_fp l cfg in
  let epsilons = epsilon_transitions l in
  let units = unit_transitions l cfg in
  let removed_eps = List.filter added_to_fixed_point
    ~f:(fun prod -> List.exists epsilons
      ~f:(fun prod2 -> if (rule_compare prod prod2) = 0 then true else false)) in
  let removed_units = List.filter removed_eps
    ~f:(fun prod -> List.exists units
      ~f:(fun prod2 -> if (rule_compare prod prod2) = 0 then true else false)) in
  removed_units





let swapNontermOrTerms (rule: nontermOrTerm list) (newNonterm : char) (replace : nontermOrTerm): nontermOrTerm list =
  List.map rule ~f:(fun x -> match x with
      |Eps -> (match replace with
          |Eps -> (Char newNonterm)
          | _ ->  Eps)
      |Char y -> (match replace with
          | Eps -> x
          | Char z -> if (Stdlib.compare z y) = 0 then (Char newNonterm) else x))

let get_nonterm_for_terminal (term : nontermOrTerm) (prods : prod list): char =
  let correct = List.filter prods ~f:(fun (n,l) -> match l with
    |[term] -> true
    | _ -> false) in
  match correct with
    |[(n, [x])] -> let Char y = n in y
    | _ -> failwith ("terminal mapped to by 0 or >= 2 things")


(*for each terminal a, adds a new nonterminal A and a rule A->a, and changes all appearances of a in RHS of rules to A*)
let replace_terminals (c:cfg) :cfg =
  let added_nonterms = makeNewChars ((List.length c.terminals) + 1) (c.nonterms) (c.terminals) [] in
  let new_nonterms = c.nonterms @ added_nonterms in
  let new_terminals = c.terminals in
  let terms_plus_eps : nontermOrTerm list = (List.map c.terminals ~f:(char_to_nontermOrTerm)) @ [Eps] in
  let added_prods : prod list = List.map2_exn added_nonterms terms_plus_eps ~f:(fun n t -> (char_to_nonterm n, [t])) in
  let includes_t (t: nontermOrTerm) (l : nontermOrTerm list) : bool = 
    List.exists l ~f:(fun x -> (Stdlib.compare x t) = 0) in
  let edited_prods : prod list = List.map2_exn terms_plus_eps c.prods
    ~f:(fun t (n,l) -> match includes_t t l with
        |true -> (n, swapNontermOrTerms l (get_nonterm_for_terminal t added_prods) t)
        |false -> (n,l) ) in
  let new_prods = edited_prods @ added_prods in
  let c2 :cfg = {nonterms = new_nonterms; terminals = new_terminals; prods = new_prods} in c2


(*let chomsky (c:cfg) :cfg =
  let remove_eps_unit = {nonterms = c.nonterms;terminals=c.terminals;prods = delete_epsilon_unit (c.prods) c} in
  let fix_terminals = replace_terminals remove_eps_unit in
  (*need to, for every A -> BCD, make A -> BE, E -> CD*)*)

let rules_of_nonterm (l : prod list) (n : nonterm) =
  List.filter l ~f:(fun (n2,l2) -> nonterm_compare n n2)

let has_recursion ((n,l) : prod) =
  let Char x = n in match (List.nth_exn l 0) with
    | Eps -> false
    | Char y -> Stdlib.compare x y = 0

let nonterm_has_recursion (c:cfg) (n: nonterm)=
  List.exists (rules_of_nonterm c.prods n) ~f:(has_recursion)

(*For every nonterm A that has recursive rules, we make a new nonterm X such that X -> alpha | alphaX for all rules of A
such that A -> Aalpha, and change the rules of A to be A -> beta | betaX for all nonrecursive rules of A beta.
All other prod rules stay unchanged.*)
let manage_recursion (c:cfg) :cfg = 

  let newpair = List.fold_left c.nonterms ~init:(c.nonterms,c.prods)
  ~f:(fun (currnonterms, currprods) n -> match nonterm_has_recursion c (Char n) with
    |true -> 
      let newX : char = makeNewChar (currnonterms) (c.terminals) in
      let newnonterms = currnonterms @ [newX] in

      let recurs = List.filter (rules_of_nonterm currprods (Char n)) ~f:(has_recursion) in
      let remove_first = List.map recurs ~f:(fun (n,l) -> match l with
        |h :: t -> (char_to_nonterm newX,t)
        | _ -> failwith ("not actually recursive")) in
      let recurs_remove1st_with_X = List.map remove_first ~f:(fun (n,l) -> (n,l @ [Char newX])) in
      let allXrules = remove_first @ recurs_remove1st_with_X in

      let nonrecurs = List.filter (rules_of_nonterm currprods (Char n)) ~f:(fun x -> not (has_recursion x)) in
      let nonrecurs_plusX = List.map nonrecurs ~f:(fun (n,l) -> (n,l @ [Char newX])) in
      let allArules = nonrecurs @ nonrecurs_plusX in

      let unchanged_rules = List.filter currprods ~f:(fun (n2,l2) -> not (nonterm_compare (Char n) n2)) in
      let new_rules_all = allArules @ allXrules @ unchanged_rules in (newnonterms, new_rules_all)
    |false -> (currnonterms, currprods)
  ) in
  let newnonterms, newprods = newpair in

  let c2 = {nonterms = newnonterms; terminals = c.terminals; prods = newprods} in c2


let get_index (l : char list) (x : char) =
  match (List.filter (List.mapi l ~f:(fun i c -> if ((Stdlib.compare c x) = 0) then i else -1)) ~f:(fun inty -> inty >= 0)) with
    |h :: t -> h
    | _ -> failwith ("not nonterminal")

let nonterms_greater_than (lhs : nonterm) (list_head : nontermOrTerm) (nonterms : char list)=
  match list_head with
    |Eps -> failwith ("cannot be terminal")
    |Char y -> let Char x = lhs in (get_index nonterms x > get_index nonterms y)
        

let rec substitute_greater_thans (c :cfg) (prodsnotx : prod list) =
  let newrules_notx : prod list = List.fold_left prodsnotx ~init:(c.prods) ~f:(fun currprods (n,l) -> 
    match (nonterms_greater_than n (List.nth_exn l 0) c.nonterms) with
      |true -> 
        let larger  = match (List.nth_exn l 0) with |Eps -> failwith ("not nonterminal") |Char x -> x
      in
        let substitutes = List.map (rules_of_nonterm currprods (Char larger))
          ~f:(fun (n2,l2) -> match l with | h :: t -> (n, l2 @ t) | _ -> failwith "not actually greater than") in
        let unchanged = List.filter currprods
          ~f:(fun (n3,l3) -> rule_compare (n,l) (n3,l3) = 1 ) in
        let allnewprods = unchanged @ substitutes in
        allnewprods
      |false -> currprods) in
  let x_rules = List.filter (c.prods)
    ~f:(fun rule -> not (List.mem prodsnotx rule ~equal:(fun a b -> rule_compare a b = 0))) in 
  let newrules = newrules_notx @ x_rules in
  let c2 = {nonterms = c.nonterms; terminals = c.terminals; prods = newrules} in 
  if (List.for_all newrules ~f:(fun (n,l) -> not (nonterms_greater_than n (List.nth_exn l 0) c.nonterms))) then c2 else
  substitute_greater_thans (c2) (prodsnotx)


let thread_through (c:cfg) =
  let newprods = 
  List.map (c.prods) ~f:(fun (n,l) -> 
    let first = List.nth_exn l 0 in
    match (isnonTerminal_opt c first) with
      |None -> (n,l)
      |Some x ->
        let term_rule = List.find_exn c.prods ~f:(fun (n2,l2) -> nontermOrTerm_compare (nonterm_to_nontermOrTerm n2) first) in
        let (_, termy) = term_rule in 
        match termy with 
          |[Char z] -> (n, (swapNontermOrTerms l z first))
          |_ -> failwith "not a single terminal; fix terminals or other procedure must have failed"   
    ) in
  let c2 = {nonterms = c.nonterms; terminals = c.terminals; prods = newprods} in c2 




let greibach (c:cfg) :cfg =
  let remove_eps_unit = {nonterms = c.nonterms;terminals=c.terminals;prods = delete_epsilon_unit (c.prods) c} in
  let fix_terminals = replace_terminals remove_eps_unit in
  (*now we have essentially chomsky form but with allowing for more than 2 nonterms on a RHS*)
  let removed_recursion = manage_recursion fix_terminals in

  let nonterms_without_xs = fix_terminals.nonterms in
  let prods_without_xs = List.filter (removed_recursion.prods)
    ~f:(fun (n,l) -> let Char z = n in List.mem nonterms_without_xs z ~equal:(fun x y -> Stdlib.compare x y = 0)) in
    (*exists nonterms_without_xs ~f:(fun y -> let Char z = n in ((Stdlib.compare z y) = 0))*)
  let fix_ges = substitute_greater_thans (removed_recursion) (prods_without_xs) in
  let finally = thread_through fix_ges in finally

  


(* --- Brzozowski derivatives --- *)

(*whether it accepts the empty string; aka if the start state goes to epsilon OR if it goes to some other nonterm which goes to epislon.*)
let e (c:cfg) : bool =
  if (*if S -> eps is a rule*)
  (List.exists c.prods ~f:(fun x -> let n,s = x in 
    if (List.compare (terminal_compare) s [Eps] = 0) then (let Char char1 = n in if Stdlib.compare char1 '0' = 0 then true else false) else false))
  then true else
  if (*if P -> eps and S -> P (for S \neq P) are rules*)
  (List.exists c.prods ~f:(fun x -> let n,s = x in let Char char1 = n in
    if (List.compare (terminal_compare) s [Eps] = 0) then (if not (Stdlib.compare char1 '0' = 0) then 
      (if (List.exists c.prods ~f:(fun y -> let n2,s2 = y in
        (if (List.compare (terminal_compare) s2 [nonterm_to_nontermOrTerm n] = 0) then
          (let Char char2 = n2 in if (Stdlib.compare char2 '0' = 0) then true else false) else false))) then true else false)
      else false) else false))
  then true else false



let containsS (rule: nontermOrTerm list) (nontermy : char list) : bool =
  match (List.filter rule ~f:(fun x -> match x with
      |Eps -> false
      |Char y -> if (Stdlib.compare (List.nth_exn nontermy 0) y) = 0 then true else false)) with
  |[] -> true
  |_ -> false

let swapSR (rule: nontermOrTerm list) (newNonterm : char) (nontermy : char list): nontermOrTerm list =
  List.map rule ~f:(fun x -> match x with
      |Eps -> x
      |Char y -> if (Stdlib.compare (List.nth_exn nontermy 0) y) = 0 then (Char newNonterm) else x)

(*the strings generated by slicing char c off the front of some string in in CFL c0*)
(*assume that it has been preproccessed as in the function preprocess above. then, make a new
nonterminal R, copy S over to R (making sure that self loops now refer to R instead of S), and let S be all rules that start with
the char c but without c itself. everything else can stay the same.*)
let d (c:char) (c0:cfg) :cfg =
  let pre = preprocess c0 in
  let nontermy = pre.nonterms in
  let termy = pre.terminals in
  (*make new nonterminal, 'R', and add it to our list of nonterms:*)  
  let newNonterm = makeNewChar nontermy termy in
  let newNonterms = nontermy @ [newNonterm] in
  (*Rules: let R -> all rules that were for S before, with S self loops swapped for R self loops*)
  let startRules = List.filter pre.prods
    ~f:(fun (nont,rule) -> let Char sym = nont in if char_compare_bool sym '0' then true else false) in
  let addedRRules : prod list = let rNonterm : nonterm = Char newNonterm in
    List.map startRules ~f:(fun prod -> let start, rule = prod in 
    match containsS rule nontermy with
      |true -> (rNonterm, swapSR rule newNonterm nontermy)
      |false -> (rNonterm, rule)
    ) in
  (*Rules: let S -> all S rules that started with c but with the c itself cut off*)
  let startsWithC = List.filter startRules ~f:(fun prod -> let start, rule = prod in
    match List.nth_exn rule 0 with
      |Char c -> true
      |_ -> false) in
  let removedC = List.map startsWithC ~f:(fun prod -> let start, rule = prod in (start, List.tl_exn rule)) in
  let addedSRules = removedC in
  (*put everything together*)
  let unchanged = List.filter pre.prods
  ~f:(fun rule -> if List.mem startRules rule ~equal:(fun x y -> if rule_compare x y = 0 then true else false)
      then false else true) in
  let newProds = addedSRules @ addedRRules @ unchanged in
  let c2 = {nonterms = newNonterms; terminals = pre.terminals; prods = newProds} in c2


let rule_terms (rule: prod) (terminators : char list) =
  List.exists (let nont, list = rule in list)
  ~f:(fun x -> match x with
  |Eps -> true
  |Char c -> List.exists terminators ~f:(fun y -> (Stdlib.compare y c) = 0))

let rec empty_helper (rules: prod list) (terminators : char list) (start: char) =
  if List.exists terminators ~f:(fun x ->(Stdlib.compare x start) = 0) then true else
    let findterminators = List.map (List.filter rules ~f:(fun x -> rule_terms x terminators))
    ~f:(fun rule -> let nonterm, list = rule in let Char c = nonterm in c) in
    empty_helper rules (terminators @ findterminators) start
let cfg_is_empty (c:cfg) : bool = empty_helper (c.prods) (c.terminals) (List.nth_exn c.nonterms 0)


















(*INTERFACE WITH DFAS*)

let rec cross_product (l1: char list) (l2: char list) =
  match l1 with
  | [] -> failwith "empty list"
  | [h] -> List.map l2 ~f:(fun c -> (h,c))
  | h :: t -> List.map l2 ~f:(fun c -> (h,c)) @ cross_product t l2

let rec char_list_union (l1: char list) (l2: char list) =
  match l1 with
  | [] -> l2
  | h :: t -> h :: char_list_union (List.filter t ~f:(fun x -> not (Stdlib.compare x h = 0))) 
  (List.filter l2 ~f:(fun x -> not (Stdlib.compare x h = 0)))


let combine_one_dfa_trans (dfaTrans: state array array) (pda: t)
(pdaTrans : transy list)
(pair : state * int)=
  let st, c = pair in
  let char = List.nth_exn pda.alpha c in
  let outstate = Array.get (Array.get dfaTrans c) st in
  let applicable = List.filter pdaTrans ~f:(fun (x,y) -> let (stat, ch, stak) = x in 
  (match ch with
  |Eps -> false
  |Char chh -> if (Stdlib.compare chh char = 0) then true else false)) in
  List.map applicable ~f:(fun (x,y) -> let (_, _, stak) = x in 
  let (_, outstack) = y in
  ((st, (Char char), stak),(outstate, outstack)))

let rec list_union (l1: transy list)
(l2: transy list) =
  match l1 with
  | [] -> l2
  | h :: t -> h :: list_union (List.filter t ~f:(fun x -> not (Stdlib.compare x h = 0))) 
  (List.filter l2 ~f:(fun x -> not (Stdlib.compare x h = 0)))

let get_intersect_transitions
(pdaTrans : transy list)
(dfaTrans : state array array) (pda: t) =
let dim1 = List.init (Array.length dfaTrans) ~f:(fun x -> x) in
let dim2 = List.init ((Array.length (Array.get dfaTrans 0))) ~f:(fun x -> x) in
let min = if List.length dim1 < List.length dim2 then dim1 else dim2 in (*THIS IS A BAND AID PROBABLY MESSING OTHER STUFF UP*)
let array = List.map2_exn min min ~f:(fun x y -> (x,y)) in
let listlist = List.map array ~f:(combine_one_dfa_trans dfaTrans pda pdaTrans) in
let noneps = List.fold_left listlist ~init:([]) ~f:list_union in
let epsilon_transitions = List.filter pdaTrans ~f:(fun (x,y) -> let _,ch,_ = x in 
match ch with |Eps -> true |_ -> false) in
noneps @ epsilon_transitions


(*might be issues using Alphabet.t vs char list-- will we be able to recognize which
elements of the alphabet are shared and which are not? e.g. a^nb^n and a*b* should have
nonempty intersection but a^nb^n and c*d* should have empty intersection*)
let intersect_with_dfa (pda: t) (dfa: Dfa.t) : t =
  let state_num = Array.length (Array.get (Dfa.trans dfa) 0) in
  let states = StateSet.of_list (List.init state_num ~f:(fun x -> x)) in
  let alpha = char_list_union pda.alpha (alpha_to_charlist (Dfa.alpha dfa)) in
  let stack_alpha = pda.stack_alpha in
  let start_state = 0 in
  let trans = get_intersect_transitions pda.trans (Dfa.trans dfa) pda in
  let intersect = {states = states ; alpha = alpha; stack_alpha = stack_alpha; trans = trans;
 start_state = start_state} in intersect
  



(*let rule_terms (rule: prod) (terminators : char list) =
  List.exists (let nont, list = rule in list)
  ~f:(fun x -> match x with
  |Eps -> true
  |Char c -> List.exists terminators ~f:(fun y -> (Stdlib.compare y c) = 0))

let rec empty_helper (rules: prod list) (terminators : char list) (start: char) =
  if List.exists terminators ~f:(fun x ->(Stdlib.compare x start) = 0) then true else
    let findterminators = List.map (List.filter rules ~f:(fun x -> rule_terms x terminators))
    ~f:(fun rule -> let nonterm, list = rule in let Char c = nonterm in c) in
    empty_helper rules (terminators @ findterminators) start
let cfg_is_empty (c: cfg) : bool = empty_helper (c.prods) (c.terminals) (List.nth_exn c.nonterms 0)*)


 let disjoint_from_dfa (pda: t) (dfa: Dfa.t) : bool =
  cfg_is_empty (cfg_from_pda (intersect_with_dfa pda dfa))
