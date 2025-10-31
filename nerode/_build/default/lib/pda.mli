(*A pushdown automata and CFG framework to capture context free languages*)

type nonterm (*a nonterminal state*)
type nontermOrTerm (*a type which encapsulates both nonterminals and terminals*)

type prod (*a type encapsulating a single production rule on the nonterminals*)

type t (*a pda*)

(*runs the pda on a given string and returns if the string is accepted or not*)
val accept : t -> Word.t -> bool

(*gets a representative string of a certain pda*)
val rep : t -> Word.t


(*CFG FUNCTIONS*)

type cfg (*a cfg*)

val input_to_cfg : string -> string -> string list -> cfg

(*converts a cfg to a pda*)
val pda_from_cfg : cfg -> t

(*convert from pda to cfg*)
val cfg_from_pda : t -> cfg

(*checks if a cfl is valid; if its nonterminals and terminals are disjoint*)
val valid_cfl : cfg -> bool

(*checks if two cfls are equivalent to each other*)
val equiv : cfg -> cfg -> bool

(*function to preprocess: if any terminals are in start state's production rules,
  move them to be of a new non-starting nonterminal and just
  let S -> P for this new nonterminal P*)
val preprocess : cfg -> cfg

(*simplifies out all terminals by making a new nonterminal for each that captures them*)
val replace_terminals : cfg -> cfg

(*takes any CFG (not necessarily in chomsky) and turns it into greibach form*)
val greibach : cfg -> cfg

(*Brzozowski derivatives*)
val e : cfg -> bool
val d : char -> cfg -> cfg

val cfg_is_empty : cfg -> bool


(*INTERFACE WITH DFAS*)

(*finds intersection of pda with dfa*)
val intersect_with_dfa : t -> Dfa.t -> t

(*checks if a pda and dfa are disjoint from each other*)
val disjoint_from_dfa : t -> Dfa.t -> bool