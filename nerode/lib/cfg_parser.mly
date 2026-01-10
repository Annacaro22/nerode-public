%token TO OR EOF NL
%token <char> CHAR

%type <char list> seq
%type <char list list> alts
%type <(char * (char list)) list> rule
%start <Rlcfg.t> cfg_eof

%%

cfg_eof:
  | rs=rules; EOF { Rlcfg.of_pairs (List.rev rs) }
  | rs=rules; NL; EOF { Rlcfg.of_pairs (List.rev rs) }
  ;

rules:
  | rs=rules; NL; r=rule { r @ rs }
  | r=rule { r }
  ;

rule:
  | c=CHAR; TO; rhs=alts { List.map (fun r -> (c, r)) rhs }
  ;

alts:
  | s=seq { [List.rev s] }
  | seqs=alts; OR; s=seq { (List.rev s)::seqs }
  ;

seq:
  | s=seq; c=CHAR { c::s }
  | { [] }
  ;


%%
