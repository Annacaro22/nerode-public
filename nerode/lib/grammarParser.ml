(** Parser for right linear grammars. *)

include Nice_parser.Make(struct
  type result = Rlcfg.t
  type token = Cfg_parser.token
  exception ParseError = Cfg_parser.Error
  let parse = Cfg_parser.cfg_eof
  include Cfg_lexer
end)
