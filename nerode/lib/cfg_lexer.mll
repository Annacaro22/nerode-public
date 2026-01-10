{
open Cfg_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'

rule next_token = parse
  | whitespace+
    { next_token lexbuf }
  | newline { (*Printf.printf "NL\n%!";*) NL }

  (* actual tokens here *)
  | "->" { (*Printf.printf "TO\n%!";*) TO }
  | '|'  { (*Printf.printf "OR\n%!";*) OR }

  (* EOF/illegal token *)
  | eof { (*Printf.printf "EOF\n%!";*) EOF }
  | _ as c { (*Printf.printf "%c\n%!" c;*) CHAR c }
