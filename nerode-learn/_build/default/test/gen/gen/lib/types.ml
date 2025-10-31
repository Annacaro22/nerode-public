open Core

type symbol =
  char
[@@deriving sexp]

type t =
  | Empty
  | Epsilon
  | Char of symbol
  | Seq of t list
  | Union of t list
  | Star of t
  | QMark of t
  | Intersect of t list
  | Neg of t
  [@@deriving sexp]

let%expect_test "yolo" =
  print_endline "Hello World"; [%expect {| Hello World |}]

let%expect_test "sexp" =
  let rx = Seq [Empty; Epsilon; Char 'A'] in
  let s = sexp_of_t rx in
  print_s s; [%expect {|(Seq (Empty Epsilon (Char A)))|}]

module Generator = struct
  open Quickcheck
  open Generator.Let_syntax

let gen_symbol : symbol Generator.t =
  Generator.char

let gen_t : t Generator.t =
  Generator.recursive_union
    [
      return Empty;
      return Epsilon;
      (* can decide if you want these or not... *)
      return (Seq []);
      return (Union []);
      return (Intersect []);
      let%map c = gen_symbol in Char c
    ]
    ~f:(fun self -> 
      [ (let%map xs = Generator.list_non_empty self in Seq xs);
        (let%map xs = Generator.list_non_empty self in Union xs); 
        (let%map x = self in Star x);
        (let%map x = self in QMark x);
        (let%map xs = Generator.list_non_empty self in Intersect xs);
        (let%map x = self in Neg x);
      ])
  end