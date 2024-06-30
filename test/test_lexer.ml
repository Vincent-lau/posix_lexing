
open Lexing
open Match 
open Lexer 

let test_lexer () = 
  let r = Star (Sum (Character 'a', Prod (Character 'a', Character 'a'))) in
  lexer r "a" |> Option.get |> Recons.to_string |> print_endline


let tests =
  let open Alcotest in
  [
    ("basic_lexing", [test_case "basic lexing" `Quick test_lexer])

  ]
