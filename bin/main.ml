open Lexing

let () =
  let open Match in
  let r = Star (Sum (Character 'a', Prod (Character 'a', Character 'a'))) in
  Lexer.lexer r "a" |> Option.get |> Recons.to_string |> print_endline



(* let () = test_lexer () *)
