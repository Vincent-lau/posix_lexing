open Lexing

let _zero_nullable () =
  let open Match in
  nullable Zero |> string_of_bool |> print_endline

let _der_a () =
  let open Match in
  let r = Star (Sum (Character 'a', Prod (Character 'a', Character 'a'))) in
  let d = der r 'a' in
  nullable d |> string_of_bool |> print_endline

let _val_to_string () =
  let open Recons in
  let v = Seq (Char 'a', Left (Char 'b')) in
  to_string v |> print_endline




(* let () = test_lexer () *)
