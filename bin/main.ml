open Lexing

let _test1 () =
  let open Match in
  let r = Star (Sum (Character 'a', Prod (Character 'a', Character 'a'))) in
  Lexer.lexer r "a" |> Option.get |> Recons.to_string |> print_endline

let _test_br () =
  let open Match in
  let r1 = Sum (Character 'a', Prod (Character 'a', Character 'a')) in
  let r = Backref (r1, Star (Character 'f'), []) in
  let c = "aafffa" in
  let d = der_s r c in
  d |> Match.to_string |> print_endline;
  nullable d |> string_of_bool |> print_endline

let _test_inj_br () =
  let open Match in
  (* let r1 = Sum (Character 'a', Prod (Character 'a', Character 'a')) in *)
  let r2 = Prod (Character 'a', Character 'a') in
  (* let r3 = Character 'a' in *)
  let r = Backref (r2, Star (Character 'f'), []) in
  let c = "aafaa" in
  let d = der_s r c in
  let e = Recons.mkeps d in
  d |> Match.to_string |> print_endline;
  e |> Recons.to_string |> print_endline;
  let i = Recons.inj_s r c e in
  i |> Recons.to_string |> print_endline

let test_seq () = 
  let open Match in
  (* let r1 = Sum (Character 'a', Prod (Character 'a', Character 'a')) in *)
  (* TODO why is this seq broken 
     
   aabc \ a = 1abc
   1abc \ a = 1\aabc + abc\a = 0abc + 1bc
   0abc + 1bc \ b = 0abc \ b + 1bc \ b = abc\b + 1\bbc + bc\b = 0bc + 0bc + 1c
   0bc + 0bc + 1c \ c = 0bc \ c + 0bc \ c + 1c\c = bc\c + bc\c + 1\cc + c\c = 0c + 0bc + 0c + 1

   mkeps (0c + 0bc + 0c + 1) = R R R Empty

  inj a.abc c (R R R Empty) = Seq (mkeps a) (inj abc c R R Empty)


  
  *)
  let r2 = Prod (Character 'a', Prod (Character 'a', Prod (Character 'b', Character 'c'))) in
  let c = "aabc" in
  let d = der_s r2 c in
  d |> Match.to_string |> print_endline;
  let e = Recons.mkeps d in
  e |> Recons.to_string |> print_endline;
  let i = Recons.inj_s r2 c e in
  i |> Recons.to_string |> print_endline



let () = test_seq ()
