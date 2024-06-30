open Lexing

let zero_nullable () =
  let open Match in
  nullable Zero |> string_of_bool |> print_endline

let der_a () =
  let open Match in
  let r = Star (Sum (Character 'a', Prod (Character 'a', Character 'a'))) in
  let d = der r 'a' in
  Alcotest.(check bool) "(a+aa)*\\a is nullable" true (nullable d);
  Alcotest.(check bool)
    "derivative value match (1+1a).(a+aa)*" true
    (d
    = Prod
        ( Sum (One, Prod (One, Character 'a'))
        , Star (Sum (Character 'a', Prod (Character 'a', Character 'a'))) ))

let test_mkeps () =
  let open Match in
  let r =
    Prod
      ( Sum (One, Prod (One, Character 'a'))
      , Star (Sum (Character 'a', Prod (Character 'a', Character 'a'))) )
  in
  let e = Recons.mkeps r in
  Alcotest.(check bool)
    "mkeps (1+1a).(a+aa)* = Seq ((Left Empty), (Stars []))" true
    (e = Seq (Left Empty, Stars []))

let test_lexer () =
  let open Match in
  let r = Star (Sum (Character 'a', Prod (Character 'a', Character 'a'))) in
  Alcotest.(check bool)
    "inj (1+1a).(a+aa)* a Seq(Left Empty, Stars [])" true
    (Lexer.lexer r "a" = Some (Stars [ Left (Char 'a') ]))

let () =
  let open Alcotest in
  run "Lexer"
    [
      ("nullable", [ test_case "zero is nullable" `Quick zero_nullable ])
    ; ("derivative", [ test_case "(a+aa)*\\a derivative" `Quick der_a ])
    ; ("mkeps test", [ test_case "mkeps (1+1a).(a+aa)*" `Quick test_mkeps ])
    ; ("basic_lexing", [ test_case "basic lexing" `Quick test_lexer ])
    ]
