type r =
  | Zero
  | One
  | Character of char
  | Sum of r * r
  | Sum3 of r * r * r
  | Prod of r * r
  | Star of r
  | Ntimes of int * r
  | Backref of r * r * char list (* Backref (r, r2)  s s2 s *)

let rec to_string = function
  | Zero -> "0"
  | One -> "1"
  | Character c -> Printf.sprintf "%c" c
  | Sum (r1, r2) -> Printf.sprintf "(%s + %s)" (to_string r1) (to_string r2)
  | Sum3 (r1, r2, r3) ->
      Printf.sprintf "Sum3(%s , %s , %s)" (to_string r1) (to_string r2)
        (to_string r3)
  | Prod (r1, r2) -> Printf.sprintf "(%s . %s)" (to_string r1) (to_string r2)
  | Star r -> Printf.sprintf "%s*" (to_string r)
  | Ntimes (n, r) -> Printf.sprintf "%s{%d}" (to_string r) n
  | Backref (r, r2, cs) ->
      Printf.sprintf "Backref( %s, %s, %s)" (to_string r) (to_string r2)
        (cs |> List.to_seq |> String.of_seq)

let rec nullable = function
  | Zero -> false
  | One -> true
  | Character _ -> false
  | Sum (r1, r2) -> nullable r1 || nullable r2
  | Sum3 (r1, r2, r3) -> nullable r1 || nullable r2 || nullable r3
  | Prod (r1, r2) -> nullable r1 && nullable r2
  | Star _r -> true
  | Ntimes (n, r) -> if n = 0 then true else nullable r
  | Backref (r, r2, cs) -> nullable r && nullable r2 && cs = []

let rec der r c =
  match (r, c) with
  | Zero, _c -> Zero
  | One, _c -> Zero
  | Character d, c -> if c = d then One else Zero
  | Sum (r1, r2), c -> Sum (der r1 c, der r2 c)
  | Sum3 (r1, r2, r3), c -> Sum3 (der r1 c, der r2 c, der r3 c)
  | Prod (r1, r2), c ->
      if nullable r1 then Sum (Prod (der r1 c, r2), der r2 c)
      else Prod (der r1 c, r2)
  | Star r, c -> Prod (der r c, Star r)
  | Ntimes (n, r), c -> if n = 0 then Zero else Prod (der r c, Ntimes (n - 1, r))
  | Backref (r, r2, cs), c ->
      let d1 = Backref (der r c, r2, cs @ [ c ]) in
      let d2 = Backref (r, der r2 c, cs) in
      if nullable r && nullable r2 && List.nth_opt cs 0 = Some c then
        let d3 = Backref (r, r2, List.tl cs) in
        Sum3 (d1, d2, d3)
      else if nullable r then Sum (d1, d2)
      else d1

let explode s = List.init (String.length s) (String.get s)
let rec der_srec r = function [] -> r | c :: s -> der_srec (der r c) s
let der_s r s = explode s |> der_srec r
