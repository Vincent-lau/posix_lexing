type r =
  | Zero
  | One
  | Character of char
  | Sum of r * r
  | Prod of r * r
  | Star of r
  | Ntimes of int * r


let rec to_string = function
  | Zero -> "0"
  | One -> "1"
  | Character c -> Printf.sprintf "%c" c
  | Sum (r1, r2) -> Printf.sprintf "%s + %s" (to_string r1) (to_string r2)
  | Prod (r1, r2) -> Printf.sprintf "%s . %s" (to_string r1) (to_string r2)
  | Star r -> Printf.sprintf "%s*" (to_string r)
  | Ntimes (n, r) -> Printf.sprintf "%s{%d}" (to_string r) n

let rec nullable = function
  | Zero -> false
  | One -> true
  | Character _ -> false
  | Sum (r1, r2) -> nullable r1 && nullable r2
  | Prod (r1, r2) -> nullable r1 || nullable r2
  | Star _r -> true
  | Ntimes (n, r) -> if n = 0 then true else nullable r

let rec der r c =
  match (r, c) with
  | Zero, _c -> Zero
  | One, _c -> Zero
  | Character d, c -> if c = d then One else Zero
  | Sum (r1, r2), c -> Sum (der r1 c, der r2 c)
  | Prod (r1, r2), c ->
      if nullable r1 then Sum (Prod (der r1 c, r2), der r2 c)
      else Prod (der r1 c, r2)
  | Star r, c -> Prod (der r c, Star r)
  | Ntimes (n, r), c -> if n = 0 then Zero else Prod (der r c, Ntimes (n - 1, r))
