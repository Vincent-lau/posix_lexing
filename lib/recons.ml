open Match

type v =
  | Empty
  | Char of char
  | Left of v
  | Right of v
  | Seq of v * v
  | Stars of v list

let rec to_string = function
  | Empty -> "Empty"
  | Char c -> Printf.sprintf "Char %c" c
  | Left v -> Printf.sprintf "Left (%s)" (to_string v)
  | Right v -> Printf.sprintf "Right (%s)" (to_string v)
  | Seq (v1, v2) -> Printf.sprintf "Seq (%s, %s)" (to_string v1) (to_string v2)
  | Stars vs ->
      List.map to_string vs |> String.concat "," |> Printf.sprintf "Stars [%s]"

exception Non_Nullable of r

let rec mkeps = function
  | One -> Empty
  | Prod (r1, r2) -> Seq (mkeps r1, mkeps r2)
  | Sum (r1, r2) -> if nullable r1 then Left (mkeps r1) else Right (mkeps r2)
  | Star _r -> Stars []
  | Ntimes (n, r) -> Stars (List.init n (Fun.const (mkeps r)))
  | r -> raise (Non_Nullable r)

let rec inj (r : r) (c : char) (v : v) : v =
  match (r, c, v) with
  | Character _d, c, Empty -> Char c
  | Sum (r1, _r2), c, Left v1 -> Left (inj r1 c v1)
  | Sum (_r1, r2), c, Right v2 -> Right (inj r2 c v2)
  | Prod (r1, _r2), c, Seq (v1, v2) -> Seq (inj r1 c v1, v2)
  | Prod (r1, _r2), c, Left (Seq (v1, v2)) -> Seq (inj r1 c v1, v2)
  | Prod (r1, r2), c, Right v2 -> Seq (mkeps r1, inj r2 c v2)
  | Star r, c, Seq (v, Stars vs) -> Stars (inj r c v :: vs)
  | Ntimes (_n, r), c, Seq (v, Stars vs) -> Stars (inj r c v :: vs)
  | _ -> failwith "invalid injection" (* TODO *)
