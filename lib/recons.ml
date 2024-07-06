open Match

type v =
  | Empty
  | Char of char
  | Left of v
  | Right of v
  | Left3 of v
  | Mid3 of v
  | Right3 of v
  | Seq of v * v
  | Stars of v list
  | Br of v * v * char list

let rec to_string = function
  | Empty -> "Empty"
  | Char c -> Printf.sprintf "Char %c" c
  | Left v -> Printf.sprintf "Left (%s)" (to_string v)
  | Right v -> Printf.sprintf "Right (%s)" (to_string v)
  | Left3 v -> Printf.sprintf "Left3 (%s)" (to_string v)
  | Mid3 v -> Printf.sprintf "Mid3 (%s)" (to_string v)
  | Right3 v -> Printf.sprintf "Right3 (%s)" (to_string v)
  | Seq (v1, v2) -> Printf.sprintf "Seq (%s, %s)" (to_string v1) (to_string v2)
  | Stars vs ->
      List.map to_string vs |> String.concat "," |> Printf.sprintf "Stars [%s]"
  | Br (v1, v2, cs) ->
      Printf.sprintf "Br (%s, %s, %s)" (to_string v1) (to_string v2)
        (List.to_seq cs |> String.of_seq)

exception Non_Nullable of r

let rec mkeps = function
  | One -> Empty
  | Prod (r1, r2) -> Seq (mkeps r1, mkeps r2)
  | Sum (r1, r2) -> if nullable r1 then Left (mkeps r1) else Right (mkeps r2)
  | Sum3 (r1, r2, r3) ->
      if nullable r1 then Left3 (mkeps r1)
      else if nullable r2 then Mid3 (mkeps r2)
      else Right3 (mkeps r3)
  | Star _r -> Stars []
  | Ntimes (n, r) -> Stars (List.init n (Fun.const (mkeps r)))
  | Backref (r, r2, []) -> Br (mkeps r, mkeps r2, [])
  | r -> raise (Non_Nullable r)

let rec inj_rec (r : r) (c : char) (v : v) : v =
  match (r, c, v) with
  | Character _d, c, Empty -> Char c
  | Sum (r1, _r2), c, Left v1 -> Left (inj_rec r1 c v1)
  | Sum (_r1, r2), c, Right v2 -> Right (inj_rec r2 c v2)
  | Prod (r1, _r2), c, Seq (v1, v2) -> Seq (inj_rec r1 c v1, v2)
  | Prod (r1, _r2), c, Left (Seq (v1, v2)) -> Seq (inj_rec r1 c v1, v2)
  | Prod (r1, r2), c, Right v2 -> Seq (mkeps r1, inj_rec r2 c v2)
  | Star r, c, Seq (v, Stars vs) -> Stars (inj_rec r c v :: vs)
  | Ntimes (_n, r), c, Seq (v, Stars vs) -> Stars (inj_rec r c v :: vs)
  | Backref (r1, _r2, _cs), c, Br (v1, v2, cv :: cvs) when c = cv ->
      Br (inj_rec r1 c v1, v2, cvs)
  | Backref (r1, _r2, _cs), c, Left (Br (v1, v2, cv :: cvs)) when c = cv ->
      Br (inj_rec r1 c v1, v2, cvs)
  | Backref (_r1, r2, _cs), c, Right (Br (v1, v2, cvs)) ->
      Br (v1, inj_rec r2 c v2, cvs)
  | Backref (r1, _r2, _cs), c, Left3 (Br (v1, v2, cv :: cvs)) when c = cv ->
      Br (inj_rec r1 c v1, v2, cvs)
  | Backref (_r1, r2, _cs), c, Mid3 (Br (v1, v2, cvs)) ->
      Br (v1, inj_rec r2 c v2, cvs)
  | Backref (_r1, _r2, _cs), c, Right3 (Br (v1, v2, cvs)) ->
      Br (v1, v2, cvs @ [ c ])
  | _ -> failwith "invalid injection" (* TODO *)

let inj r c v =
  match r with
  | Backref (r1, r2, cs) -> inj_rec (Backref (r1, r2, List.rev cs)) c v
  | r -> inj_rec r c v

let rec inj_srec r cs v =
  match cs with [] -> v | c :: cs -> inj r c v |> inj_srec r cs

let inj_s r s v =
  let cs = explode s |> List.rev in
  inj_srec r cs v
