open Recons
open Match

let explode s = List.init (String.length s) (String.get s)

let rec lexer_rec r = function
  | [] -> if nullable r then Some (mkeps r) else None
  | c :: s -> lexer_rec (der r c) s |> Option.map (inj r c)

let lexer r s = lexer_rec r (explode s)
