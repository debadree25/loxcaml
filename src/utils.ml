let ( let* ) = Result.bind
let ( let** ) = Option.bind

let rec result_list_map f lst =
  match lst with
  | [] -> Ok []
  | x :: xs ->
      let* y  = f x in
      let* ys = result_list_map f xs in
      Ok (y :: ys)
