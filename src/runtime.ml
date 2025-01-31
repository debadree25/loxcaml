open Token
open Ast

type value =
  | Primitive of literal
  | NativeFunc of int * string * (value list -> value)
  | UserFunc of
      int * token_info * string * token_info list * statement * environment

and environment = {
  enclosing : environment option;
  bindings : (string, value) Hashtbl.t;
}

let clock = NativeFunc (0, "clock", fun _ -> Primitive (LNumber (Unix.time ())))
let builtins_list = [ ("clock", clock) ]
let make_enviroment enclosing = { enclosing; bindings = Hashtbl.create 10 }

let make_globals environment =
  List.iter
    (fun (name, value) -> Hashtbl.add environment.bindings name value)
    builtins_list

let add_binding environment name value =
  Hashtbl.add environment.bindings name value

let reassign_binding environment name value =
  Hashtbl.replace environment.bindings name value

let literal_to_str = function
  | LBool b -> string_of_bool b
  | LNil -> "nil"
  | LString s -> s
  | LNumber n ->
      if Float.is_integer n then Printf.sprintf "%d" (int_of_float n)
      else Printf.sprintf "%.15g" n

let value_to_str = function
  | Primitive lit -> literal_to_str lit
  | NativeFunc _ -> "<native fn>"
  | UserFunc (_, _, name, _, _, _) -> Printf.sprintf "<fn %s>" name

let dump_environment environment =
  let binding_printer bindings =
    Hashtbl.iter
      (fun key value -> Printf.printf "%s -> %s\n" key (value_to_str value))
      bindings
  in
  let rec dump env =
    binding_printer env.bindings;
    match env.enclosing with Some enclosing -> dump enclosing | None -> ()
  in
  dump environment
