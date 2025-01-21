open Token
open Ast

type value =
  | Primitive of literal
  | NativeFunc of int * string * (value list -> value)
  | UserFunc of
      int
      * token_info
      * string
      * token_info list
      * statement
      * environment

and environment = {
  enclosing : environment option;
  bindings : (string, value) Hashtbl.t;
}

let clock =
  NativeFunc (0, "clock", fun _ -> Primitive (LNumber (Unix.time ())))

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
