open Ast
open Token

type func_type =
  | NativeFunction
  | UserFunction

type value =
  | Primitive of literal
  | Function of int * statement * func_type