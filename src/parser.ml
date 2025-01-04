open Token
open Ast

type parser = {
  mutable tokens : token_info array;
  mutable current : int;
  mutable had_error : bool;
}

let make_parser (tokens : token_info list) =
  { tokens = Array.of_list tokens; current = 0; had_error = false }

let peek parser = parser.tokens.(parser.current).ttype
let peek_with_token_info parser = parser.tokens.(parser.current)
let is_at_end parser = match peek parser with EOF -> true | _ -> false
let previous parser = parser.tokens.(parser.current - 1).ttype

let advance parser =
  if not (is_at_end parser) then parser.current <- parser.current + 1;
  previous parser

let check parser expected =
  if is_at_end parser then false else peek parser = expected

let check_pattern parser expected =
  if is_at_end parser then false
  else token_to_token_pattern (peek parser) = expected

let rec match_tokens parser tokens =
  match tokens with
  | [] -> false
  | token :: remaining ->
      if check parser token then (
        ignore (advance parser);
        true)
      else match_tokens parser remaining

let rec match_tokens_by_pattern parser tokens =
  match tokens with
  | [] -> false
  | token :: remaining ->
      if check_pattern parser token then (
        ignore (advance parser);
        true)
      else match_tokens_by_pattern parser remaining

type parse_error = string * token_info

let consume parser expected (message : string) : (token, parse_error) result =
  if check parser expected then Ok (advance parser)
  else Error (message, peek_with_token_info parser)

let report_error parser (parse_error : parse_error) =
  let message, token = parse_error in
  Printf.eprintf "[line %d] Error: %s %s\n" token.line
    (match token.ttype with
    | EOF -> "at end"
    | _ -> Printf.sprintf "at '%s'" token.lexeme)
    message;
  parser.had_error <- true

let rec expression (parser : parser) : (expr, parse_error) result =
  equality parser

and equality parser =
  match comparison parser with
  | Ok expr -> equality_continuation parser expr
  | Error e -> Error e

and equality_continuation parser left =
  if match_tokens parser [ BANG_EQUAL; EQUAL_EQUAL ] then
    let operator = previous parser in
    match comparison parser with
    | Ok right -> equality_continuation parser (Binary (left, operator, right))
    | Error e -> Error e
  else Ok left

and comparison parser =
  match term parser with
  | Ok expr -> comparison_continuation parser expr
  | Error e -> Error e

and comparison_continuation parser left =
  if match_tokens parser [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ] then
    let operator = previous parser in
    match term parser with
    | Ok right ->
        comparison_continuation parser (Binary (left, operator, right))
    | Error e -> Error e
  else Ok left

and term parser =
  match factor parser with
  | Ok expr -> term_continuation parser expr
  | Error e -> Error e

and term_continuation parser left =
  if match_tokens parser [ MINUS; PLUS ] then
    let operator = previous parser in
    match factor parser with
    | Ok right -> term_continuation parser (Binary (left, operator, right))
    | Error e -> Error e
  else Ok left

and factor parser =
  match unary parser with
  | Ok expr -> factor_continuation parser expr
  | Error e -> Error e

and factor_continuation parser left =
  if match_tokens parser [ SLASH; STAR ] then
    let operator = previous parser in
    match unary parser with
    | Ok right -> factor_continuation parser (Binary (left, operator, right))
    | Error e -> Error e
  else Ok left

and unary parser =
  if match_tokens parser [ BANG; MINUS ] then
    let operator = previous parser in
    match unary parser with
    | Ok right -> Ok (Unary (operator, right))
    | Error e -> Error e
  else primary parser

and primary parser =
  if
    match_tokens_by_pattern parser
      [ T_FALSE; T_TRUE; T_NIL; T_NUMBER; T_STRING ]
  then Ok (Literal (previous parser))
  else if match_tokens parser [ LEFT_PAREN ] then
    match expression parser with
    | Ok expr -> (
        match consume parser RIGHT_PAREN "Expect ')' after expression" with
        | Ok _ -> Ok (Grouping expr)
        | Error e -> Error e)
    | Error e -> Error e
  else Error ("Expect expression", peek_with_token_info parser)

let parse_tokens (tokens : token_info list) : (expr, parse_error) result =
  let parser = make_parser tokens in
  match expression parser with
  | Ok expr -> Ok expr
  | Error e ->
      report_error parser e;
      Error e
