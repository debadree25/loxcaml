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
let previous_with_token_info parser = parser.tokens.(parser.current - 1)

let advance parser =
  if not (is_at_end parser) then parser.current <- parser.current + 1;
  previous parser

let retreat parser =
  if parser.current > 0 then parser.current <- parser.current - 1;
  peek parser

let check parser expected =
  if is_at_end parser then false else peek parser = expected

let check_pattern parser expected =
  if is_at_end parser then false
  else token_to_token_pattern (peek parser) = expected

let rec match_tokens_by_matcher matcher parser tokens =
  match tokens with
  | [] -> false
  | token :: remaining ->
      if matcher parser token then (
        ignore (advance parser);
        true)
      else match_tokens_by_matcher matcher parser remaining

let match_tokens = match_tokens_by_matcher check
let match_tokens_by_pattern = match_tokens_by_matcher check_pattern

type parse_error = string * token_info

let consume parser expected (message : string) =
  if check parser expected then Ok (advance parser)
  else Error (message, peek_with_token_info parser)

let consume_by_pattern parser pattern (message : string) =
  if check_pattern parser pattern then Ok (advance parser)
  else Error (message, peek_with_token_info parser)

let report_error parser (parse_error : parse_error) =
  let message, token = parse_error in
  Printf.eprintf "[line %d] Error: %s %s\n" token.line
    (match token.ttype with
    | EOF -> "at end"
    | _ -> Printf.sprintf "at '%s'" token.lexeme)
    message;
  parser.had_error <- true

let literal_from_token_info token_info =
  match token_info.ttype with
  | NUMBER n -> Some (LNumber (float_of_string n))
  | STRING s -> Some (LString s)
  | TRUE -> Some (LBool true)
  | FALSE -> Some (LBool false)
  | NIL -> Some LNil
  | _ -> None

let rec expression parser = equality parser

and make_binary_continuation tokens_to_match continuation_fn parser left =
  if match_tokens parser tokens_to_match then
    let operator_info = previous_with_token_info parser in
    let operator = operator_info.ttype in
    match continuation_fn parser with
    | Ok right ->
        make_binary_continuation tokens_to_match continuation_fn parser
          (Binary (left, operator, right, operator_info))
    | Error e -> Error e
  else Ok left

and equality parser =
  let equality_continuation =
    make_binary_continuation [ BANG_EQUAL; EQUAL_EQUAL ] comparison
  in
  match comparison parser with
  | Ok expr -> equality_continuation parser expr
  | Error e -> Error e

and comparison parser =
  let comparison_continuation =
    make_binary_continuation [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ] term
  in
  match term parser with
  | Ok expr -> comparison_continuation parser expr
  | Error e -> Error e

and term parser =
  let term_continuation = make_binary_continuation [ MINUS; PLUS ] factor in
  match factor parser with
  | Ok expr -> term_continuation parser expr
  | Error e -> Error e

and factor parser =
  let factor_continuation = make_binary_continuation [ SLASH; STAR ] unary in
  match unary parser with
  | Ok expr -> factor_continuation parser expr
  | Error e -> Error e

and unary parser =
  if match_tokens parser [ BANG; MINUS ] then
    let operator_info = previous_with_token_info parser in
    let operator = operator_info.ttype in
    match unary parser with
    | Ok right -> Ok (Unary (operator, right, operator_info))
    | Error e -> Error e
  else primary parser

and primary parser =
  if
    match_tokens_by_pattern parser
      [ T_FALSE; T_TRUE; T_NIL; T_NUMBER; T_STRING ]
  then
    let token_info = previous_with_token_info parser in
    let literal = literal_from_token_info token_info in
    match literal with
    | Some lit -> Ok (Literal lit)
    | None -> Error ("Expect literal", token_info)
  else if match_tokens_by_pattern parser [ T_IDENTIFIER ] then
    let token_info = previous_with_token_info parser in
    Ok (Variable (token_info.ttype, token_info))
  else if match_tokens parser [ LEFT_PAREN ] then
    match expression parser with
    | Ok expr -> (
        match consume parser RIGHT_PAREN "Expect ')' after expression" with
        | Ok _ -> Ok (Grouping expr)
        | Error e -> Error e)
    | Error e -> Error e
  else Error ("Expect expression", peek_with_token_info parser)

let rec synchronize parser =
  let prev = advance parser in
  if is_at_end parser then ()
  else
    match prev with
    | SEMICOLON -> ()
    | CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN ->
        ignore (retreat parser)
    | _ -> synchronize parser

let rec declaration parser =
  if match_tokens parser [ VAR ] then (
    match var_declaration parser with
    | Ok stmt -> Ok stmt
    | Error e ->
        synchronize parser;
        Error e)
  else statement parser

and var_declaration parser =
  let name = consume_by_pattern parser T_IDENTIFIER "Expect variable name" in
  let name_token_info = previous_with_token_info parser in
  match name with
  | Ok name_token -> (
      match
        if match_tokens parser [ EQUAL ] then expression parser
        else Ok (Literal LNil)
      with
      | Ok expr ->
          let _ =
            consume parser SEMICOLON "Expect ';' after variable declaration"
          in
          Ok (Var (name_token, name_token_info, Some expr))
      | Error e -> Error e)
  | Error e -> Error e

and statement parser =
  if match_tokens parser [ PRINT ] then print_statement parser
  else expression_statement parser

and print_statement parser =
  match expression parser with
  | Ok expr ->
      let _ = consume parser SEMICOLON "Expect ';' after value" in
      Ok (Print expr)
  | Error e -> Error e

and expression_statement parser =
  match expression parser with
  | Ok expr ->
      let _ = consume parser SEMICOLON "Expect ';' after expression" in
      Ok (Expression expr)
  | Error e -> Error e

let rec parse parser =
  if is_at_end parser then []
  else
    match declaration parser with
    | Ok stmt -> stmt :: parse parser
    | Error e ->
        report_error parser e;
        synchronize parser;
        parse parser

let parse_tokens tokens =
  let parser = make_parser tokens in
  let statements = parse parser in
  if parser.had_error then Error 65 else Ok statements
