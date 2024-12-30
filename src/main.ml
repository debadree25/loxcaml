type token =
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | IDENTIFIER of string
  | STRING of string
  | NUMBER of float
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF

let token_type_to_name_str t =
  match t with
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | IDENTIFIER s -> "IDENTIFIER " ^ s
  | STRING s -> "STRING " ^ s
  | NUMBER n -> "NUMBER " ^ string_of_float n
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | FOR -> "FOR"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
  | EOF -> "EOF"

let token_type_to_str t =
  match t with
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | COMMA -> ","
  | DOT -> "."
  | MINUS -> "-"
  | PLUS -> "+"
  | SEMICOLON -> ";"
  | SLASH -> "/"
  | STAR -> "*"
  | BANG -> "!"
  | BANG_EQUAL -> "!="
  | EQUAL -> "="
  | EQUAL_EQUAL -> "=="
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  | IDENTIFIER s -> s
  | STRING s -> s
  | NUMBER n -> string_of_float n
  | AND -> "and"
  | CLASS -> "class"
  | ELSE -> "else"
  | FALSE -> "false"
  | FUN -> "fun"
  | FOR -> "for"
  | IF -> "if"
  | NIL -> "nil"
  | OR -> "or"
  | PRINT -> "print"
  | RETURN -> "return"
  | SUPER -> "super"
  | THIS -> "this"
  | TRUE -> "true"
  | VAR -> "var"
  | WHILE -> "while"
  | EOF -> ""

type token_info = {
  ttype : token;
  lexeme : string;
  literal : string option;
  line : int;
}

let token_info_to_str (t : token_info) =
  Printf.sprintf "%s %s %s"
    (token_type_to_name_str t.ttype)
    t.lexeme
    (match t.literal with None -> "null" | Some s -> s)

let make_token_info ttype literal line =
  let lexeme = token_type_to_str ttype in
  { ttype; lexeme; literal; line }

type scanner = {
  source : string;
  mutable tokens : token_info list;
  mutable start_lexeme : int;
  mutable current_lexeme : int;
  mutable line : int;
  mutable had_error : bool;
}

let make_scanner source =
  {
    source;
    tokens = [];
    start_lexeme = 0;
    current_lexeme = 0;
    line = 1;
    had_error = false;
  }

let is_at_end scanner = scanner.current_lexeme >= String.length scanner.source

let add_token scanner token literal line =
  scanner.tokens <- make_token_info token literal line :: scanner.tokens

let advance scanner =
  scanner.current_lexeme <- scanner.current_lexeme + 1;
  scanner.source.[scanner.current_lexeme - 1]

let peek scanner =
  if is_at_end scanner then '\000' else scanner.source.[scanner.current_lexeme]

let match_next (scanner : scanner) expected =
  if peek scanner <> expected then false
  else (
    ignore (advance scanner);
    true)

let advance_line scanner = scanner.line <- scanner.line + 1

let report_error scanner line message =
  Printf.eprintf "[line %d] Error: %s\n" line message;
  scanner.had_error <- true

let scan_token scanner =
  let c = advance scanner in
  match c with
  | '(' -> add_token scanner LEFT_PAREN None scanner.line
  | ')' -> add_token scanner RIGHT_PAREN None scanner.line
  | '{' -> add_token scanner LEFT_BRACE None scanner.line
  | '}' -> add_token scanner RIGHT_BRACE None scanner.line
  | ',' -> add_token scanner COMMA None scanner.line
  | '.' -> add_token scanner DOT None scanner.line
  | '-' -> add_token scanner MINUS None scanner.line
  | '+' -> add_token scanner PLUS None scanner.line
  | ';' -> add_token scanner SEMICOLON None scanner.line
  | '*' -> add_token scanner STAR None scanner.line
  | '!' ->
      if match_next scanner '=' then
        add_token scanner BANG_EQUAL None scanner.line
      else add_token scanner BANG None scanner.line
  | '=' ->
      if match_next scanner '=' then
        add_token scanner EQUAL_EQUAL None scanner.line
      else add_token scanner EQUAL None scanner.line
  | '<' ->
      if match_next scanner '=' then
        add_token scanner LESS_EQUAL None scanner.line
      else add_token scanner LESS None scanner.line
  | '>' ->
      if match_next scanner '=' then
        add_token scanner GREATER_EQUAL None scanner.line
      else add_token scanner GREATER None scanner.line
  | '\n' -> advance_line scanner
  | _ ->
      report_error scanner scanner.line
        (Printf.sprintf "Unexpected character: %c" c)

let scan_tokens scanner =
  while not (is_at_end scanner) do
    scanner.start_lexeme <- scanner.current_lexeme;
    scan_token scanner
  done;
  add_token scanner EOF None scanner.line;
  List.rev scanner.tokens

type tokenize_result = { tokens : token_info list; had_error : bool }

let tokenize (source : string) : tokenize_result =
  let scanner = make_scanner source in
  let tokens = scan_tokens scanner in
  { tokens; had_error = scanner.had_error }

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  if command <> "tokenize" then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);

  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  if String.length file_contents > 0 then (
    let tokenize_result = tokenize file_contents in
    List.iter
      (fun t -> print_endline (token_info_to_str t))
      tokenize_result.tokens;
    if tokenize_result.had_error then exit 65 else ())
  else print_endline (token_info_to_str (make_token_info EOF None 1))
