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
  | STRING
  | NUMBER
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
  | STRING -> "STRING"
  | NUMBER -> "NUMBER"
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
let add_token scanner (ti : token_info) = scanner.tokens <- ti :: scanner.tokens

let advance scanner =
  scanner.current_lexeme <- scanner.current_lexeme + 1;
  scanner.source.[scanner.current_lexeme - 1]

let report_error scanner line message =
  Printf.eprintf "[line %d] Error: %s\n" line message;
  scanner.had_error <- true

let scan_token scanner =
  let c = advance scanner in
  match c with
  | '(' ->
      add_token scanner
        {
          ttype = LEFT_PAREN;
          lexeme = "(";
          literal = None;
          line = scanner.line;
        }
  | ')' ->
      add_token scanner
        {
          ttype = RIGHT_PAREN;
          lexeme = ")";
          literal = None;
          line = scanner.line;
        }
  | '{' ->
      add_token scanner
        {
          ttype = LEFT_BRACE;
          lexeme = "{";
          literal = None;
          line = scanner.line;
        }
  | '}' ->
      add_token scanner
        {
          ttype = RIGHT_BRACE;
          lexeme = "}";
          literal = None;
          line = scanner.line;
        }
  | ',' ->
      add_token scanner
        { ttype = COMMA; lexeme = ","; literal = None; line = scanner.line }
  | '.' ->
      add_token scanner
        { ttype = DOT; lexeme = "."; literal = None; line = scanner.line }
  | '-' ->
      add_token scanner
        { ttype = MINUS; lexeme = "-"; literal = None; line = scanner.line }
  | '+' ->
      add_token scanner
        { ttype = PLUS; lexeme = "+"; literal = None; line = scanner.line }
  | ';' ->
      add_token scanner
        { ttype = SEMICOLON; lexeme = ";"; literal = None; line = scanner.line }
  | '*' ->
      add_token scanner
        { ttype = STAR; lexeme = "*"; literal = None; line = scanner.line }
  | '!' ->
      add_token scanner
        { ttype = BANG; lexeme = "!"; literal = None; line = scanner.line }
  | '=' ->
      add_token scanner
        { ttype = EQUAL; lexeme = "="; literal = None; line = scanner.line }
  | '<' ->
      add_token scanner
        { ttype = LESS; lexeme = "<"; literal = None; line = scanner.line }
  | '>' ->
      add_token scanner
        { ttype = GREATER; lexeme = ">"; literal = None; line = scanner.line }
  | _ -> report_error scanner scanner.line "Unexpected character"

let scan_tokens scanner =
  while not (is_at_end scanner) do
    scanner.start_lexeme <- scanner.current_lexeme;
    scan_token scanner;
    scanner.line <- scanner.line + 1
  done;
  add_token scanner
    { ttype = EOF; lexeme = "EOF"; literal = None; line = scanner.line };
  List.rev scanner.tokens

let tokenize (source : string) : token_info list =
  let scanner = make_scanner source in
  scan_tokens scanner

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

  if String.length file_contents > 0 then
    let tokens = tokenize file_contents in
    List.iter (fun t -> print_endline (token_info_to_str t)) tokens
  else print_endline "EOF  null";
  ()
