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
  | NUMBER of string
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
  | IDENTIFIER _ -> "IDENTIFIER"
  | STRING _ -> "STRING"
  | NUMBER _ -> "NUMBER"
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
  | STRING s -> Printf.sprintf "\"%s\"" s
  | NUMBER n -> Printf.sprintf "%s" n
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

let string_to_reseved_word = function
  | "and" -> Some AND
  | "class" -> Some CLASS
  | "else" -> Some ELSE
  | "false" -> Some FALSE
  | "for" -> Some FOR
  | "fun" -> Some FUN
  | "if" -> Some IF
  | "nil" -> Some NIL
  | "or" -> Some OR
  | "print" -> Some PRINT
  | "return" -> Some RETURN
  | "this" -> Some THIS
  | "true" -> Some TRUE
  | "var" -> Some VAR
  | "while" -> Some WHILE
  | "super" -> Some SUPER
  | _ -> None

type literal = LString of string | LNumber of float

type token_info = {
  ttype : token;
  lexeme : string;
  literal : literal option;
  line : int;
}

let token_info_to_str (t : token_info) =
  Printf.sprintf "%s %s %s"
    (token_type_to_name_str t.ttype)
    t.lexeme
    (match t.literal with
    | None -> "null"
    | Some s -> (
        match s with
        | LString s -> s
        | LNumber n ->
            if Float.is_integer n then Printf.sprintf "%.01f" n
            else Printf.sprintf "%.15g" n))

let make_token_info ttype literal line =
  let lexeme = token_type_to_str ttype in
  { ttype; lexeme; literal; line }
