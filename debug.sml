structure Debug = struct
  fun dump_pos ({line, col}: Token.SourcePos) : string =
    "line " ^ (Int.toString line) ^ " col " ^ (Int.toString col)

  fun dump_token_kind (kind: Token.TokenKind) : string =
    case kind of
         Token.Semicolon => "semicolon ';'"
       | Token.Comma => "comma ','"
       | Token.Equals => "equals '='"
       | Token.OpenBrace => "open_brace '{'"
       | Token.CloseBrace => "close_brace '}'"
       | Token.OpenBracket => "open_bracket '['"
       | Token.CloseBracket => "close_bracket ']'"
       | Token.ContDirEdge => "cont_dir_edge '->'"
       | Token.ContNonDirEdge => "cont_undir_edge '--'"
       | Token.DottedDirEdge => "dotted_dir_edge '.>'"
       | Token.DottedNonDirEdge => "dotted_undir_edge '..'"
       | Token.Id s => "identifier '" ^ s ^ "'"
       | Token.StrLit s => "string '" ^ s ^ "'"
       | Token.NumLit n => "number '" ^ (Int.toString n) ^ "'"
       | Token.BoolLit b =>
           "bool '" ^ (if b then "true" else "false") ^ "'"
       | Token.ColorCode (r,g,b) =>
           "color 'r = " ^ (Int.toString r)
           ^ ", g = " ^ (Int.toString g)
           ^ ", b = " ^ (Int.toString b) ^ "'"

  fun dump_token ((kind, pos): Token.Token) : string =
    (dump_pos pos) ^ " token " ^ (dump_token_kind kind)

  fun dump_token_list ([] : Token.Token list) = ""
    | dump_token_list (t::ts) = 
    (dump_token t) ^ (dump_token_list ts)

  fun debug_tokenize (s: string) =
  let
    val err_msg : string * Token.SourcePos -> string =
      fn (msg, pos) => msg ^ " at " ^ (dump_pos pos) ^ "\n"
  in
    print (dump_token_list (Lexer.tokenize s))
    handle
    Token.InvalidToken pos => print (err_msg ("invalid token", pos))
  end

end
