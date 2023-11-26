structure Debug = struct
  fun dump_pos ({line, col}: Token.SourcePos) : string =
    "line " ^ (Int.toString line) ^ " col " ^ (Int.toString col)

  fun err_msg ((msg, pos) : string * Token.SourcePos) : string =
    msg ^ " at " ^ (dump_pos pos) ^ "\n"

  (*** LEXER ***)

  fun dump_bool (b : bool) : string =
    "bool '" ^ (if b then "true" else "false") ^ "'"

  fun dump_color (r: int, g: int, b: int) : string =
    "color 'r = " ^ (Int.toString r)
    ^ ", g = " ^ (Int.toString g)
    ^ ", b = " ^ (Int.toString b) ^ "'"

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
       | Token.BoolLit b => dump_bool b
       | Token.ColorCode color => dump_color color

  fun dump_token ((kind, pos): Token.Token) : string =
    (dump_pos pos) ^ " token " ^ (dump_token_kind kind)

  fun dump_token_list ([] : Token.Token list) = ""
    | dump_token_list (t::ts) = 
    (dump_token t) ^ (dump_token_list ts)

  fun debug_tokenize (s: string) =
    print (dump_token_list (Lexer.tokenize s))
    handle
        Token.InvalidToken pos => print (err_msg ("invalid token", pos))

  (*** AST ***)

  fun dump_error_loc Ast.ErrorAtEOF : string = "end of file"
    | dump_error_loc (Ast.ErrorPos pos) = dump_pos pos

  fun dump_ast_id (Ast.Id (s, pos)) : string =
    "ID '" ^ s ^ "' at " ^ (dump_pos pos)

  fun dump_ast_literal (Ast.LitStr (s, pos)) : string =
    "(string '" ^ s ^ "' at " ^ (dump_pos pos) ^ ")"
    | dump_ast_literal (Ast.LitNum (n, pos)) =
    "(number '" ^ (Int.toString n) ^ "' at " ^ (dump_pos pos) ^ ")"
    | dump_ast_literal (Ast.LitBool (b, pos)) =
    "(" ^ (dump_bool b) ^ " at " ^ (dump_pos pos) ^ ")"
    | dump_ast_literal (Ast.LitColor (color, pos)) =
    "(" ^ (dump_color color) ^ " at " ^ (dump_pos pos) ^ ")"

  fun dump_edge_type (Ast.ContDirEdge pos) : string =
    "(cont_dir_edge '->' at " ^ (dump_pos pos) ^ ")"
    | dump_edge_type (Ast.ContNonDirEdge pos) =
    "(cont_undir_edge '--' at " ^ (dump_pos pos) ^ ")"
    | dump_edge_type (Ast.DottedDirEdge pos) =
    "(dotted_dir_edge '.>' at " ^ (dump_pos pos) ^ ")"
    | dump_edge_type (Ast.DottedNonDirEdge pos) =
    "(dotted_undir_edge '..' at " ^ (dump_pos pos) ^ ")"

  fun dump_name_values ([] : (Ast.Identifier * Ast.Literal) list) = ""
    | dump_name_values ((id, literal)::rest) =
    "(name = " ^ (dump_ast_id id)
    ^ " value = " ^ (dump_ast_literal literal)
    ^ " " ^ (dump_name_values rest) ^ ")"

  fun dump_attribute (Ast.Attr attr_list) : string =
    "(attribute " ^ (dump_name_values attr_list) ^ ")"

  fun dump_opt_attr (NONE : Ast.Attribute option) = "(no attribute)"
    | dump_opt_attr (SOME attr) = dump_attribute attr

  fun dump_vertex (Ast.Vertex (id, NONE, opt_attr)) : string =
    "(vertex " ^ (dump_ast_id id)
    ^ " (no label) " ^ (dump_opt_attr opt_attr) ^ ")"
    | dump_vertex (Ast.Vertex (id, SOME label, opt_attr)) =
    "(vertex " ^ (dump_ast_id id) ^
    " (label " ^ (dump_ast_literal label) ^ ")"
    ^ (dump_opt_attr opt_attr) ^ ")"

  fun dump_edge (Ast.Edge (v_from, v_to, edge_type, opt_attr)) : string =
    "(edge from " ^ (dump_vertex v_from)
    ^ " to " ^ (dump_vertex v_to)
    ^ " type " ^ (dump_edge_type edge_type)
    ^ " " ^ (dump_opt_attr opt_attr)
    ^ ")"

  fun dump_graph_elem (Ast.ElemE edge) : string = dump_edge edge
    | dump_graph_elem (Ast.ElemG (Ast.Graph (vertex, elems))) =
    "(graph " ^ (dump_vertex vertex)
    ^ " elements " ^ (dump_elem_list elems) ^ ")"
  and dump_elem_list ([] : Ast.GraphElem list) : string = ""
    | dump_elem_list (e::es) = (dump_graph_elem e) ^ (dump_elem_list es)

  fun dump_figure (Ast.Fig (opt_attr, elems)) : string =
    "(figure " ^ (dump_opt_attr opt_attr)
    ^ " elements " ^ (dump_elem_list elems)
    ^ ")\n"

  fun debug_parser (s : string) =
    print (dump_figure (Parser.parse_figure (Lexer.tokenize s)))
    handle
        Ast.SyntaxError loc =>
            print ("syntax error at " ^ (dump_error_loc loc) ^ "\n")
      | Token.InvalidToken pos => print (err_msg ("invalid token", pos))

end
