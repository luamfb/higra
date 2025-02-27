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

  (*** SEMANTIC VALUES ***)

  fun dump_sema_shape Sema.Circle = "circle"
    | dump_sema_shape Sema.Rect = "rect"

  fun dump_sema_edge_type Sema.Cont = "(cont_edge '--')"
    | dump_sema_edge_type Sema.ContDir = "(cont_dir_edge '->')"
    | dump_sema_edge_type Sema.Dotted = "(dotted_edge '..')"
    | dump_sema_edge_type Sema.DottedDir = "(dotted_dir_edge '.>')"

  fun dump_sema_color (color : Sema.Color) = "(" ^ (dump_color color) ^ ")"

  fun dump_sema_vertex_attrib ((color, shape, size) : Sema.VertexAttrib) =
    "(vertex_attrib: color = " ^ (dump_sema_color color)
    ^ ", shape = " ^ (dump_sema_shape shape)
    ^ ", size = " ^ (Int.toString size)
    ^ ")"

  fun dump_sema_edge_attrib (color : Sema.Color) =
    "(edge_attrib: color = " ^ (dump_sema_color color) ^ ")"

  fun dump_sema_fig_attrib ((font, horiz) : Sema.FigAttrib) =
    "(fig_attrib: font = " ^ font ^ ", horiz = " ^ (dump_bool horiz) ^ ")"

  fun dump_sema_vertex_info ((label, attrib) : Sema.VertexInfo) =
    "(vertex_info: label = " ^ label
    ^ ", attrib = " ^ (dump_sema_vertex_attrib attrib) ^ ")"

  fun dump_sema_state ([] : Sema.State) = "(empty state)"
    | dump_sema_state ((id, info)::rest) =
    "(id = " ^ id ^ " => " ^ (dump_sema_vertex_info info)
    ^ ", " ^ (dump_sema_state rest) ^ ")"

  fun dump_sema_edge ((from, to, edge_type, attr) : Sema.Edge) =
    "(edge: from = " ^ from ^ ", to = " ^ to
    ^ ", type = " ^ (dump_sema_edge_type edge_type)
    ^ ", attr = " ^ (dump_sema_edge_attrib attr)
    ^ ")"

  fun dump_edge_list ([] : Sema.Edge list) = ""
    | dump_edge_list (e::es) =
    (dump_sema_edge e) ^ ", " ^ (dump_edge_list es)

  fun dump_sema_graph (Sema.Graph (id, edges, subgraphs)) =
    "(graph: id = " ^ id
    ^ ", edges = (" ^ (dump_edge_list edges)
    ^ "), subgraphs = (" ^ (dump_graph_list subgraphs) ^ "))"
  and dump_graph_list [] = ""
    | dump_graph_list (g::gs) =
    (dump_sema_graph g) ^ ", " ^ (dump_graph_list gs)

  fun dump_sema_figure ((attr, toplevel, state) : Sema.Figure) =
    "(figure: " ^ (dump_sema_fig_attrib attr)
    ^ ", toplevel = " ^ (dump_sema_graph toplevel)
    ^ ", state = " ^ (dump_sema_state state)
    ^ ")"

  fun debug_sema (s: string) =
    print (dump_sema_figure
    (Sema.sema_figure (Parser.parse_figure (Lexer.tokenize s))) ^ "\n")
    handle
      Sema.SemaError (pos_start, pos_end) =>
          print (
          "semantic error from " ^ (dump_pos pos_start)
          ^ " to " ^ (dump_pos pos_end) ^ "\n")
      | Ast.SyntaxError loc =>
          print ("syntax error at " ^ (dump_error_loc loc) ^ "\n")
      | Token.InvalidToken pos => print (err_msg ("invalid token", pos))

  (*** BACKEND ***)

  fun print_svg_list ([] : (Sema.VertexId * string) list) : unit = ()
    | print_svg_list ((id, str) :: rest) =
    let
      val header = "-------------------- " ^ id ^ " --------------------"
      val _ = print header
      val _ = print "\n"
      val _ = print str
      val _ = print "\n"
    in
      print_svg_list rest
    end

  fun debug_backend (s: string) =
    print_svg_list (Backend.draw_figure
      (Sema.sema_figure (Parser.parse_figure (Lexer.tokenize s))))
    handle
      Sema.SemaError (pos_start, pos_end) =>
          print (
          "semantic error from " ^ (dump_pos pos_start)
          ^ " to " ^ (dump_pos pos_end) ^ "\n")
      | Ast.SyntaxError loc =>
          print ("syntax error at " ^ (dump_error_loc loc) ^ "\n")
      | Token.InvalidToken pos => print (err_msg ("invalid token", pos))

end
