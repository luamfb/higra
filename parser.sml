structure Ast = struct
  (* only the leaves of the AST store source position *)
  datatype Identifier = Id of string * Token.SourcePos

  datatype Literal = LitStr of string * Token.SourcePos
                   | LitNum of int * Token.SourcePos
                   | LitBool of bool * Token.SourcePos
                   | LitColor of (int * int * int) * Token.SourcePos

  datatype EdgeType = ContDirEdge of Token.SourcePos
                    | ContNonDirEdge of Token.SourcePos
                    | DottedDirEdge of Token.SourcePos
                    | DottedNonDirEdge of Token.SourcePos

  datatype Attribute = Attr of (Identifier * Literal) list

  datatype Vertex = Vertex of Identifier * Literal option * Attribute option
  datatype Edge = Edge of Vertex * Vertex * EdgeType * Attribute option

  datatype GraphElem = ElemE of Edge
                     | ElemG of Graph
  (* note: "and" is required for mutually recursive definitions *)
  and Graph = Graph of Vertex * GraphElem list

  datatype Figure = Fig of Attribute option * GraphElem list

  datatype ErrorLoc = ErrorPos of Token.SourcePos
                    | ErrorAtEOF

  exception SyntaxError of ErrorLoc
end

fun syntax_error_at (t : Token.Token list) : 'a =
  case t of
       ((_, pos)::_) => raise Ast.SyntaxError (Ast.ErrorPos pos)
     | [] => raise Ast.SyntaxError Ast.ErrorAtEOF

structure Parser = struct
  (* we mark our current position in production rules with HERE *)

  (* HERE <id> *)
  fun parse_id [] = raise Ast.SyntaxError Ast.ErrorAtEOF
    | parse_id (((kind, pos)::ts) : Token.Token list)
    : Token.Token list * Ast.Identifier =
    case kind of
         (Token.Id s) => (ts, (Ast.Id (s, pos)))
       | _ => raise Ast.SyntaxError (Ast.ErrorPos pos)

  (* HERE (<strLit> | <boolLit> | <numLit> | <colorCode>) *)
  fun parse_literal [] = raise Ast.SyntaxError Ast.ErrorAtEOF
    | parse_literal (((kind, pos)::ts) : Token.Token list)
    : Token.Token list * Ast.Literal =
    case kind of
         Token.StrLit s => (ts, (Ast.LitStr (s, pos)))
       | Token.NumLit n => (ts, (Ast.LitNum (n, pos)))
       | Token.BoolLit b => (ts, (Ast.LitBool (b, pos)))
       | Token.ColorCode c => (ts, (Ast.LitColor (c, pos)))
       | _ => raise Ast.SyntaxError (Ast.ErrorPos pos)

  (* HERE <edgeOper> *)
  fun parse_edge_oper [] = raise Ast.SyntaxError Ast.ErrorAtEOF
    | parse_edge_oper (((kind, pos)::ts) : Token.Token list)
    : Token.Token list * Ast.EdgeType =
    case kind of
         Token.ContDirEdge => (ts, (Ast.ContDirEdge pos))
       | Token.DottedDirEdge => (ts, (Ast.DottedDirEdge pos))
       | Token.ContNonDirEdge => (ts, (Ast.ContNonDirEdge pos))
       | Token.DottedNonDirEdge => (ts, (Ast.DottedNonDirEdge pos))
       | _ => raise Ast.SyntaxError (Ast.ErrorPos pos)

  (* HERE <id> "=" <literal> *)
  fun parse_assign (t0 : Token.Token list)
    : Token.Token list * (Ast.Identifier * Ast.Literal) =
    let
      val (t1, id) = parse_id t0
    in
      (* <id> HERE "=" <literal> *)
      case t1 of
           ((Token.Equals, _)::t2) =>
           let
             val (t3, literal) = parse_literal t2
           in
             (t3, (id, literal))
           end
         | _ => syntax_error_at t1
    end

  (* "[" HERE <assign> (","      <assign>)* "]" *)
  (* "["      <assign> ("," HERE <assign>)* "]" *)
  fun parse_assign_list (t0 : Token.Token list)
    : Token.Token list * (Ast.Identifier * Ast.Literal) list =
    let
      val (t1, (id, literal)) = parse_assign t0
    in
      (* "[" <assign> (HERE "," <assign>)*      "]" *)
      (* "[" <assign> (     "," <assign>)* HERE "]" *)
      case t1 of
           ((Token.CloseBracket,_)::t2) => (t2, [(id, literal)])
         | ((Token.Comma, _)::t2) =>
             let
               val (t3, assign_list) = parse_assign_list t2
             in
               (t3, (id, literal)::assign_list)
             end
         | _ => syntax_error_at t1
    end

  fun parse_attribute (t0 : Token.Token list)
    : Token.Token list * Ast.Attribute =
    let
      val (t1, assign_list) = parse_assign_list t0
    in
      (t1, Ast.Attr assign_list)
    end

  (* HERE <id> <strLit>? <attribute>? *)
  fun parse_vertex (t0 : Token.Token list)
    : Token.Token list * Ast.Vertex =
    let
      val (t1, id) = parse_id t0
    in
      (* <id> HERE <strLit>? <attribute>? *)
      case t1 of
         ((Token.OpenBracket, _)::t2) =>
             let
               val (t3, attr) = parse_attribute t2
               val vertex = Ast.Vertex (id, NONE, (SOME attr))
             in
               (t3, vertex)
             end
         | (((Token.StrLit s), label_pos)::t2) =>
             let
               val label = Ast.LitStr (s, label_pos)
             in
               (* <id> <strLit>? HERE <attribute>? *)
               case t2 of
                    ((Token.OpenBracket, _)::t3) =>
                    let
                      val (t4, attr) = (parse_attribute t3)
                      val vertex = Ast.Vertex (id, (SOME label), (SOME attr))
                    in
                      (t4, vertex)
                    end
                  | _ => (t2, (Ast.Vertex (id, (SOME label), NONE)))
             end
         | _ => (t1, Ast.Vertex (id, NONE, NONE))
    end

  (* <vertex> HERE <edgeOper> <attribute>? <vertex> ";" *)
  fun parse_edge (t0 : Token.Token list) (v_from : Ast.Vertex)
    : Token.Token list * Ast.Edge =
    let
      val (t1, edge_oper) = parse_edge_oper t0
      val ensure_semicolon =
        fn (tokens, edge) : Token.Token list * Ast.Edge =>
          case tokens of
               ((Token.Semicolon, _)::rest_tok) => (rest_tok, edge)
             | _ => syntax_error_at tokens
    in
      case t1 of
           ((Token.OpenBracket, _)::t2) =>
           let
             val (t3, attr) = parse_attribute t2
             val (t4, v_to) = parse_vertex t3
             val edge = Ast.Edge (v_from, v_to, edge_oper, SOME attr)
           in
             ensure_semicolon (t4, edge)
           end
         | _ =>
             let
               val (t2, v_to) = parse_vertex t1
               val edge = Ast.Edge (v_from, v_to, edge_oper, NONE)
             in
               ensure_semicolon (t2, edge)
             end
    end

  (* HERE <vertex> ";" *)
  (* HERE <vertex> "{" <graphElem>+ "}" *)
  (* HERE <vertex> <edgeOper> ... *)
  fun parse_graph_elem (t0 : Token.Token list)
    : Token.Token list * Ast.GraphElem =
    let
      val (t1, vertex) = parse_vertex t0
    in
      (* <vertex> HERE ";" *)
      (* <vertex> HERE "{" <graphElem>+ "}" *)
      (* <vertex> HERE <edgeOper> ... *)
      case t1 of
           ((Token.Semicolon, _)::t2) =>
           let
             val graph = Ast.Graph (vertex, [])
           in
             (t2, Ast.ElemG graph)
           end
         | ((Token.OpenBrace, _)::t2) =>
             let
               val (t3, elems) = parse_graph_elem_list t2
               val graph = Ast.Graph (vertex, elems)
             in
               (t3, Ast.ElemG graph)
             end
         | [] => syntax_error_at t1
         | ((tok_kind, _)::_) =>
             case tok_kind of
                  (Token.ContDirEdge
                | Token.ContNonDirEdge
                | Token.DottedDirEdge
                | Token.DottedNonDirEdge) =>
                    let
                      (* note: t1 includes edge operator *)
                      val (t2, edge) = parse_edge t1 vertex
                    in
                      (t2, Ast.ElemE edge)
                    end
                | _ => syntax_error_at t1
    end
  (* HERE <graphElem>+ "}" *)
  (* HERE <graphElem>+ EOF *)
  and parse_graph_elem_list (t0: Token.Token list)
  : Token.Token list * Ast.GraphElem list =
  let
    val (t1, elem) = parse_graph_elem t0
  in
    (* <graphElem> HERE <graphElem>* *)
    (* <graphElem> HERE "}" *)
    (* <graphElem> HERE EOF *)
    case t1 of
         ((Token.CloseBrace, _)::t2) => (t2, [elem])
       | [] => (t1, [elem])
       | _ =>
           let
             val (t2, elem_list) = parse_graph_elem_list t1
           in
             (t2, elem :: elem_list)
           end
  end

  (* HERE <attribute>? <graphElem>* *)
  fun parse_figure (t0 : Token.Token list) : Ast.Figure =
  let
    val parse_after_attrib =
      fn (tokens, opt_attr) =>
        case tokens of
             [] => (Ast.Fig (opt_attr, []))
           | _ =>
               let
                 val (rest_tok, elem_list) = parse_graph_elem_list tokens
               in
                 case rest_tok of
                      [] => Ast.Fig (opt_attr, elem_list)
                    | _ =>
                        (* not all tokens have been used *)
                        syntax_error_at rest_tok
               end
  in
    case t0 of
         ((Token.OpenBracket, _)::t1) =>
           let
             val (t2, attr) = parse_attribute t1
           in
             parse_after_attrib (t2, SOME attr)
           end
       | _ => parse_after_attrib (t0, NONE)
  end

end
