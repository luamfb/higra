fun get_vertex_name (Ast.Vertex ((Ast.Id (s, _)), _, _)) = s

(* traverse a list of (name, value) pairs to find a value whose name
 * matches the given string
 *)
fun find_value_of (s : string) ([] : (string * 'a) list) : 'a option = NONE
  | find_value_of (s : string) ((name, value)::rest) =
      if name = s then
        SOME value
      else
        find_value_of s rest

structure Sema = struct
  exception SemaError (* TODO store position here by traversing AST *)

  datatype Shape = Circle | Rect
  datatype EdgeType = Cont
                    | ContDir
                    | Dotted
                    | DottedDir
  type Color = int * int * int

  type VertexAttrib = Color * Shape * int
  type EdgeAttrib = Color
  type FigAttrib = string * bool

  type VertexInfo = string * VertexAttrib
  type State = (string * VertexInfo) list
  type Edge = VertexInfo * VertexInfo * EdgeType * EdgeAttrib

  datatype Graph = Graph of VertexInfo * (Edge list) * (Graph list)

  type Figure = FigAttrib * Graph

  val default_color = (0, 0, 0)
  val default_shape = Circle
  val default_font = "Times"
  val default_size = 14
  val default_horiz = true

  val vertex_defaults = (default_color, default_shape, default_size)
  val edge_defaults = default_color
  val fig_defaults = (default_font, default_horiz)

  fun sema_edge_type (Ast.ContNonDirEdge _) : EdgeType = Cont
    | sema_edge_type (Ast.ContDirEdge _) = ContDir
    | sema_edge_type (Ast.DottedNonDirEdge _) = Dotted
    | sema_edge_type (Ast.DottedDirEdge _) = DottedDir

  fun lit_num (Ast.LitNum (n, _)) : int = n
    | lit_num _ = raise SemaError

  fun lit_str (Ast.LitStr (s, _)) : string = s
    | lit_str _ = raise SemaError

  fun lit_bool (Ast.LitBool (b, _)) : bool = b
    | lit_bool _ = raise SemaError

  fun lit_color (Ast.LitColor (c, _)) : Color = c
    | lit_color _ = raise SemaError

  fun choose_non_def v1 v2 v_def =
    if v2 = v_def then
      v1
    else if v1 = v_def then
      v2
    else
      raise SemaError

  fun edge_attr (Ast.Attr [((Ast.Id ("color", _)), lit)]) = lit_color lit
    | edge_attr _ = raise SemaError

  fun fig_attr (Ast.Attr []) : FigAttrib = fig_defaults
    | fig_attr (Ast.Attr [(Ast.Id ("font", _), lit)]) =
    (lit_str lit, default_horiz)
    | fig_attr (Ast.Attr [(Ast.Id ("horiz", _), lit)]) =
    (default_font, lit_bool lit)
    | fig_attr (Ast.Attr [(_, _)]) = raise SemaError
    | fig_attr (Ast.Attr ((id, lit)::rest)) =
    let
      val (font0, horiz0) = fig_attr (Ast.Attr [(id, lit)])
      val (font1, horiz1) = fig_attr (Ast.Attr rest)
      val font = choose_non_def font0 font1 default_font
      val horiz = choose_non_def horiz0 horiz1 default_horiz
    in
      (font, horiz)
    end

  fun vertex_attr (Ast.Attr []) : VertexAttrib = vertex_defaults
    | vertex_attr (Ast.Attr [(Ast.Id ("color", _), lit)]) =
    (lit_color lit, default_shape, default_size)
    | vertex_attr (Ast.Attr [(Ast.Id ("fontsize", _), lit)]) =
    (default_color, default_shape, lit_num lit)

    | vertex_attr (Ast.Attr [(Ast.Id("shape", _), Ast.LitStr("circle", _))]) =
    (default_color, Circle, default_size)
    | vertex_attr (Ast.Attr [(Ast.Id("shape", _), Ast.LitStr("rect", _))]) =
    (default_color, Rect, default_size)

    | vertex_attr (Ast.Attr [(_, _)]) = raise SemaError
    | vertex_attr (Ast.Attr ((id, lit)::rest)) =
    let
      val (color0, shape0, size0) = vertex_attr (Ast.Attr [(id,lit)])
      val (color1, shape1, size1) = vertex_attr (Ast.Attr rest)
      val color = choose_non_def color0 color1 default_color
      val shape = choose_non_def shape0 shape1 default_shape
      val size = choose_non_def size0 size1 default_size
    in
      (color, shape, size)
    end

  fun sema_vertex (Ast.Vertex (Ast.Id(s, _), opt_l, opt_a)) (state : State)
    : State * VertexInfo =
    let
      val vertex_info =
        case (opt_l, opt_a) of
             (NONE, NONE) => (s, vertex_defaults)
           | (SOME label, NONE) => (lit_str label, vertex_defaults)
           | (NONE, SOME attr) => (s, vertex_attr attr)
           | (SOME label, SOME attr) => (lit_str label, vertex_attr attr)

      val new_state : State = (s, vertex_info) :: state
    in
      case find_value_of s state of
           SOME _ => raise SemaError
         | NONE => (new_state, vertex_info)
    end

  fun get_or_make_vertex (v : Ast.Vertex) (state : State)
    : State * VertexInfo =
    case find_value_of (get_vertex_name v) state of
         NONE => sema_vertex v state
       | SOME x => (state, x)

  fun sema_edge (Ast.Edge (from, to, edge_type, opt_attr)) (state0 : State)
    : State * Edge =
    let
      val attr_val = case opt_attr of
                          NONE => edge_defaults
                        | SOME attr => edge_attr attr
      val (state1, v_from) = get_or_make_vertex from state0
      val (state2, v_to) = get_or_make_vertex to state1
      val edge_oper = sema_edge_type edge_type
    in
      (state2, (v_from, v_to, edge_oper, attr_val))
    end

  fun sema_graph (Ast.Graph (vertex, elems)) (state0 : State)
    : State * Graph =
    let
      val (state1, v) = sema_vertex vertex state0
      val (state2, edges, subgraphs) = sema_elem_list elems state1
    in
      (state2, Graph (v, edges, subgraphs))
    end
  and sema_elem_list (elems : Ast.GraphElem list) (state : State)
    : State * Edge list * Graph list =
    case elems of
         [] => (state, [], [])
       | ((Ast.ElemE e)::rest) =>
           let
             val (state1, cur_edge) = sema_edge e state
             val (state2, edges, subgraphs) = sema_elem_list rest state1
           in
             (state2, cur_edge :: edges, subgraphs)
           end
       | ((Ast.ElemG g)::rest) =>
           let
             val (state1, cur_subgraph) = sema_graph g state
             val (state2, edges, subgraphs) = sema_elem_list rest state1
           in
             (state2, edges, cur_subgraph :: subgraphs)
           end

  fun sema_figure (Ast.Fig (opt_attr, elem_list)) : Figure =
  let
    val toplevel_id = Ast.Id ("_toplevel", {line=0, col=0})
    val toplevel = Ast.Vertex (toplevel_id, NONE, NONE)
    val empty_state : State = []
    val attr : FigAttrib = case opt_attr of
                                NONE => fig_defaults
                              | SOME a => fig_attr a
    val (_, figure : Graph) =
      sema_graph (Ast.Graph (toplevel, elem_list)) empty_state
  in
    (attr, figure)
  end

end
