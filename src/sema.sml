fun get_vertex_name (Ast.Vertex ((Ast.Id (s, _)), _, _)) = s

(* traverse a list of (name, value) pairs to find if some value has the
 * same name as the given string s
 *)
fun contains_id (s : string) ([] : (string * 'a) list) : bool = false
  | contains_id (s : string) ((name, _)::rest) =
  if name = s then
    true
  else
    contains_id s rest

structure Sema = struct
  exception SemaError of Token.SourcePos * Token.SourcePos

  datatype Shape = Circle | Rect
  datatype EdgeType = Cont
                    | ContDir
                    | Dotted
                    | DottedDir
  type Color = int * int * int

  type VertexAttrib = Color * Shape * int
  type EdgeAttrib = Color
  type FigAttrib = string * bool

  type VertexId = string
  type VertexInfo = string * VertexAttrib
  type State = (VertexId * VertexInfo) list
  type Edge = VertexId * VertexId * EdgeType * EdgeAttrib

  datatype Graph = Graph of VertexId * (Edge list) * (Graph list)

  type Figure = FigAttrib * Graph * State

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
    | lit_num lit = raise SemaError (Ast.literal_range lit)

  fun lit_str (Ast.LitStr (s, _)) : string = s
    | lit_str lit = raise SemaError (Ast.literal_range lit)

  fun lit_bool (Ast.LitBool (b, _)) : bool = b
    | lit_bool lit = raise SemaError (Ast.literal_range lit)

  fun lit_color (Ast.LitColor (c, _)) : Color = c
    | lit_color lit = raise SemaError (Ast.literal_range lit)

  fun choose_non_def v1 v2 v_def range =
    if v2 = v_def then
      v1
    else if v1 = v_def then
      v2
    else
      raise SemaError range

  fun edge_attr (Ast.Attr [((Ast.Id ("color", _)), lit)]) = lit_color lit
    | edge_attr attr = raise SemaError (Ast.attr_range attr)

  fun fig_attr (Ast.Attr name_val) : FigAttrib =
    case name_val of
         [] => fig_defaults
       | [(Ast.Id ("font", _), lit)] => (lit_str lit, default_horiz)
       | [(Ast.Id ("horiz", _), lit)] => (default_font, lit_bool lit)
       | [(_, _)] =>
           raise SemaError (Ast.attr_range (Ast.Attr name_val))
       | (id, lit)::rest =>
           let
             val (font0, horiz0) = fig_attr (Ast.Attr [(id, lit)])
             val (font1, horiz1) = fig_attr (Ast.Attr rest)
             val range = Ast.attr_range (Ast.Attr name_val)
             val font = choose_non_def font0 font1 default_font range
             val horiz = choose_non_def horiz0 horiz1 default_horiz range
           in
             (font, horiz)
           end

  fun vertex_attr (Ast.Attr name_val) : VertexAttrib =
    case name_val of
         [] => vertex_defaults
       | [(Ast.Id ("color", _), lit)] =>
           (lit_color lit, default_shape, default_size)
       | [(Ast.Id ("fontsize", _), lit)] =>
           (default_color, default_shape, lit_num lit)
       | [(Ast.Id("shape", _), Ast.LitStr("circle", _))] =>
           (default_color, Circle, default_size)
       | [(Ast.Id("shape", _), Ast.LitStr("rect", _))] =>
           (default_color, Rect, default_size)
       | [(_, _)] =>
           raise SemaError (Ast.attr_range (Ast.Attr name_val))
       | ((id, lit)::rest) =>
           let
             val (color0, shape0, size0) = vertex_attr (Ast.Attr [(id,lit)])
             val (color1, shape1, size1) = vertex_attr (Ast.Attr rest)
             val range = Ast.attr_range (Ast.Attr name_val)
             val color = choose_non_def color0 color1 default_color range
             val shape = choose_non_def shape0 shape1 default_shape range
             val size = choose_non_def size0 size1 default_size range
           in
             (color, shape, size)
           end

  fun sema_vertex (Ast.Vertex v) (state : State) : State * VertexId =
    let
      val (Ast.Id(s, _), opt_l, opt_a) = v
      val vertex_info =
        case (opt_l, opt_a) of
             (NONE, NONE) => (s, vertex_defaults)
           | (SOME label, NONE) => (lit_str label, vertex_defaults)
           | (NONE, SOME attr) => (s, vertex_attr attr)
           | (SOME label, SOME attr) => (lit_str label, vertex_attr attr)

      val new_state = (s, vertex_info) :: state
    in
      if contains_id s state then
        raise SemaError (Ast.vertex_range (Ast.Vertex v))
      else
        (new_state, s)
    end

  fun get_or_make_vertex (v : Ast.Vertex) (state : State) : State * VertexId =
  let
    val id = get_vertex_name v
  in
    if contains_id id state then
      (state, id)
    else
      sema_vertex v state
  end

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
    val (last_state, figure : Graph) =
      sema_graph (Ast.Graph (toplevel, elem_list)) empty_state
  in
    (attr, figure, last_state)
  end

end
