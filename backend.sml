val xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

exception InternalError

fun get_vertex_info (id : Sema.VertexId) ([] : Sema.State) : Sema.VertexInfo =
  raise InternalError
  | get_vertex_info id ((name, info)::rest) =
  if name = id then
    info
  else
    get_vertex_info id rest

fun is_edge_directed (e_type : Sema.EdgeType) : bool =
  (e_type = Sema.ContDir) orelse (e_type = Sema.DottedDir)

fun is_edge_dotted (e_type : Sema.EdgeType) : bool =
  (e_type = Sema.Dotted) orelse (e_type = Sema.DottedDir)

structure Backend = struct
  (* All dimensions are measured in pixels. *)

  (* separation between vertices *)
  val vertex_sep = 25

  (* offset by which to displace an edge if there are vertices in the way *)
  val edge_displ = vertex_sep div 2

  (* size of a directed edge's arrow head *)
  val arrow_size = 5

  (* size of dashes and gaps in dotted edges *)
  val dash_size = 2

  (* mapping of vertices drawing on screen and their bounding boxes *)
  type Drawing = (Sema.VertexId * Geom.Box) list

  fun gen_color_svg ((r,g,b) : Sema.Color) : string =
    "rgb("
    ^ (Int.toString r) ^ ","
    ^ (Int.toString g) ^ ","
    ^ (Int.toString b) ^ ")"

  fun box_list_from ([] : Drawing) : Geom.Box list = []
    | box_list_from ((_, box):: rest) = box::(box_list_from rest)

  fun get_vertex_box (id : Sema.VertexId) ([] : Drawing) : Geom.Box option =
    NONE
    | get_vertex_box id ((cur_id, box)::rest) =
    if id = cur_id then
      SOME box
    else
      get_vertex_box id rest

  (* always tries to walk towards the primary dimension:
   * left if horizontal is true, down otherwise *)
  fun place_free_vertex (id : Sema.VertexId) (drawing : Drawing)
    ((width, height) : int * int) (horiz : bool) : Drawing * Geom.Point =
    let
      val (x, y) =
        Geom.next_free_topleft vertex_sep (box_list_from drawing) horiz
      val box : Geom.Box = (x, y, width, height)
      val new_drawing : Drawing = (id, box)::drawing
    in
      (new_drawing, (x, y))
    end

  fun place_vertex_near_to (id : Sema.VertexId) (drawing : Drawing)
    (near : Geom.Box) ((width, height) : int * int) (horiz : bool)
    : Drawing * Geom.Point =
    let
      val box_list = box_list_from drawing
      val box =
        Geom.free_box_near_to vertex_sep near (width, height) box_list horiz
      val (x, y, _, _) = box
      val new_drawing : Drawing = (id, box)::drawing
    in
      (new_drawing, (x, y))
    end

  fun gen_shape_svg ((x, y, width, height) : Geom.Box) (shape : Sema.Shape)
    : string =
    case shape of
         Sema.Rect =>
           "<rect fill=\"none\" x=\"" ^ (Int.toString x)
           ^ "\" y= \"" ^ (Int.toString y)
           ^ "\" width=\"" ^ (Int.toString width)
           ^ "\" height=\"" ^ (Int.toString height)
           ^ "\"/>\n"
       | Sema.Circle =>
           let
             val radius_x = width div 2
             val radius_y = height div 2
             val center_x = x + radius_x
             val center_y = y + radius_y
           in
             "<ellipse fill=\"none\" cx=\"" ^ (Int.toString center_x)
             ^ "\" cy=\"" ^ (Int.toString center_y)
             ^ "\" rx=\"" ^ (Int.toString radius_x)
             ^ "\" ry=\"" ^ (Int.toString radius_y)
             ^ "\" />\n"
           end

  fun gen_vertex_svg (id : Sema.VertexId) (box : Geom.Box) (label : string)
    ((color, shape, fontsize) : Sema.VertexAttrib) (font : string) : string =
    let
      val (center_x, center_y) = Geom.box_center box
    in
      "<g id=\"" ^ id ^ "\" class=\"vertex\" stroke=\""
      ^ (gen_color_svg color) ^ "\" fill=\""
      ^ (gen_color_svg color) ^ "\">\n"
      ^ (gen_shape_svg box shape)
      ^ "<text text-anchor=\"middle\" stroke-width=\"0\" x=\""
      ^ (Int.toString center_x)
      ^ "\" y =\"" ^ (Int.toString center_y)
      ^ "\" font-family=\"" ^ font
      ^ "\" font-size=\"" ^ (Int.toString fontsize)
      ^ "\">" ^ label ^ "</text>\n</g>"
    end

  fun draw_vertex (id : Sema.VertexId) (state : Sema.State) (drawing : Drawing)
    (fig_attr : Sema.FigAttrib) (opt_near : Geom.Box option)
    : Drawing * Geom.Box * string =
  let
    val (font, horiz) = fig_attr
    val (label, v_attr) = get_vertex_info id state

    (* FIXME should be computed dynamically depending on text and font *)
    val width = 60
    val height = 60

    val (new_drawing, (topleft_x, topleft_y)) =
      case opt_near of
           NONE => place_free_vertex id drawing (width, height) horiz
         | SOME near =>
             place_vertex_near_to id drawing near (width, height) horiz

    val box = (topleft_x, topleft_y, width, height)
    val svg = gen_vertex_svg id box label v_attr font
  in
    (new_drawing, box, svg)
  end

  fun draw_free_vertex (id : Sema.VertexId) (state : Sema.State)
    (fig_attr : Sema.FigAttrib) (drawing : Drawing)
    : Drawing * Geom.Box * string =
    draw_vertex id state drawing fig_attr NONE

  fun draw_vertex_near_to (id : Sema.VertexId) (near : Geom.Box)
    (state : Sema.State) (fig_attr : Sema.FigAttrib) (drawing : Drawing)
    : Drawing * Geom.Box * string =
    draw_vertex id state drawing fig_attr (SOME near)

  fun draw_vertices_if_needed ((v_from, v_to) : Sema.VertexId * Sema.VertexId)
    (state : Sema.State) (fig_attr : Sema.FigAttrib) (drawing0 : Drawing)
    : Drawing * Geom.Box * Geom.Box * string =
    case (get_vertex_box v_from drawing0, get_vertex_box v_to drawing0) of
         (NONE, NONE) =>
         let
           val (drawing1, from_box, svg1) =
             draw_free_vertex v_from state fig_attr drawing0
           val (drawing2, to_box, svg2) =
             draw_vertex_near_to v_to from_box state fig_attr drawing1
           in
             (drawing2, from_box, to_box, svg1 ^ "\n" ^ svg2)
           end
       | (SOME from_box, NONE) =>
           let
             val (drawing1, to_box, svg1) =
               draw_vertex_near_to v_to from_box state fig_attr drawing0
           in
             (drawing1, from_box, to_box, svg1)
           end
       | (NONE, SOME to_box) =>
           let
             val (drawing1, from_box, svg1) =
               draw_vertex_near_to v_from to_box state fig_attr drawing0
           in
             (drawing1, from_box, to_box, svg1)
           end
       | (SOME from_box, SOME to_box) =>
           (drawing0, from_box, to_box, "")

  fun gen_arrowhead_svg ((_, _, e_type, _) : Sema.Edge)
    (from : Geom.Point) (to : Geom.Point) : string =
    if not (is_edge_directed e_type) then
      ""
    else
      let
        val (x0, y0) = to
        val ((x1, y1), (x2, y2)) = Geom.arrowhead_points from to arrow_size
      in
        "<path d=\"M" ^ (Int.toString x0) ^ "," ^ (Int.toString y0)
        ^ " L" ^ (Int.toString x1) ^ "," ^ (Int.toString y1)
        ^ " L" ^ (Int.toString x2) ^ "," ^ (Int.toString y2)
        ^ " Z\"/>\n"
      end

  fun gen_dash_svg ((_, _, e_type, _) : Sema.Edge) : string =
    if not (is_edge_dotted e_type) then
      ""
    else
      " stroke-dasharray=\"" ^ (Int.toString dash_size) ^ "\""

  fun gen_edge_svg (edge : Sema.Edge)
    ((x1, y1) : Geom.Point) ((x2, y2) : Geom.Point) =
    let
      val (_, _, e_type, attr) = edge
      val arrowhead_svg = gen_arrowhead_svg edge (x1, y1) (x2, y2)
      val dash_svg = gen_dash_svg edge
      val color_svg = gen_color_svg attr
    in
      "<g class=\"edge\" stroke=\""
      ^ color_svg ^ "\" fill=\""
      ^ color_svg ^ "\">\n<line x1=\"" ^ (Int.toString x1)
      ^ "\" y1 = \"" ^ (Int.toString y1)
      ^ "\" x2 = \"" ^ (Int.toString x2)
      ^ "\" y2 = \"" ^ (Int.toString y2)
      ^ "\"" ^ dash_svg ^ "/>"
      ^ arrowhead_svg ^ "\n</g>"
    end

  (* an indirect edge takes a "detour" because another vertex is in
   * the way of a direct edge joining the vertices' centers *)
  fun gen_indirect_edge_svg (edge : Sema.Edge)
    (from_box : Geom.Box) (to_box : Geom.Box) =
    let
      val (_, _, e_type, attr) = edge
      val ((x0, y0), (x1, y1), (x2, y2), (x3, y3)) =
        Geom.compute_indirect_path from_box to_box edge_displ
      val arrowhead_svg = gen_arrowhead_svg edge (x2, y2) (x3, y3)
      val dash_svg = gen_dash_svg edge
      val color_svg = gen_color_svg attr
    in
      "<g class=\"indirect_edge\" stroke=\""
      ^ color_svg ^ "\" fill=\""
      ^ color_svg ^ "\">\n<path d=\"M"
      ^ (Int.toString x0) ^ "," ^ (Int.toString y0)
      ^ " L" ^ (Int.toString x1) ^ "," ^ (Int.toString y1)
      ^ " L" ^ (Int.toString x2) ^ "," ^ (Int.toString y2)
      ^ " L" ^ (Int.toString x3) ^ "," ^ (Int.toString y3)
      ^ "\" fill=\"none\"" ^ dash_svg ^ "/>\n"
      ^ arrowhead_svg ^ "\n</g>"
    end

  fun draw_edge (edge : Sema.Edge) (state : Sema.State)
    (fig_attr: Sema.FigAttrib) (drawing0 : Drawing) : Drawing * string =
    let
      val (v_from, v_to, e_type, attr) = edge
      val (drawing1, from_box, to_box, vertex_svg) =
        draw_vertices_if_needed (v_from, v_to) state fig_attr drawing0
      val (p1, p2) = Geom.box_connector_line from_box to_box
      val box_list = box_list_from drawing1
      val edge_svg =
        if Geom.line_segment_is_free p1 p2 box_list then
          gen_edge_svg edge p1 p2
        else
          gen_indirect_edge_svg edge from_box to_box
    in
      (drawing1, vertex_svg ^ "\n" ^ edge_svg)
    end

  fun draw_edge_list [] (state : Sema.State) (fig_attr: Sema.FigAttrib)
    (drawing0 : Drawing) : Drawing * string = (drawing0, "")
    | draw_edge_list ((e::es) : Sema.Edge list) state fig_attr drawing0 =
    let
      val (drawing1, edge_svg) = draw_edge e state fig_attr drawing0
      val (drawing2, rest_svg) = draw_edge_list es state fig_attr drawing1
      val full_svg = edge_svg ^ "\n" ^ rest_svg
    in
      (drawing2, full_svg)
    end

  fun draw_svg ((fig_attr, Sema.Graph (_, edges, graphs), state) : Sema.Figure)
    : string =
    let
      val (drawing, edges_svg) = draw_edge_list edges state fig_attr []
      val (max_x, max_y) = Geom.box_list_max_dim (box_list_from drawing)
      val fig_w = max_x + vertex_sep
      val fig_h = max_y + vertex_sep
      val svg_width = 500
      val svg_height = 500
      val svg_header =
        "<svg width=\"" ^ (Int.toString svg_width)
        ^ "\" height=\"" ^ (Int.toString svg_height)
        ^ "\" viewBox=\"0 0 " ^ (Int.toString fig_w)
        ^ " " ^ (Int.toString fig_h)
        ^ "\" xmlns=\"http://www.w3.org/2000/svg\">\n"
    in
      (* TODO also recurse for each subgraph *)
      xml_header ^ svg_header ^ edges_svg ^ "</svg>\n"
    end
end
