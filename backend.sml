val xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

(* TODO dynamically compute width / height based on drawing size *)
val svg_header =
  "<svg width=\"700\" height=\"700\" xmlns=\"http://www.w3.org/2000/svg\">"
val svg_footer = "</svg>"

exception InternalError

fun get_vertex_info (id : Sema.VertexId) ([] : Sema.State) : Sema.VertexInfo =
  raise InternalError
  | get_vertex_info id ((name, info)::rest) =
  if name = id then
    info
  else
    get_vertex_info id rest

structure Backend = struct
  (* All dimensions are measured in pixels. *)

  (* separation between vertices *)
  val vertex_sep = 25

  (* offset by which to displace an edge if there are vertices in the way *)
  val edge_displ = vertex_sep div 2

  (* mapping of vertices drawing on screen and their bounding boxes *)
  type Drawing = (Sema.VertexId * Geom.Box) list

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
    ((width, height) : int * int) : Drawing * Geom.Point =
    let
      val (x, y) = Geom.next_free_topleft vertex_sep (box_list_from drawing)
      val box : Geom.Box = (x, y, width, height)
      val new_drawing : Drawing = (id, box)::drawing
    in
      (new_drawing, (x, y))
    end

  fun place_vertex_near_to (id : Sema.VertexId) (drawing : Drawing)
    (near : Geom.Box) ((width, height) : int * int) : Drawing * Geom.Point =
    let
      val box_list = box_list_from drawing
      val box =
        Geom.free_box_near_to vertex_sep near (width, height) box_list
      val (x, y, _, _) = box
      val new_drawing : Drawing = (id, box)::drawing
    in
      (new_drawing, (x, y))
    end

  (* TODO take vertex attributes into account *)
  fun gen_vertex_svg (id : Sema.VertexId) ((v_x, v_y) : int * int)
    ((vertex_rx, vertex_ry) : int * int) (label : string) =
    "<g id=\"" ^ id ^ "\" class=\"vertex\">\n"
    ^ "<ellipse fill=\"none\" stroke=\"black\" cx=\"" ^ (Int.toString v_x)
    ^ "\" cy=\"" ^ (Int.toString v_y)
    ^ "\" rx=\"" ^ (Int.toString vertex_rx)
    ^ "\" ry=\"" ^ (Int.toString vertex_ry)
    ^ "\" />\n<text text-anchor=\"middle\" x=\"" ^ (Int.toString v_x)
    ^ "\" y =\"" ^ (Int.toString v_y)
    ^ "\" font-family=\"Times\" font-size=\"14\">" ^ label ^ "</text>\n"
    ^ "</g>"

  fun draw_vertex (id : Sema.VertexId) (state : Sema.State) (drawing : Drawing)
    (opt_near : Geom.Box option) : Drawing * Geom.Box * string =
  let
    val (label, v_attr) = get_vertex_info id state

    (* TODO compute dynamically depending on text length *)
    val vertex_rx = 30
    val vertex_ry = 30

    val width = 2 * vertex_rx
    val height = 2 * vertex_ry

    val (new_drawing, (v_topleft_x, v_topleft_y)) =
      case opt_near of
           NONE => place_free_vertex id drawing (width, height)
         | SOME near => place_vertex_near_to id drawing near (width, height)

    val v_x = v_topleft_x + (width div 2)
    val v_y = v_topleft_y + (height div 2)

    val svg = gen_vertex_svg id (v_x, v_y) (vertex_rx, vertex_ry) label
    val box = (v_topleft_x, v_topleft_y, width, height)
  in
    (new_drawing, box, svg)
  end

  fun draw_free_vertex (id : Sema.VertexId) (state : Sema.State)
    (drawing : Drawing) : Drawing * Geom.Box * string =
    draw_vertex id state drawing NONE

  fun draw_vertex_near_to (id : Sema.VertexId) (near : Geom.Box)
    (state : Sema.State) (drawing : Drawing)
    : Drawing * Geom.Box * string =
    draw_vertex id state drawing (SOME near)

  fun draw_vertices_if_needed ((v_from, v_to) : Sema.VertexId * Sema.VertexId)
    (state : Sema.State) (drawing0 : Drawing)
    : Drawing * Geom.Box * Geom.Box * string =
    case (get_vertex_box v_from drawing0, get_vertex_box v_to drawing0) of
         (NONE, NONE) =>
         let
           val (drawing1, from_box, svg1) =
             draw_free_vertex v_from state drawing0
           val (drawing2, to_box, svg2) =
             draw_vertex_near_to v_to from_box state drawing1
           in
             (drawing2, from_box, to_box, svg1 ^ "\n" ^ svg2)
           end
       | (SOME from_box, NONE) =>
           let
             val (drawing1, to_box, svg1) =
               draw_vertex_near_to v_to from_box state drawing0
           in
             (drawing1, from_box, to_box, svg1)
           end
       | (NONE, SOME to_box) =>
           let
             val (drawing1, from_box, svg1) =
               draw_vertex_near_to v_from to_box state drawing0
           in
             (drawing1, from_box, to_box, svg1)
           end
       | (SOME from_box, SOME to_box) =>
           (drawing0, from_box, to_box, "")

  fun gen_edge_svg ((v_from, v_to, e_type, attr) : Sema.Edge)
    (from_box : Geom.Box) (to_box : Geom.Box) =
    let
      val ((x1, y1), (x2, y2)) =
        Geom.box_connector_line from_box to_box edge_displ
    in
      (* TODO take edge color / type into account *)
      "<g class=\"edge\">\n<line x1=\"" ^ (Int.toString x1)
      ^ "\" y1 = \"" ^ (Int.toString y1)
      ^ "\" x2 = \"" ^ (Int.toString x2)
      ^ "\" y2 = \"" ^ (Int.toString y2)
      ^ "\" stroke = \"black\"/>\n</g>"
    end

  fun draw_edge (edge : Sema.Edge)
    (state : Sema.State) (drawing0 : Drawing) : Drawing * string =
    let
      val (v_from, v_to, e_type, attr) = edge
      val (drawing, from_box, to_box, vertex_svg) =
        draw_vertices_if_needed (v_from, v_to) state drawing0
      val edge_svg = gen_edge_svg edge from_box to_box
    in
      (drawing, vertex_svg ^ "\n" ^ edge_svg)
    end

  fun draw_edge_list [] (state : Sema.State) (drawing : Drawing)
    : string = ""
    | draw_edge_list ((e::es) : Sema.Edge list) state drawing =
    let
      val (new_drawing, edge_svg) = (draw_edge e state drawing)
    in
      edge_svg ^ "\n" ^ (draw_edge_list es state new_drawing)
    end

  fun draw_svg ((fig_attr, Sema.Graph (_, edges, graphs), state) : Sema.Figure)
    : string =
    (* TODO also recurse for each subgraph *)
    xml_header ^ "\n" ^ svg_header ^ "\n"
    ^ (draw_edge_list edges state [])
    ^ svg_footer ^ "\n"
end
