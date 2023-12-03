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

  (* top-left x, top-left y, width, height *)
  type Box = int * int * int * int

  (* mapping of vertices drawing on screen and their bounding boxes *)
  type Drawing = (Sema.VertexId * Box) list

  fun drawing_max_dim ([] : Drawing) : int * int = (0, 0)
    | drawing_max_dim ((_, (x, y, width, height))::rest) =
    let
      val cur_x = x + width
      val cur_y = y + height
      val (rest_x, rest_y) = drawing_max_dim rest
      val max_x = if cur_x > rest_x then cur_x else rest_x
      val max_y = if cur_y > rest_y then cur_y else rest_y
    in
      (max_x, max_y)
    end

  fun get_vertex_box (id : Sema.VertexId) ([] : Drawing) : Box option =
    NONE
    | get_vertex_box id ((cur_id, box)::rest) =
    if id = cur_id then
      SOME box
    else
      get_vertex_box id rest

  (* always tries to walk towards the primary dimension:
   * left if horizontal is true, down otherwise *)
  fun place_free_vertex (id : Sema.VertexId) (drawing : Drawing)
    ((width, height) : int * int) : Drawing * (int * int) =
    let
      (* TODO actually take horizontal into account *)
      val (max_x, _) = drawing_max_dim drawing
      val x = if max_x > 0 then max_x + vertex_sep else 0
      val y = 0
      val box : Box = (x, y, width, height)
      val new_drawing : Drawing = (id, box)::drawing
    in
      (new_drawing, (x, y))
    end

  fun draw_vertex (id : Sema.VertexId) (state : Sema.State) (drawing : Drawing)
    : Drawing * string =
  let
    (* TODO take vertex attributes into account *)
    val (label, v_attr) = get_vertex_info id state

    (* TODO compute dynamically depending on text length *)
    val vertex_rx = 30
    val vertex_ry = 30

    val width = 2 * vertex_rx
    val height = 2 * vertex_ry

    val (new_drawing, (v_topleft_x, v_topleft_y)) =
      place_free_vertex id drawing (width, height)

    (* TODO take shape into account *)
    val v_x = v_topleft_x + (width div 2)
    val v_y = v_topleft_y + (height div 2)

    val vertex_svg =
      "<g id=\"" ^ id ^ "\" class=\"vertex\">\n"
      ^ "<ellipse fill=\"none\" stroke=\"black\" cx=\"" ^ (Int.toString v_x)
      ^ "\" cy=\"" ^ (Int.toString v_y)
      ^ "\" rx=\"" ^ (Int.toString vertex_rx)
      ^ "\" ry=\"" ^ (Int.toString vertex_ry)
      ^ "\" />\n<text text-anchor=\"middle\" x=\"" ^ (Int.toString v_x)
      ^ "\" y =\"" ^ (Int.toString v_y)
      ^ "\" font-family=\"Times\" font-size=\"14\">" ^ label ^ "</text>\n"
      ^ "</g>"
  in
    (new_drawing, vertex_svg)
  end

  fun draw_edge ((v_from, v_to, e_type, attr) : Sema.Edge)
    (state : Sema.State) (drawing0 : Drawing) : Drawing * string =
    let
      val opt_from_box = get_vertex_box v_from drawing0
      val opt_to_box = get_vertex_box v_to drawing0
      val (drawing, svg) =
        case (opt_from_box, opt_to_box) of
             (NONE, NONE) =>
             let
               val (drawing1, svg1) = draw_vertex v_from state drawing0
               val (drawing2, svg2) = draw_vertex v_to state drawing1
             in
               (drawing2, svg1 ^ "\n" ^ svg2)
             end
           (* TODO When positioning a vertex that has an edge to
            * another one that's already been positioned,
            * make it as close as possible to the original one *)
           | (SOME from_box, NONE) => draw_vertex v_to state drawing0
           | (NONE, SOME to_box) => draw_vertex v_from state drawing0
           | _ => (drawing0, "")
    in
      (* FIXME draw actual edge too, not just vertices
       *    for that, we'll probably need the bounding boxes too;
       *    make draw_vertex return that
       *)
      (drawing, svg)
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
