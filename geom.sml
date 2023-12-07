exception InternalError

structure Geom = struct
  (* top-left x, top-left y, width, height *)
  type Point = int * int

  (* top-left x, top-left y, width, height *)
  type Box = int * int * int * int

  datatype Line = Vertical of int (* x coord *)
                     | Horizontal of int (* y coord *)
                     (* slope, inv_slope, offset *)
                     | Oblique of real * real * real

  fun box_list_max_dim ([] : Box list) : int * int = (0, 0)
    | box_list_max_dim ((x, y, width, height)::rest) =
    let
      val cur_x = x + width
      val cur_y = y + height
      val (rest_x, rest_y) = box_list_max_dim rest
      val max_x = if cur_x > rest_x then cur_x else rest_x
      val max_y = if cur_y > rest_y then cur_y else rest_y
    in
      (max_x, max_y)
    end

  fun next_free_topleft (sep : int) (box_list : Box list) (horiz : bool)
    : Point =
  let
    val (max_x, max_y) = box_list_max_dim box_list
  in
    if horiz then
      (max_x + sep, sep)
    else
      (sep, max_y + sep)
  end

  fun point_right_to (sep : int) (near : Box) : Point =
  let
    val (near_x, near_y, near_w, near_h) = near
    val next_x = near_x + near_w + sep
    val next_y = near_y
  in
    (next_x, next_y)
  end

  fun point_down_to (sep : int) (near : Box) : Point =
  let
    val (near_x, near_y, near_w, near_h) = near
    val next_x = near_x
    val next_y = near_y + near_h + sep
  in
    (next_x, next_y)
  end

  fun box_intersects ((left1,top1,w1,h1) : Box) ((left2,top2,w2,h2) : Box)
    : bool =
  let
    val (right1, bottom1) = (left1 + w1, top1 + h1)
    val (right2, bottom2) = (left2 + w2, top2 + h2)
  in
    not (right1 < left2
      orelse left1 > right2
      orelse bottom1 < top2
      orelse top1 > bottom2)
  end

  fun box_center ((x, y, width, height) : Box) : Point =
  let
    val x_center = x + (height div 2)
    val y_center = y + (width div 2)
  in
    (x_center, y_center)
  end

  fun box_is_free (box : Box) ([] : Box list) = true
    | box_is_free (box : Box) (cur::rest) =
    if (box_intersects box cur) then
      false
    else
      box_is_free box rest

  fun free_box_near_to (sep : int) (near : Box) (width: int, height: int)
    (box_list : Box list) (horiz : bool) : Box =
    let
      val (x_r, y_r) = point_right_to sep near
      val (x_d, y_d) = point_down_to sep near
      val box_r = (x_r, y_r, width, height)
      val box_d = (x_d, y_d, width, height)
      val (primary, secondary) =
        if horiz then
          (box_r, box_d)
        else
          (box_d, box_r)
    in
      if (box_is_free primary box_list) then
        primary
      else if (box_is_free secondary box_list) then
        secondary
      else
        (* FIXME should try diagonals as well *)
        let
          val (next_x, next_y) = next_free_topleft sep box_list horiz
        in
          (next_x, next_y, width, height)
        end
    end

  (* get (left, top, right, bottom) coordinates of a box *)
  fun box_coords ((left, top, width, height) : Box)
    : int * int * int * int =
    (left, top, left + width, top + height)

  fun line_between ((x1, y1) : Point) ((x2, y2) : Point) : Line =
    if (x1 = x2) then
      Vertical x1
    else if (y1 = y2)  then
      Horizontal y2
    else
      let
        (* the line is given by y = slope * x + offset,
         * or equivalently x = inv_slope * y - offset.
         * Here x_1 - x_2 != 0 and y_1 - y_2 != 0,
         * so neither slope nor inv_slope trigger division by zero *)
        val delta_y = Real.fromInt (y1 - y2)
        val delta_x = Real.fromInt (x1 - x2)
        val slope = delta_y / delta_x
        val inv_slope = delta_x / delta_y
        val offset = (Real.fromInt y1) - slope * (Real.fromInt x1)
      in
        Oblique (slope, inv_slope, offset)
      end

  fun point_in_line ((x_p, y_p) : Point) (line : Line) : bool =
    case line of
         Vertical x => x = x_p
       | Horizontal y => y = y_p
       | Oblique (slope, _, offset) =>
           y_p = round (slope * Real.fromInt(x_p) + offset)

  fun points_are_colinear (p1 : Point) (p2 : Point) (p3 : Point) : bool =
    point_in_line p3 (line_between p1 p2)

  (* check that point p is between p1 and p2 *)
  fun point_is_between (p : Point) (p1 : Point) (p2 : Point)  : bool =
    let
      val (x, y) = p
      val (x1, y1) = p1
      val (x2, y2) = p2
      val (x_min, x_max) = if x1 < x2 then (x1, x2) else (x2, x1)
      val (y_min, y_max) = if y1 < y2 then (y1, y2) else (y2, y1)
    in
      x >= x_min andalso x <= x_max andalso y >= y_min andalso y <= y_max
    end

  (* gives the intersection point between the given line and
   * the box's edge that is closest to a given point P.
   *)
  fun line_box_intersection (box : Box) (line : Line) ((x_p,y_p) : Point)
    : Point =
    let
      val (left, top, right, bot) = box_coords box
      val (x_c, y_c) = box_center box
    in
      case line of
           Vertical x => (x, (if y_p < top then top else bot))
         | Horizontal y => ((if x_p < left then left else right), y)
         | Oblique (slope, inv_slope, offset) =>
             let
               val line_x_at =
                 fn y => round (inv_slope * ((Real.fromInt y) - offset))
               val line_y_at =
                 fn x => round (slope * (Real.fromInt x) + offset)
               val x_within_box = fn x => x <= right andalso x >= left
               val y_within_box = fn y => y <= bot andalso y >= top

               val line_x_at_bot = line_x_at bot
               val line_x_at_top = line_x_at top
               val line_y_at_left = line_y_at left
               val line_y_at_right = line_y_at right
             in
               if x_p < x_c andalso y_within_box line_y_at_left then
                 (left, line_y_at_left)
               else if x_p > x_c andalso y_within_box line_y_at_right then
                 (right, line_y_at_right)
               else if y_p < y_c andalso x_within_box line_x_at_top then
                 (line_x_at_top, top)
               else if y_p > y_c andalso x_within_box line_x_at_bot then
                 (line_x_at_bot, bot)
               else
                 raise InternalError
        end
    end

  fun line_segment_is_free (p1 : Point) (p2 : Point) ([] : Box list) = true
    | line_segment_is_free p1 p2 (box::rest) =
    let
      val center = box_center box
      val is_between = (point_is_between center p1 p2)
    in
      if is_between andalso (points_are_colinear center p1 p2) then
          false
      else
        line_segment_is_free p1 p2 rest
    end

  (* gets the coordinates of a line connecting the boxes *)
  fun box_connector_line (box1 : Box) (box2 : Box) : Point * Point =
  let
    (*
    val (left1, top1, right1, bot1) = box_coords box1
    val (left2, top2, right2, bot2) = box_coords box2
    *)
    val center1 = box_center box1
    val center2 = box_center box2
    val line = line_between center1 center2

    (* TODO take vertex shape into account *)
    val p1 = line_box_intersection box1 line center2
    val p2 = line_box_intersection box2 line center1
  in
      (p1, p2)
  end

  fun compute_indirect_path (from_box : Box) (to_box : Box) (displ : int)
    : Point * Point * Point * Point =
    let
      val (_, _, right_from, bot_from) = box_coords from_box
      val (_, _, right_to, bot_to) = box_coords to_box
      val (x_center_from, y_center_from) = box_center from_box
      val (x_center_to, y_center_to) = box_center to_box
    in
      if x_center_from = x_center_to then
        (* vertical line: go right first *)
        let
          val (x0, y0) = (right_from, y_center_from)
          val (x3, y3) = (right_to, y_center_to)

          val (x1, y1) = (x0 + displ, y0)
          val (x2, y2) = (x3 + displ, y3)
        in
          ((x0, y0), (x1, y1), (x2, y2), (x3, y3))
        end
      else
        (* non-vertical line: go down first *)
        let
          val (x0, y0) = (x_center_from, bot_from)
          val (x3, y3) = (x_center_to, bot_to)

          val (x1, y1) = (x0, y0 + displ)
          val (x2, y2) = (x3, y3 + displ)
        in
          ((x0, y0), (x1, y1), (x2, y2), (x3, y3))
        end
    end

  fun distance ((x1, y1) : Point) ((x2, y2) : Point) : real =
  let
    val delta_x = (Real.fromInt x2) - (Real.fromInt x1)
    val delta_y = (Real.fromInt y2) - (Real.fromInt y1)
  in
    Math.sqrt (delta_x * delta_x + delta_y * delta_y)
  end

  (* computes the arrowhead points of the edge going from one point
   * to another. *)
  fun arrowhead_points ((x_from, y_from) : Point) ((x_to, y_to) : Point)
    (size : int) : Point * Point =
    case (line_between (x_from, y_from) (x_to, y_to)) of
         Horizontal _ =>
           let
             val x = if x_from < x_to then x_to - size else x_to + size
           in
             ((x, y_to - size), (x, y_to + size))
           end
       | Vertical _ =>
           let
             val y = if y_from < y_to then y_to - size else y_to + size
           in
             ((x_to - size, y), (x_to + size, y))
           end
       | Oblique (slope, inv_slope, offset) =>
           let
             val edge_len = distance (x_from, y_from) (x_to, y_to)
             (* from point `from` to the center of base of arrowhead *)
             val base_dist = edge_len - (Real.fromInt size)
             val base_delta_x = base_dist / (Math.sqrt (1.0 + slope*slope))
             (* coordinates of center of base of arrowhead *)
             val base_x =
               if x_from < x_to then
                 (Real.fromInt x_from) + base_delta_x
               else
                 (Real.fromInt x_from) - base_delta_x
             val base_y = slope * base_x + offset
             (* slope and offset of perpendicular line that goes through
              * the center of base of arrowhead *)
             val perp_slope = ~inv_slope
             val perp_offset = base_y - perp_slope * base_x
             (* from center of base of arrowhead to each base point *)
             val delta_x =
               (Real.fromInt size) / Math.sqrt (1.0 + perp_slope * perp_slope)
             val x1 = base_x + delta_x
             val x2 = base_x - delta_x
             val y1 = perp_slope * x1 + perp_offset
             val y2 = perp_slope * x2 + perp_offset
           in
             ((round x1, round y1), (round x2, round y2))
           end
end
