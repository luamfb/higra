exception InternalError

structure Geom = struct
  (* top-left x, top-left y, width, height *)
  type Point = int * int

  (* top-left x, top-left y, width, height *)
  type Box = int * int * int * int

  datatype Line = Vertical of int (* x coord *)
                     | Horizontal of int (* y coord *)
                     (* slope, inverse slope, offset *)
                     | Oblique of int * int * int

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

  fun next_free_topleft (sep : int) (box_list : Box list) : Point =
  let
    (* TODO take horizontal into account *)
    val (max_x, _) = box_list_max_dim box_list
    val x = if max_x > 0 then max_x + sep else 0
    val y = 0
  in
    (x, y)
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

  fun box_intersects ((left1,top1,w1,h1) : Box) ((left2,top2,w2,h2) : Box) : bool =
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
    (box_list : Box list) : Box =
    let
      val (x_r, y_r) = point_right_to sep near
      val (x_d, y_d) = point_down_to sep near
      val box_r = (x_r, y_r, width, height)
      val box_d = (x_d, y_d, width, height)
    in
      (* TODO reverse if horizontal is false *)
      if (box_is_free box_r box_list) then
        box_r
      else
        if (box_is_free box_d box_list) then
          box_d
        else
          (* FIXME should try diagonals as well *)
          let
            val (next_x, next_y) = next_free_topleft sep box_list
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
        val slope = (y1 - y2) div (x1 - x2)
        val inv_slope = (x1 - x2) div (y1 - y2)
        val offset = y1 - slope * x1
      in
        Oblique (slope, inv_slope, offset)
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
               val line_x_at = fn y => inv_slope * (y - offset)
               val line_y_at = fn x => slope * x + offset
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

  (* gets the coordinates of a line connecting the boxes *)
  fun box_connector_line (box1 : Box) (box2 : Box) (edge_displ : int)
    : Point * Point =
  let
    val (left1, top1, right1, bot1) = box_coords box1
    val (left2, top2, right2, bot2) = box_coords box2
    val center1 = box_center box1
    val center2 = box_center box2
    val (x_center1, y_center1) = center1
    val (x_center2, y_center2) = center2
    val line = line_between center1 center2
  in
      (* TODO take vertex shape into account *)
      (* FIXME should check if there's a vertex in the way *)
      (line_box_intersection box1 line center2,
      line_box_intersection box2 line center1)
  end

end
