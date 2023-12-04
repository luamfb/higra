structure Geom = struct
  (* top-left x, top-left y, width, height *)
  type Point = int * int

  (* top-left x, top-left y, width, height *)
  type Box = int * int * int * int

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

end
