open Core

let range (lo : int) (hi : int) : (int list) =
  let ret = ref [] in
  for i = lo to hi do
    ret := i :: !ret;
  done;
  !ret

module Point = struct
  type t =
    {
      x : int;
      y : int;
    }

  let make (x : int) (y : int) : t =
    {
      x = x;
      y = y;
    }

  let add (p1 : t) (p2 : t) : t =
    {
      x = p1.x + p2.x;
      y = p1.y + p2.y;
    }

  let leq (p1 : t) (p2 : t) : bool =
    p1.x <= p2.x && p1.y <= p2.y

end

module Point_map = Set.M(Point)

module Claim = struct
  type t =
    {
      id : int;
      corner: Point.t;
      w : int;
      h : int;
    }

  let make id corner w h =
    {
      id = id;
      corner = corner;
      w = w;
      h = h;
    }

  let top_left (c : t) : (Point.t) =
    c.corner

  let bottom_right (c : t) : (Point.t) =
    Point.add c.corner {x = c.w; y = c.h}

  let for_all_points (c : t) fn =
    let tl = top_left c in
    let br = bottom_right c in
    let r1 = range tl.x br.x in
    let r2 = range tl.y br.y in

    List.concat
      (
        List.map r1 ~f: (fun x ->
            List.map r2 ~f: (fun y ->
                fn (Point.make x y)
              ))
      )

  let contains_point (c : t) (p : Point.t) : bool =
    c.corner <= p && 
    c.corner.x <= p.x && p.x <= c.corner.x + c.w &&
    c.corner.y <= p.y && p.y <= c.corner.y + c.h

end


let parse_line (line : string) : Claim.t =
  Scanf.sscanf line "#%d @ %d,%d: %dx%d"
    (fun id x y w h ->
       Claim.make
         id
         (Point.make x y)
         w
         h
    )

let () =
  let raw_lines = In_channel.input_lines In_channel.stdin in
  let claims = List.map ~f: parse_line  raw_lines in
  let claims_contain_point p =
    List.exists claims ~f: (fun c ->
        Claim.contains_point c p
      )
  in
  let points = List.map claims ~f: (fun c ->
      Claim.for_all_points c (fun p ->
          claims_contain_point p
        )
    )
  in

  ()
