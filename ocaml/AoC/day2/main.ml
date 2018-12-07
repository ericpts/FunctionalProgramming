
open Core

let are_close (w1: string) (w2: string) : bool =
  let w1 = String.to_list w1
  and w2 = String.to_list w2
      in

  let eq_vec : (bool list List.Or_unequal_lengths.t) = List.map2 w1 w2 ~f: (fun c1 c2 -> c1 = c2) in
      match eq_vec with
      | Unequal_lengths -> false
      | Ok eq_vec -> 
  let eq_pos = List.fold eq_vec ~init: 0 ~f: (fun acc now -> acc + if now then 1 else 0) in
  eq_pos = (List.length w1) - 1

let () =
  let raw_lines = In_channel.input_lines In_channel.stdin in
  let try_find_match word =
    List.filter_map ~f: (fun other ->
        if are_close word other
        then Some (word, other)
        else None
      )
      raw_lines
  in
  let maybe_pairs = List.concat_map ~f: try_find_match raw_lines in
  List.iter maybe_pairs ~f: (fun (w1, w2) -> printf "%s %s\n" w1 w2);
  ()

