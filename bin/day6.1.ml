let () =
  let inp = open_in "inputs/test.txt" in
  let grid = Dynarray.make 0 (Array.make 0 '.') in
  let directions = [ 1, 0; 0, -1; -1, 0; 0, 1 ] in
  let start = ref None in
  let rec get_position line y x =
    match line with
    | [] -> None
    | element :: rest ->
      if element = '.' || element = '#'
      then get_position rest (y + 1) x
      else Some (element, (x, y))
  in
  try
    while true do
      let grid_line = input_line inp |> String.to_seq |> Array.of_seq in
      if Option.is_none !start
      then start := get_position (Array.to_list grid_line) 0 (Dynarray.length grid);
      Dynarray.add_last grid grid_line
    done
  with
  | End_of_file ->
    close_in inp;
    assert (Option.is_some !start);
    let width = Array.length (Dynarray.get grid 0) in
    let height = Dynarray.length grid in
    let visited = Hashtbl.create (height * width) in
    let index x y =
      try Array.get (Dynarray.get grid x) y with
      | Invalid_argument _ -> '.'
    in
    let is_not_inbound pos =
      match pos with
      | _, y when y >= width || y < 0 -> true
      | x, _ when x >= height || x < 0 -> true
      | _ -> false
    in
    let rec can_rejoin direction current =
      if Option.is_some (Hashtbl.find_opt visited current) && (Hashtbl.find visited current) = direction
      then true
      else (
        match index (fst current) (snd current) with
        | '#' -> false
        | _ ->
          let next = fst current + fst direction, snd current + snd direction in
          if is_not_inbound next
          then false
          else
            can_rejoin direction (fst current + fst direction, snd current + snd direction))
    in
    let rec explore coords direction_ind count =
      let i, j = coords in
      let direction = List.nth directions direction_ind in
      let new_direction = List.nth directions ((direction_ind + 1) mod 4) in
      let new_count =
        if can_rejoin new_direction (i, j)
        then (
          count + 1)
        else count
      in
      match i + fst direction, j + snd direction with
      | x, y when x = height || x = -1 || y = width || y = -1 -> new_count
      | x, y when index x y = '#' -> explore (i, j) ((direction_ind + 1) mod 4) new_count
      | x, y ->
        Hashtbl.replace visited (i, j) direction;
        explore (x, y) direction_ind new_count
    in
    let get_direction guard =
      match guard with
      | 'v' -> 0
      | '>' -> 1
      | '^' -> 2
      | '<' -> 3
      | _ -> 0
    in
    Printf.printf
      "%d\n"
      (explore (snd (Option.get !start)) (get_direction (fst (Option.get !start))) 0)
;;
