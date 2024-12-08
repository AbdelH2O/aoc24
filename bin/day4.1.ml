let () =
  let inp = open_in "inputs/input4.txt" in
  let grid = Dynarray.make 0 (Array.make 0 '.') in
  let sequance = "MAS" |> String.to_seq |> Array.of_seq in
  let directions = [ (1, 1), (1, -1); (1, 1), (-1, 1) ] in
  let positions = [ 2, 0; 0, 2 ] in
  try
    while true do
      let grid_line = input_line inp |> String.to_seq |> Array.of_seq in
      Dynarray.add_last grid grid_line
    done
  with
  | End_of_file ->
    close_in inp;
    let length = Array.length (Dynarray.get grid 0) in
    let height = Dynarray.length grid in
    let index x y =
      try Array.get (Dynarray.get grid x) y with
      | Invalid_argument _ -> '.'
    in
    let rec check x y direction ind =
      match ind with
      | 3 -> true
      | ind ->
        if Array.get sequance ind = index x y
        then check (x + fst direction) (y + snd direction) direction (ind + 1)
        else false
    in
    let matching_dir direction pos =
      match pos with
      | 2, 0 -> fst direction * -1, snd direction * 1
      | 0, 2 -> fst direction * 1, snd direction * -1
      (* Shouldn't happen *)
      | _ -> direction
    in
    let check_x x y direction pos =
      if check x y direction 0
         && check (x + fst pos) (y + snd pos) (matching_dir direction pos) 0
      then 1
      else 0
    in
    let rec check_all x y dirs poss =
      match poss, dirs with
      | [], [] -> 0
      | pos :: other_poss, dir :: other_dirs ->
        check_x x y (fst dir) pos
        + check_x x y (snd dir) pos
        + check_all x y other_dirs other_poss
      | _, _ -> 0
    in
    let rec explore i j =
      match i, j with
      | x, _ when x = height -> 0
      | x, y when y = length -> explore (x + 1) 0
      | x, y -> check_all x y directions positions + explore i (j + 1)
    in
    Printf.printf "%d\n" (explore 0 0)
;;
