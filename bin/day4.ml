let () =
  let inp = open_in "input4.txt" in
  let grid = Dynarray.make 0 (Array.make 0 '.') in
  let sequance = "XMAS" |> String.to_seq |> Array.of_seq in
  let directions = [ 1, 0; 1, 1; 0, 1; -1, 1; -1, 0; -1, -1; 0, -1; 1, -1 ] in
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
      | 4 -> 1
      | ind ->
        if Array.get sequance ind = index x y
        then check (x + fst direction) (y + snd direction) direction (ind + 1)
        else 0
    in
    let rec check_all x y dirs =
      match dirs with
      | [] -> 0
      | direction :: rest -> check x y direction 0 + check_all x y rest
    in
    let rec explore i j =
      match i, j with
      | x, _ when x = height -> 0
      | x, y when y = length -> explore (x + 1) 0
      | x, y -> check_all x y directions + explore i (j + 1)
    in
    Printf.printf "%d\n" (explore 0 0)
;;
