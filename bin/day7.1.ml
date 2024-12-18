let () =
  let inp = open_in "inputs/input7.txt" in
  let result = ref 0 in
  let rec is_valid_eq result current operands =
    match operands with
    | [] -> result = current
    | h :: rest ->
      is_valid_eq result (current + h) rest
      || is_valid_eq result (current * h) rest
      || is_valid_eq result (int_of_string (string_of_int current ^ string_of_int h)) rest
  in
  try
    while true do
      let equation = input_line inp |> String.split_on_char ':' in
      let goal = int_of_string (List.nth equation 0) in
      let operands =
        String.split_on_char ' ' (List.nth equation 1)
        |> List.filter (fun s -> s <> "")
        |> List.map (fun s -> int_of_string s)
      in
      result
      := !result
         + if is_valid_eq goal (List.hd operands) (List.tl operands) then goal else 0
    done
  with
  | End_of_file -> Printf.printf "%d\n" !result
;;
