let append_item lst a = lst @ [a]

let rec diff elements last first =
  if first then
    match elements with
    | [] -> []
    | _ :: rest -> diff rest last false
  else
    match elements with
    | [] -> []
    | x :: rest -> [(last - x)] @ diff rest x false

let rec check polarity first elements = 
  match elements with
  | [] -> 1
  | x :: rest -> (
    if abs(x) < 1 || abs(x) > 3 || ((x > 0) != polarity) then
      (if first then
        check polarity false rest
      else
        0)
    else
      check polarity first rest
  )

let () = 
  let safe = ref 0 in
  let inp = open_in "test.txt" in

  try
    while true do
      let elements = input_line inp |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") |> List.map (fun s -> int_of_string s) in
      safe := !safe + (diff elements (List.nth elements 0) true |> (check (List.nth elements 0 > List.nth elements 1)) true);
    done;
  with End_of_file ->

  close_in inp;

  Printf.printf "%d\n" !safe

