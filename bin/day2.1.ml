let rec diff elements last first =
  if first
  then (
    match elements with
    | [] -> []
    | _ :: rest -> diff rest last false)
  else (
    match elements with
    | [] -> []
    | x :: rest -> [ last - x ] @ diff rest x false)
;;

let is_invalid num polarity = abs num < 1 || abs num > 3 || num > 0 != polarity

let rec check polarity elements =
  match elements with
  | [] -> 1
  | x :: rest -> if is_invalid x polarity then 0 else check polarity rest
;;

let rec get_polarity pos neg elements =
  match elements with
  | [] -> if pos > neg then true else false
  | x :: rest ->
    if x > 0
    then get_polarity (pos + 1) neg rest
    else if x < 0
    then get_polarity pos (neg + 1) rest
    else get_polarity pos neg rest
;;

let rec check_all polarity prev rest =
  match rest with
  | [] -> check polarity (diff prev (List.nth prev 0) true)
  | cur :: other ->
    if check polarity (diff (prev @ other) (List.nth (prev @ other) 0) true) == 1
    then 1
    else check_all polarity (prev @ [ cur ]) other
;;

let () =
  let safe = ref 0 in
  let inp = open_in "input2.txt" in
  try
    while true do
      let elements =
        input_line inp
        |> String.split_on_char ' '
        |> List.filter (fun s -> s <> "")
        |> List.map (fun s -> int_of_string s)
      in
      let diffs = diff elements (List.nth elements 0) true in
      let polarity = get_polarity 0 0 diffs in
      safe := !safe + check_all polarity [] elements
    done
  with
  | End_of_file ->
    close_in inp;
    Printf.printf "%d\n" !safe
;;
