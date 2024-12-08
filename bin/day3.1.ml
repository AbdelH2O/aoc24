open Str

let () =
  let count = ref 0 in
  let inp = open_in "input3.txt" in
  let entire_pattern = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let does = Str.regexp "do\\(n't\\)?()" in
  let dont = "don't()" in
  let last = ref "do()" in
  let process num1 num2 = int_of_string num1 * int_of_string num2 in
  let get_last_action text pos =
    try
      let _ = Str.search_backward does text pos in
      Str.matched_string text
    with
    | Not_found -> !last
  in
  let rec find_matches text pos count =
    try
      let ind = Str.search_forward entire_pattern text pos in
      let num1 = Str.matched_group 1 text in
      let num2 = Str.matched_group 2 text in
      let ending = Str.match_end () in
      if compare dont (get_last_action text ind) == 0
      then find_matches text ending count
      else find_matches text ending (count + process num1 num2)
    with
    | Not_found -> count
  in
  try
    while true do
      let line = input_line inp in
      count := !count + find_matches line 0 0;
      last := get_last_action line (String.length line)
    done
  with
  | End_of_file ->
    ();
    close_in inp;
    Printf.printf "%d\n" !count
;;
