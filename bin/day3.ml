open Str;;

let () = 
  let count = ref 0 in
  let inp = open_in "input3.txt" in
  let entire_pattern = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in

  let process num1 num2 = (int_of_string num1) * (int_of_string num2) in

  let rec find_matches text pos count =
    try
      let _ = Str.search_forward entire_pattern text pos in
      let num1 = Str.matched_group 1 text in
      let num2 = Str.matched_group 2 text in
      find_matches text (Str.match_end ()) (count + process num1 num2)
    with Not_found -> count
  in
  try
    while true do
      let line = input_line inp in
      count := !count + (find_matches line 0 0);
    done;
  with End_of_file ->

  close_in inp;


  Printf.printf "%d\n" !count;



