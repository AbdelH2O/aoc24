let append_item lst a = lst @ [a]


let rec diff a b =
  match a, b with
  | [], [] -> 0
  | x :: a, y :: b -> abs (x - y) + diff a b
  | x :: _, [] -> x
  | [], y :: _ -> y

let () = 
  let a = ref [] in
  let b = ref [] in
  let inp = open_in "input1.txt" in

  try
    while true do
      let a_and_b = input_line inp |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") in
      a := append_item !a (int_of_string (List.nth a_and_b 0));
      b := append_item !b (int_of_string (List.nth a_and_b 1));

    done;
  with End_of_file ->

  close_in inp;

  a := List.sort compare !a;
  b := List.sort compare !b;

  Printf.printf "%d\n" (diff !a !b)

