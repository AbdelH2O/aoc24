let append_item lst a = lst @ [a]

let incr_hash my_hash key =
  match Hashtbl.mem my_hash key with
  | false -> Hashtbl.add my_hash key 1
  | true -> Hashtbl.replace my_hash key ((Hashtbl.find my_hash key) + 1)

let rec count my_hash a =
  match a with
  | [] -> 0
  | x :: rest -> (try Hashtbl.find my_hash x with Not_found -> 0) * x + (count my_hash rest)

let () = 
  let a = ref [] in
  let b = ref [] in

  let my_hash = Hashtbl.create 10000 in

  let inp = open_in "input1.txt" in

  try
    while true do
      let a_and_b = input_line inp |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") in
      a := append_item !a (int_of_string (List.nth a_and_b 0));
      b := append_item !b (int_of_string (List.nth a_and_b 1));
    incr_hash my_hash (int_of_string (List.nth a_and_b 1));
    done;
  with End_of_file ->

  close_in inp;

  Printf.printf "%d\n" (count my_hash !a)
  

