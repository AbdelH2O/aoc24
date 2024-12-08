let () =
  let inp = open_in "input5.txt" in
  let dependencies = Hashtbl.create 1000000 in
  let count = ref 0 in
  let add_dep x y =
    match Hashtbl.mem dependencies x with
    | false -> Hashtbl.add dependencies x [ y ]
    | true -> Hashtbl.add dependencies x (Hashtbl.find dependencies x @ [ y ])
  in
  try
    while true do
      let rule =
        input_line inp |> String.split_on_char '|' |> List.filter (fun s -> s <> "")
      in
      if List.length rule = 0
      then raise End_of_file
      else add_dep (List.nth rule 1) (List.nth rule 0)
    done
  with
  | End_of_file ->
    let rec all_deps_printed deps exists visited =
      match deps with
      | [] -> true
      | dep :: other_deps ->
        if Hashtbl.mem exists dep && not (Hashtbl.mem visited dep)
        then false
        else all_deps_printed other_deps exists visited
    in
    let get_dependencies deps page =
      match Hashtbl.mem deps page with
      | true -> Hashtbl.find deps page
      | false -> []
    in
    let rec check pages exists visited =
      match pages with
      | [] -> true
      | page :: other_pages ->
        Hashtbl.add visited page true;
        all_deps_printed (get_dependencies dependencies page) exists visited
        && check other_pages exists visited
    in
    let rec register pages map =
      match pages with
      | [] -> ()
      | page :: other_pages ->
        Hashtbl.add map page true;
        register other_pages map
    in
    (try
       while true do
         let pages = input_line inp |> String.split_on_char ',' in
         let exists = Hashtbl.create (List.length pages) in
         let visited = Hashtbl.create (List.length pages) in
         register pages exists;
         if not (check pages exists visited)
         then count := !count + int_of_string (List.nth pages (List.length pages / 2))
       done
     with
     | End_of_file ->
       close_in inp;
       Printf.printf "%d\n" !count)
;;
