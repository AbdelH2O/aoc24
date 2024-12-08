let () =
  let inp = open_in "inputs/input5.txt" in
  let dependencies = Hashtbl.create 1000000 in
  let count = ref 0 in
  let add_dep map x y =
    match Hashtbl.mem map x with
    | false -> Hashtbl.add map x [ y ]
    | true -> Hashtbl.add map x (Hashtbl.find map x @ [ y ])
  in
  try
    while true do
      let rule =
        input_line inp |> String.split_on_char '|' |> List.filter (fun s -> s <> "")
      in
      if List.length rule = 0
      then raise End_of_file
      else add_dep dependencies (List.nth rule 1) (List.nth rule 0)
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
    let valid_order pages exists =
      let in_degree = Hashtbl.create (List.length pages) in
      let dependents = Hashtbl.create (List.length pages) in
      List.iter
        (fun page ->
          let neighbors = get_dependencies dependencies page in
          List.iter
            (fun neighbor ->
              if Hashtbl.mem exists neighbor
              then (
                add_dep dependents neighbor page;
                Hashtbl.replace
                  in_degree
                  page
                  ((Hashtbl.find_opt in_degree page |> Option.value ~default:0) + 1);
                if not (Hashtbl.mem in_degree neighbor)
                then Hashtbl.add in_degree neighbor 0))
            neighbors)
        pages;
      let queue = Queue.create () in
      Hashtbl.iter (fun node deg -> if deg = 0 then Queue.add node queue) in_degree;
      let result = ref [] in
      while not (Queue.is_empty queue) do
        let page = Queue.take queue in
        result := page :: !result;
        List.iter
          (fun neighbor ->
            if Hashtbl.mem exists neighbor
            then (
              let new_degree = Hashtbl.find in_degree neighbor - 1 in
              Hashtbl.replace in_degree neighbor new_degree;
              if new_degree = 0 then Queue.add neighbor queue))
          (get_dependencies dependents page)
      done;
      !result
    in
    (try
       while true do
         let pages = input_line inp |> String.split_on_char ',' in
         let exists = Hashtbl.create (List.length pages) in
         let visited = Hashtbl.create (List.length pages) in
         register pages exists;
         if not (check pages exists visited)
         then
           count
           := !count
              + int_of_string
                  (List.nth (valid_order pages exists) (List.length pages / 2))
       done
     with
     | End_of_file ->
       close_in inp;
       Printf.printf "%d\n" !count)
;;
