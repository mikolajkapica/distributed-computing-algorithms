type process = {
  process_id : string;
    power : int;
    mutable time : int;
}

type computer = {
  computer_id : string;
    mutable usage : int;
    mutable usages: int list;
    mutable current_processes : process list;
    mutable processes : process list;
}

(* create n computers with p processes (either process or null(id=-1)*)
let create_computers n p =
  Random.self_init ();
  let rec create_processes p =
    match p with
    | 0 -> []
    | p ->
        match Random.float 1.0 > 0.1 with
        | false -> {process_id = "-1"; power = 0; time = 0} :: create_processes (p - 1)
        | true ->
            {
              process_id = string_of_int p;
              power = Random.int 60;
              time = Random.int 20;
            }
            :: create_processes (p - 1)
  in
  let create_a_computer id processes =
    {
      computer_id = string_of_int id;
      usage = 0;
      usages = [];
      current_processes = [];
      processes = processes
    }
  in
  let computers: computer list = [] in
  let rec create n =
    match n with
    | 0 -> computers 
    | n -> 
      let processes = create_processes p in
      create_a_computer n processes :: create (n - 1) 
  in
  create n

(* let print_computers computers = *)
(*   let rec print_computer computer = *)
(*     match computer with *)
(*     | [] -> () *)
(*     | computer :: rest -> *)
(*         Printf.printf "Computer id: %s\n" computer.computer_id; *)
(*         Printf.printf "Computer usage: %i\n" computer.usage; *)
(*         Printf.printf "Computer current processes: "; *)
(*         List.iter (fun x -> Printf.printf "%s " x.process_id) computer.current_processes; *)
(*         Printf.printf "\n"; *)
(*         Printf.printf "Computer processes: "; *)
(*         List.iter (fun x -> Printf.printf "%s " x.process_id) computer.processes; *)
(*         Printf.printf "\n"; *)
(*         print_computer rest *)
(*   in *)
(*   print_computer computers *)

let add_process_to_computer computer process =
  computer.current_processes <- computer.current_processes @ [process];
  computer.usage <- computer.usage + process.power

let rec ask not_asked_computers computer z process p =
  match z with
  | 0 -> (
    add_process_to_computer computer process;
    ()
  )
  | _ ->
    let random_idx = Random.int (List.length not_asked_computers) in 
    let random_computer = List.nth not_asked_computers random_idx in 
    let not_asked_computers = List.filter (fun x -> x.computer_id <> random_computer.computer_id) not_asked_computers in
    if random_computer.usage + process.power <= p then ( 
      add_process_to_computer random_computer process;
      ()
    )
    else 
      ask not_asked_computers computer (z-1) process p

let time_passage computers =
  List.iter (fun x -> 
    x.current_processes <- List.filter (fun y -> y.time > 0) x.current_processes;
    x.usage <- List.fold_left (fun acc x -> acc + x.power) 0 x.current_processes;
    x.usages <- x.usage :: x.usages;
    List.iter (fun y -> y.time <- y.time - 1) x.current_processes
  ) computers

let computer_usages computers =
  List.iter (fun x -> Printf.printf "{Computer: %s: %i} " x.computer_id x.usage) computers;
  Printf.printf "\n"

let stats computers = 
  (* get standard deviation *)
  let usages = List.map (fun x -> x.usage) computers in
  let average = List.fold_left (fun acc x -> acc + x) 0 usages / List.length usages in
  let variance = List.fold_left (fun acc x -> acc + (x - average) * (x - average)) 0 usages / List.length usages in
  let std_dev = sqrt (float_of_int variance) in
  Printf.printf "Average usage: %i\n" average;
  Printf.printf "Standard deviation: %f\n" std_dev;
  Printf.printf "Max usage: %i\n" (List.fold_left (fun acc x -> if x > acc then x else acc) 0 usages);
  Printf.printf "Min usage: %i\n" (List.fold_left (fun acc x -> if x < acc then x else acc) 100000000 usages);
  Printf.printf "\n"

  (* List.iter (fun x ->  *)
  (*   Printf.printf "Computer: %s\n" x.computer_id; *)
  (*   Printf.printf "Average usage: %i\n" (List.fold_left (fun acc x -> acc + x) 0 x.usages / List.length x.usages); *)
  (*   Printf.printf "Max usage: %i\n" (List.fold_left (fun acc x -> if x > acc then x else acc) 0 x.usages); *)
  (*   Printf.printf "Min usage: %i\n" (List.fold_left (fun acc x -> if x < acc then x else acc) 100000000 x.usages); *)
  (*   Printf.printf "\n" *)
  (* ) computers *)

let simulate p r z n t handle_computers =
    let computers = create_computers n t in
    let rec time_tick time_left computers =
        match time_left with
        | 0 -> (
            Printf.printf "Simulation ended\n";
            stats computers;
            ()
        ) 
        | _ -> 
            time_passage computers;
            handle_computers computers z p r;
            time_tick (time_left - 1) computers
    in 
    time_tick t computers

