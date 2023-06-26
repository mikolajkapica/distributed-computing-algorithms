type process = {
  process_id : string;
  power : int;
  mutable time : int
}

type computer = {
  computer_id : string;
  mutable usage : int;
  mutable usages : int list;
  mutable current_processes : process list;
  mutable processes : process list;
}

type migration_stats = {
  mutable migration_counter : int;
  mutable migration_ask_counter : int;
}

(** create n computers with p processes (either process or null(id=-1)*)
let create_computers n_computers p_processes =
  (* Random.self_init (); *)
  let rec create_processes p_processes =
    match p_processes with
    | 0 -> []
    | p -> (
        match Random.float 1.0 > 0.1 with
        | false ->
            { process_id = "-1"; power = 0; time = 0 }
            :: create_processes (p - 1)
        | true ->
            {
              process_id = string_of_int p;
              power = Random.int 50;
              time = Random.int 20;
            }
            :: create_processes (p - 1))
  in
  let create_a_computer id processes =
    {
      computer_id = string_of_int id;
      usage = 0;
      usages = [];
      current_processes = [];
      processes;
    }
  in
  let computers : computer list = [] in
  let rec create_computers n_computers =
    match n_computers with
    | 0 -> computers
    | n ->
        let processes = create_processes p_processes in
        create_a_computer n processes :: create_computers (n - 1)
  in
  create_computers n_computers

(** add process to computer *)
let add_process_to_computer computer process =
  computer.current_processes <- computer.current_processes @ [ process ];
  computer.usage <- computer.usage + process.power

(** ask for migration *)
let rec ask not_asked_computers computer z process p migration_stats =
  migration_stats.migration_ask_counter <-
    migration_stats.migration_ask_counter + 1;
  match z with
  | 0 ->
      add_process_to_computer computer process;
      ()
  | _ ->
      let random_idx = Random.int (List.length not_asked_computers) in
      let random_computer = List.nth not_asked_computers random_idx in
      let not_asked_computers =
        List.filter
          (fun x -> x.computer_id <> random_computer.computer_id)
          not_asked_computers
      in
      if random_computer.usage + process.power <= p then (
        migration_stats.migration_counter <-
          migration_stats.migration_counter + 1;
        add_process_to_computer random_computer process;
        ())
      else ask not_asked_computers computer (z - 1) process p migration_stats

(** simulate time passage *)
let time_passage computers =
  List.iter
    (fun x ->
      x.current_processes <-
        List.filter (fun y -> y.time > 0) x.current_processes;
      x.usage <-
        List.fold_left (fun acc x -> acc + x.power) 0 x.current_processes;
      x.usages <- x.usage :: x.usages;
      List.iter (fun y -> y.time <- y.time - 1) x.current_processes)
    computers

(** display stats *)
let stats computers migration_stats =
  let usages = List.map (fun x -> x.usage) computers in
  let average =
    List.fold_left (fun acc x -> acc + x) 0 usages / List.length usages
  in
  let variance =
    List.fold_left (fun acc x -> acc + ((x - average) * (x - average))) 0 usages
    / List.length usages
  in
  let std_dev = sqrt (float_of_int variance) in
  Printf.printf "Average usage: %i\n" average;
  Printf.printf "Standard deviation: %f\n" std_dev;
  Printf.printf "Max usage: %i\n" (List.fold_left (fun acc x -> if acc < x then x else acc) 0 usages);
  Printf.printf "Migration counter: %i\n" migration_stats.migration_counter;
  Printf.printf "Migration ask counter: %i\n\n" migration_stats.migration_ask_counter;
  ()

(** simulation function *)
(* p -> what computer can handle 
 * r -> for altruist to take from others if he has less then r
 * z -> number of computers to ask for migration
 * n_computers -> number of computers
 * time -> time of simulation 
 * handle_computers -> function to handle computers (lazy/smart/altruist)
*)
let simulate p r z n_computers time handle_computers =
  let computers = create_computers n_computers time in
  let migration_stats = { migration_counter = 0; migration_ask_counter = 0 } in
  let rec time_tick time_left computers =
    match time_left with
    | 0 ->
        Printf.printf "Simulation ended\n";
        stats computers migration_stats;
        ()
    | _ ->
        time_passage computers;
        handle_computers computers z p r migration_stats;
        time_tick (time_left - 1) computers
  in
  time_tick time computers
