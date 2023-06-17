type process = { 
    process_id : string; 
    power : int; 
    time : int }

type computer = {
  computer_id : string;
  mutable usage : int;
  mutable current_processes : process list;
  mutable processes : process list;
}

(* create n computers *)
let create_computers n p =
  let rec create_processes p =
    match p with
    | 0 -> []
    | p ->
        match Random.bool () with
        | false -> {process_id = "-1"; power = 0; time = 0} :: create_processes (p - 1)
        | true ->
            {
              process_id = string_of_int p;
              power = Random.int 100;
              time = Random.int 10;
            }
            :: create_processes (p - 1)
  in
  let create_a_computer id processes =
    {
      computer_id = string_of_int id;
      usage = 0;
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


(* iterate over the list of computers and print the state of each one and the list of processes *)
(* let print_computers computers = *)
(*   let print_process processes = *)
(*     Printf.printf "{Process: %s: power: %i}, " processes.process_id processes.power *)
(*   in *)
(*   List.iter *)
(*     (fun computer -> *)
(*       Printf.printf "Computer: %s: usage: %i " computer.computer_id computer.usage; *)
(*       List.iter (fun process -> print_process process) computer.processes; *)
(*       Printf.printf "\n\n") *)
(*     computers *)

(* iterate over the list of computers
   get first process in the list of processes 
   if the process has id -1, then remove it
   else:
       ask z random computers to execute the process
           if they agree, then add it to the list of their current processes
           else, take it if we can
   update the usage of computer*)

let rec ask not_asked_computers computer count process =
  match count with
  | 0 -> (
      match not_asked_computers with
      | [] -> ()
      | _ -> 
        computer.current_processes <- computer.current_processes @ [process];
        computer.usage <- computer.usage + process.power;
        ask [] computer 0 process;
      )
  | _ ->
    let random_computer = List.nth not_asked_computers (Random.int (List.length not_asked_computers)) in
    let not_asked_computers = List.filter (fun x -> x.computer_id <> random_computer.computer_id) not_asked_computers in
    if random_computer.usage + process.power <= 100 then ( 
        random_computer.current_processes <- random_computer.current_processes @ [process];
        random_computer.usage <- random_computer.usage + process.power;
        ask not_asked_computers computer 0 process;)
    else ask not_asked_computers computer (count - 1) process

let rec handle_computers computers z =
  match computers with
    | [] -> ()
    | computer :: rest ->
        let process = List.hd computer.processes in
        computer.processes <- List.tl computer.processes;
        match process.process_id with
        | "-1" -> handle_computers rest z 
        | _ -> ask computers computer z process 

let simulate_lazy p r z n t =
    print_endline "Simulation started";
    let computers = create_computers n t in
    let rec time_tick time_left computers =
        match time_left with
        | 0 -> ()
        | _ -> 
            Printf.printf("Time left: %i\n") time_left;
            handle_computers computers z;
            time_tick (time_left - 1) computers
    in
    time_tick t computers

let () =
    let p = 0.5 in
    let r = 0.2 in
    let z = 3 in
    let n = 10 in
    let t = 100 in
    simulate_lazy p r z n t 
