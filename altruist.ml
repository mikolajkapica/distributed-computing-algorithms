type process = { 
    process_id : string; 
    power : int; 
    mutable time : int }

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
        match Random.float 1.0 > 0.3 with
        | false -> {process_id = "-1"; power = 0; time = 0} :: create_processes (p - 1)
        | true ->
            {
              process_id = string_of_int p;
              power = Random.int 30;
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
  in
  create n

let print_computers computers =
  let rec print_computer computer =
    match computer with
    | [] -> ()
    | computer :: rest ->
        Printf.printf "Computer id: %s\n" computer.computer_id;
        Printf.printf "Computer usage: %i\n" computer.usage;
        Printf.printf "Computer current processes: ";
        List.iter (fun x -> Printf.printf "%s " x.process_id) computer.current_processes;
        Printf.printf "\n";
        Printf.printf "Computer processes: ";
        List.iter (fun x -> Printf.printf "%s " x.process_id) computer.processes;
        Printf.printf "\n";
        print_computer rest
  in
  print_computer computers

(* iterate over the list of computers
   get first process in the list of processes 
   if the process has id -1, then remove it
   else:
       ask z random computers to execute the process
           if they agree, then add it to the list of their current processes
           else, take it if we can
   update the usage of computer*)

let add_process_to_computer computer process =
  computer.current_processes <- computer.current_processes @ [process];
  computer.usage <- computer.usage + process.power

let rec ask not_asked_computers computer process p =
  match List.length not_asked_computers with
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
      ask not_asked_computers computer process p

let handle_computers computers p r =
  let rec aux computers computers_to_handle =
    match computers_to_handle with
      | [] -> ()
      | computer :: rest ->
          let process = List.hd computer.processes in
          computer.processes <- List.tl computer.processes;
          let computers_to_handle = List.filter(fun x -> x.computer_id <> computer.computer_id) computers_to_handle in
          match process.process_id with
          | "-1" -> ()
          | _ -> 
              let new_usage = computer.usage + process.power in
              if new_usage < p then (
                add_process_to_computer computer process; 
                if new_usage < r then (
                  List.iter (fun x -> if x.usage > p then (
                    List.iter (fun y -> if x.usage - y.power < p then (
                          computer.current_processes <- computer.current_processes @ [y];
                          aux computers computers_to_handle
                      )
                    ) x.current_processes
                  )) computers
                )
              )
              else ask computers computer process p;
          aux computers computers_to_handle 
  in
  aux computers computers 

let time_passage computers =
  List.iter (fun x -> 
    x.current_processes <- List.filter (fun y -> y.time > 0) x.current_processes;
    x.usage <- List.fold_left (fun acc x -> acc + x.power) 0 x.current_processes;
    List.iter (fun y -> y.time <- y.time - 1) x.current_processes
  ) computers

let computer_usages computers =
  List.iter (fun x -> Printf.printf "{Computer: %s: %i} " x.computer_id x.usage) computers;
  Printf.printf "\n"

let simulate_lazy p r n t =
    let computers = create_computers n t in
    let rec time_tick time_left computers =
        match time_left with
        | 0 -> () 
        | _ -> 
            computer_usages computers;
            time_passage computers;
            handle_computers computers p r;
            time_tick (time_left - 1) computers
    in 
    time_tick t computers

let () =
    let p = 50 in
    let r = 30 in
    let n = 10 in
    let t = 1000 in
    simulate_lazy p r n t;
