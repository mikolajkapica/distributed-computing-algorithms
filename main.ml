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
let create_computers n =
  let create_processes n =
    let rec aux acc = function
      | 0 -> acc
      | n -> match Random.bool () with
          | false -> aux ({process_id = "-1"; power = 0; time = 0} :: acc) (n - 1)
          | true ->
              aux
                ({
                   process_id = string_of_int n;
                   power = Random.int 100;
                   time = Random.int 10;
                 }
                :: acc)
                (n - 1)
    in
    aux [] n
  in
  let rec aux acc = function
    | 0 -> acc
    | n ->
          aux
          ({
             computer_id = string_of_int n;
             usage = 0;
             current_processes = [];
             processes = create_processes (Random.int 100);
            }
          :: acc)
          (n - 1)
    in
    aux [] n

(* iterate over the list of computers and print the state of each one and the list of processes *)
let print_computers computers =
  let print_process processes =
    Printf.printf "{Process: %s: power: %i}, " processes.process_id processes.power
  in
  List.iter
    (fun computer ->
      Printf.printf "Computer: %s: usage: %i " computer.computer_id computer.usage;
      List.iter (fun process -> print_process process) computer.processes;
      Printf.printf "\n\n")
    computers

(* iterate over the list of computers
   get first process in the list of processes 
   if the process has id -1, then remove it
   else:
       ask z random computers to execute the process
           if they agree, then add it to the list of their current processes
           else, take it if we can
   update the usage of computer*)

    let accepting_computer = let rec ask not_asked_computers count process =
      match count with
      | 0 -> []
      | z ->
        let random_computer = List.nth not_asked_computers (Random.int (List.length not_asked_computers)) in
        if random_computer.usage + process.power <= 100 then 
            random_computer.current_processes <- random_computer.current_processes @ [process];
            random_computer.usage <- random_computer.usage + process.power;
            random_computer :: ask not_asked_computers z;

    let rec handle_computers computers z =
        match computers with
        | [] -> ()
        | computer :: rest ->
            (* handle new process *)
            let process = List.hd computer.processes in
            let get_accepting_computer = match process.process_id with
            | "-1" -> 
                    computer.processes <- List.tl computer.processes; 
                    None
            | _ -> ask computers z 

let simulate_lazy p r z n = 
    let computers = create_computers n in
    let rec time_tick time_left computers =
        match time_left with
        | 0 -> ()
        | _ -> handle_computers computers

let () =
    let p = 0.5 in
    let r = 0.2 in
    let z = 3 in
    let n = 10 in
    simulate_lazy p r z n 

