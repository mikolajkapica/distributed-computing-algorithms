type process = { 
    process_id : string; 
    power : float; 
    time : int }

type computer = {
  computer_id : string;
  mutable usage : float;
  mutable processes : process list;
}

(* create n computers *)
let create_computers n =
  let create_processes n =
    let rec aux acc = function
      | 0 -> acc
      | n ->
          aux
            ({
               process_id = string_of_int n;
               power = Random.float 100.;
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
          ({ computer_id = string_of_int n; usage = 0.; processes = create_processes 4 } :: acc)
          (n - 1)
  in
  aux [] n

(* iterate over the list of computers and print the state of each one and the list of processes *)
let print_computers computers =
  let print_process processes =
    Printf.printf "{Process: %s: power: %f}, " processes.process_id
      processes.power
  in
  List.iter
    (fun computer ->
      Printf.printf "Computer: %s: usage: %f " computer.computer_id
        computer.usage;
      List.iter (fun process -> print_process process) computer.processes;
      Printf.printf "\n")
    computers

(* create processes for a computer *)

let add_processes_to_computer computer processes =
  let add_process_to_computer computer process =
    computer.processes <- process :: computer.processes;
    computer.usage <- computer.usage +. process.power
  in
  List.iter (fun process -> add_process_to_computer computer process) processes

let () =
  let p = 0.5 in
  let z = 3 in
  let r = 0.2 in
  let n = 4 in
  let computers = create_computers n in
  print_computers computers
