open Computer
(* iterate over the list of computers
   get first process in the list of processes 
   if the process has id -1, then remove it
   else:
       ask z random computers to execute the process
           if they agree, then add it to the list of their current processes
           else, take it if we can
   update the usage of computer*)

module Lazy = struct
let handle_computers computers z p _ =
  let rec aux computers computers_to_handle z =
    match computers_to_handle with
      | [] -> ()
      | computer :: _ ->
          let process = List.hd computer.processes in
          computer.processes <- List.tl computer.processes;
          let computers_to_handle = List.filter(fun x -> x.computer_id <> computer.computer_id) computers_to_handle in
          match process.process_id with
          | "-1" -> ()
          | _ -> ask computers computer z process p;
          aux computers computers_to_handle z
  in
  aux computers computers z
end
