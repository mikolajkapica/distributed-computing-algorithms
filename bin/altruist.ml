open Computer

module Altruist = struct
  let handle_computers computers _ p r migration_stats =
    let rec aux computers computers_to_handle =
      match computers_to_handle with
      | [] -> ()
      | computer :: _ -> (
          if List.length computer.processes = 0 then
            let computers_to_handle =
              List.filter
                (fun x -> x.computer_id <> computer.computer_id)
                computers_to_handle
            in
            aux computers computers_to_handle
          else
            let process = List.hd computer.processes in
            computer.processes <- List.tl computer.processes;
            let computers_to_handle =
              List.filter
                (fun x -> x.computer_id <> computer.computer_id)
                computers_to_handle
            in
            match process.process_id with
            | "-1" -> ()
            | _ ->
                let new_usage = computer.usage + process.power in
                if new_usage < p then (
                  add_process_to_computer computer process;
                  if new_usage < r then List.iter
                      (fun x -> if x.usage > p then
                          List.iter (fun y -> if x.usage - y.power < p then (
                                computer.current_processes <- computer.current_processes @ [ y ];
                                x.current_processes <- List.filter (fun z -> z.process_id <> y.process_id) x.current_processes;
                                x.usage <- x.usage - y.power;
                                computer.usage <- computer.usage + y.power;
                                aux computers computers_to_handle))
                            x.current_processes)
                      computers)
                else ask computers computer (List.length computers) process p migration_stats;
                aux computers computers_to_handle)
    in
    aux computers computers
end
