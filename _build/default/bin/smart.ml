open Computer

module Smart = struct
  let handle_computers computers z p _ migration_stats =
    let rec aux computers computers_to_handle =
      match computers_to_handle with
      | [] -> ()
      | computer :: _ -> (
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
              if computer.usage + process.power <= 100 then
                add_process_to_computer computer process
              else ask computers computer z process p migration_stats;
              aux computers computers_to_handle)
    in
    aux computers computers
end
