open Computer
include Lazy
include Smart
include Altruist

let args = Array.to_list Sys.argv 
let () = 
  let p = 50 in
  let r = 20 in
  let z = 3 in
  let n = 100 in
  let t = 100 in
  let simulated = ref false in
  if List.find_opt (fun x -> x = "lazy") args <> None then (
    print_endline "Lazy:";
    simulated := true;
    simulate p r z n t Lazy.handle_computers
  );
  if List.find_opt (fun x -> x = "smart") args <> None then (
    print_endline "Smart:";
    simulated := true;
    simulate p r z n t Smart.handle_computers
  );
  if List.find_opt (fun x -> x = "altruist") args <> None then (
    print_endline "Altruist:";
    simulated := true;
    simulate p r z n t Altruist.handle_computers
  );
  if not !simulated then print_endline "Valid args: lazy, smart, altruist"
