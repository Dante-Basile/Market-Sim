#1 "market_sim.eml.ml"
(* In OCaml, `begin ... end` is the same as `( ... )` *)
[@@@ocaml.warning "-27"] (* unused variable *)
[@@@ocaml.warning "-33"] (* unused package *)

open Market_sim_lib;;
open Core;;

let render_stock = 
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<body>\n% print_string \"hi\"\n  <h1>Add stock: </h1>\n  <input type=\"text\">\n  <button>Submit</button>\n</body>\n</html>\n\n");
(Buffer.contents ___eml_buffer)
#18 "market_sim.eml.ml"
let render_home tasks =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<body>\n  <h1>Stock Market Simulator</h1>\n  <button>Add stock</button>\n</body>\n</html> \n\n\n");
(Buffer.contents ___eml_buffer)
#27 "market_sim.eml.ml"
let tasks = [
  ("write documentation", true);
  ("create examples", true);
  ("publish website", true);
  ("profit", false);
] 

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/"
      (fun _ ->
        render_home tasks
        |> Dream.html);
        


  ]
  @@ Dream.not_found
