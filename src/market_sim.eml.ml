(* In OCaml, `begin ... end` is the same as `( ... )` *)
[@@@ocaml.warning "-27"] (* unused variable *)
[@@@ocaml.warning "-33"] (* unused package *)

open Market_sim_lib;;
open Core;;

let render_stock = 
  <html>
  <body>
  % print_string "hi"
    <h1>Add stock: </h1>
    <input type="text">
    <button>Submit</button>
  </body>
  </html>

let render_home tasks =
  <html>
  <body>
    <h1>Stock Market Simulator</h1>
    <button>Add stock</button>
  </body>
  </html> 


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
