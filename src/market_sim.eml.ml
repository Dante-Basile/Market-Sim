[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"] (* unused variable *)
[@@@ocaml.warning "-33"] (* unused package *)

open Market_sim_lib;;
open Core;;

let render_stock ?action request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Add Stock</h1>
  
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="action" autofocus>
    </form>
      
  </body>
  </html> 

let render_home ?action request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Stock Market Simulator</h1>
    <script src="./client.js" id="root"></script>

%   begin match action with
%   | None -> ()
%   | Some action ->
      <p>Invalid option</p>
     
      
%   end;
  <%s! Dream.form_tag ~action:"/" request %>
  <p>Please choose one of the following options and indicate your choice below: </p>
  <li>Add stock</li>
  <li>Add player</li>
  <li>Buy IPO</li>
  <li>Bid offer</li>
  <li>Ask offer</li>
  <li>Get price</li>
  <li>Get player info</li>

  <br>
  <input name="action" autofocus>
  </form>
  </body>
  </html> 

(*
  main:
*)
let () =
  let (stocks: stock_price_map) = Map.empty (module String) in
  let (bids: order_map) = Map.empty (module String) in
  let (asks: order_map) = Map.empty (module String) in
  let (players: player_map) = Map.empty (module String) in
  let (opinions: opinion_map) = Map.empty (module String) in
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get  "/"
      (fun request ->
        Dream.html (render_home request));

    Dream.post "/"
      (fun request ->
        match%lwt Dream.form request with
        | `Ok ["action", "add stock"] ->
          Dream.html (render_stock request)
        | `Ok ["action", action] -> Dream.html (render_home ~action request)
        | _ ->
          Dream.empty `Bad_Request);
        


  ]
  @@ Dream.not_found
