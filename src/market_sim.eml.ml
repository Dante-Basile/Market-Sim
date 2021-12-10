(* temporarily adding these while in development *)
[@@@ocaml.warning "-26"] (* unused variable *)

open Market_sim_lib;;
open Core;;

let (stocks: stock_price_map) = Map.empty (module String);;
let (bids: order_map) = Map.empty (module String);;
let (asks: order_map) = Map.empty (module String);;
let (players: player_map) = Map.empty (module String);;
let (opinions: opinion_map) = Map.empty (module String);;
let cur_player = "";;

let render_stock ?duplicate request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Add Stock</h1>
%   begin match duplicate with
%   | Some true ->
      <p>Stock already exists</p>
%   | _ -> ()
%   end;
    <p>Please enter stock name:</p>
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="stock_name" autofocus>
    </form>
      
  </body>
  </html> 

let render_player ~(duplicate: bool) ~(approved_player: string option) request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Add Player</h1>
%   begin match duplicate with
%   | true ->
      <p>Player already exists</p>
%   | false -> ()
%   end;

%   begin match approved_player with
%   | Some p ->
      <p>Please enter funds for <%s p %>:</p>
      <%s! Dream.form_tag ~action:"/" request %>
        <input name="player_funds" autofocus>
      </form>
%   | None -> 
      <p>Please enter player name:</p>
      <%s! Dream.form_tag ~action:"/" request %>
        <input name="player_name" autofocus>
      </form>
%   end;
  
      
  </body>
  </html> 

let render_home ?invalid_choice stocks players request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Stock Market Simulator</h1>

%   begin match invalid_choice with
%   | Some true ->
      <p>Invalid option</p>
%   | _ -> ()
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
  <p>Current number of stocks: <%s (string_of_int (Map.length stocks)) %></p>
  <br>
  <p>Current number of players: <%s (string_of_int (Map.length players)) %></p>
  </form>
  

  </body>
  </html> 

(*
  main:
*)
let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get  "/"
      (fun request ->
        Dream.html (render_home stocks players request));

    Dream.post "/"
      (fun request ->
        match%lwt Dream.form request with
        | `Ok ["action", action] ->
          begin match String.lowercase action with
          | "add stock" -> Dream.html (render_stock request)
          | "add player" -> Dream.html (render_player ~duplicate:false ~approved_player:None request)
          | _ -> Dream.html (render_home ~invalid_choice:true stocks players request)
          end
        | `Ok ["stock_name", stock_name] -> 
          begin match add_stock stock_name stocks with
          | Ok updated_stock -> 
            let stocks = updated_stock in Dream.html (render_home stocks players request)
          | Error _ -> Dream.html (render_stock ~duplicate:true request)
          end
        | `Ok ["player_name", player_name] -> 
          let player_list = Map.keys players in
          begin match List.find player_list ~f:(fun p -> String.(=) p player_name) with
          | None -> let cur_player = player_name in Dream.html (render_player ~duplicate:false ~approved_player:(Some player_name) request)
          | Some _ -> Dream.html (render_player ~duplicate:true ~approved_player:None request) (* player already exists *)
          end
        | `Ok ["player_funds", player_funds] ->
          let f = float_of_string player_funds in
          begin match add_player cur_player f players with
          | Ok updated_players -> let players = updated_players in Dream.html (render_home stocks players request)
          | Error _ -> failwith "duplicate player not caught in player_name match"
          end
        | `Ok _ -> Dream.html (render_home ~invalid_choice:true stocks players request)
        | _ -> Dream.empty `Bad_Request);
        


  ]
  @@ Dream.not_found
