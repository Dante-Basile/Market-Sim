(* temporarily adding these while in development *)
[@@@ocaml.warning "-26"] (* unused variable *)

open Market_sim_lib;;
open Core;;

(* let (stocks: stock_price_map) = Map.empty (module String);;
let (bids: order_map) = ref Map.empty (module String);;
let (asks: order_map) = ref Map.empty (module String);;
let (players: player_map) = ref Map.empty (module String);;
let (opinions: opinion_map) = ref Map.empty (module String);;
let cur_player = ref "";; *)


let stocks = ref (Map.empty (module String));;
let bids = ref (Map.empty (module String));;
let asks = ref (Map.empty (module String));;
let players = ref (Map.empty (module String));;
(* let opinions = ref (Map.empty (module String));; *)
let cur_player = ref "";;

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

let render_buy_ipo request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Buy IPO</h1>

    <p>Please enter IPO order request in form "STOCK|COUNT|PURCHASE_VALUE|PLAYER_NAME"</p>
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="ipo_order" autofocus>
    </form>
      
  </body>
  </html> 

let render_offer_bid request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Bid Offer</h1>

    <p>Please enter bid offer request in form "STOCK|COUNT|PURCHASE_VALUE|PLAYER_NAME"</p>
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="bid_offer" autofocus>
    </form>
      
  </body>
  </html> 

let render_offer_ask request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Ask Offer</h1>

    <p>Please enter ask offer request in form "STOCK|COUNT|PURCHASE_VALUE|PLAYER_NAME"</p>
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="ask_offer" autofocus>
    </form>
      
  </body>
  </html> 

let render_home ?msg stocks players request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Stock Market Simulator</h1>

%   begin match msg with
%   | Some m ->
      <p><%s m %></p>
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

let format_order (order: string) : string list = 
  String.split_on_chars ~on:['|'; ' '; '\t'; '\n'] order
  |> List.filter ~f:(fun x -> String.(<>) x "")

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
        Dream.html (render_home !stocks !players request));

    Dream.post "/"
      begin
      fun request ->
        match%lwt Dream.form request with
        | `Ok ["action", action] ->
          begin match String.lowercase action with
          | "add stock" -> Dream.html (render_stock request)
          | "add player" -> Dream.html (render_player ~duplicate:false ~approved_player:None request)
          | "buy ipo" -> Dream.html (render_buy_ipo request)
          | "bid offer" -> Dream.html (render_offer_bid request)
          | "ask offer" -> Dream.html (render_offer_ask request)
          | _ -> Dream.html (render_home ~msg:"Invalid choice" !stocks !players request)
          end
        | `Ok ["stock_name", stock_name] -> 
          begin match add_stock stock_name !stocks with
          | Ok updated_stock -> 
            stocks := updated_stock;
            Dream.html (render_home !stocks !players request)
          | Error _ -> Dream.html (render_stock ~duplicate:true request)
          end
        | `Ok ["player_name", player_name] -> 
          let player_list = Map.keys !players in
          begin match List.find player_list ~f:(fun p -> String.(=) p player_name) with
          | None -> 
            cur_player := player_name; 
            Dream.html (render_player ~duplicate:false ~approved_player:(Some player_name) request)
          | Some _ -> Dream.html (render_player ~duplicate:true ~approved_player:None request) (* player already exists *)
          end
        | `Ok ["player_funds", player_funds] ->
          let f = float_of_string player_funds in
          begin match add_player !cur_player f !players with
          | Ok updated_players -> 
            players := updated_players;
            Dream.html (render_home !stocks !players request)
          | Error _ -> failwith "duplicate player not caught in player_name match"
          end
        | `Ok ["ipo_order", ipo_order] -> 
          let o = format_order ipo_order in
          begin match o with
          | [ticker; ct; value; p_id] -> 
            let order = {ticker = ticker; ct = int_of_string ct; value = float_of_string value; p_id = p_id} in
            begin match buy_ipo order !stocks !players with 
            | Ok updated_players ->
              players := updated_players;
              Dream.html (render_home !stocks !players request)
            | Error e -> Dream.html (render_home ~msg:e !stocks !players request)
            end
          | _ -> Dream.html (render_home ~msg:"Invalid IPO order" !stocks !players request)
          end
        | `Ok ["bid_offer", bid_offer] -> 
          let o = format_order bid_offer in
          begin match o with
          | [ticker; ct; value; p_id] -> 
            let order = {ticker = ticker; ct = int_of_string ct; value = float_of_string value; p_id = p_id} in
            begin match offer_bid order !bids !asks !stocks !players with 
            | Ok (updated_bids, updated_asks, updated_stocks, updated_players) ->
              bids := updated_bids;
              asks := updated_asks;
              stocks := updated_stocks;
              players := updated_players;
              Dream.html (render_home !stocks !players request)
            | Error e -> Dream.html (render_home ~msg:e !stocks !players request)
            end
          | _ -> Dream.html (render_home ~msg:"Invalid bid order" !stocks !players request)
          end
        | `Ok ["ask_offer", ask_offer] -> 
          let o = format_order ask_offer in
          begin match o with
          | [ticker; ct; value; p_id] -> 
            let order = {ticker = ticker; ct = int_of_string ct; value = float_of_string value; p_id = p_id} in
            begin match offer_ask order !bids !asks !stocks !players with 
            | Ok (updated_bids, updated_asks, updated_stocks, updated_players) ->
              bids := updated_bids;
              asks := updated_asks;
              stocks := updated_stocks;
              players := updated_players;
              Dream.html (render_home !stocks !players request)
            | Error e -> Dream.html (render_home ~msg:e !stocks !players request)
            end
          | _ -> Dream.html (render_home ~msg:"Invalid ask order" !stocks !players request)
          end
        | `Ok _ -> Dream.html (render_home ~msg:"Invalid choice" !stocks !players request)
        | _ -> Dream.empty `Bad_Request
      end;
        


  ]
  @@ Dream.not_found

  (* to do:
  - catch error where player fund is not float *)