open Market_sim_lib;;
open Core;;

let stocks = ref (Map.empty (module String));;
let bids = ref (Map.empty (module String));;
let asks = ref (Map.empty (module String));;
let players = ref (Map.empty (module String));;
let opinions = ref (Map.empty (module String));;
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

let render_player ?msg ~(duplicate: bool) ~(approved_player: string option) request =
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

%   begin match msg with
%   | Some m ->
      <p><%s m %></p>
%   | _ -> ()
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

let render_get_price request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Get Stock Price</h1>
    <p>Please enter stock</p>
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="stock_get_price" autofocus>
    </form>
  </body>
  </html> 

let render_get_player_info request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Get Player Info</h1>
    <p>Please enter player name</p>
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="player_name_info" autofocus>
    </form>
  </body>
  </html> 

let render_get_bid_ask request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Get Highest Bid and Lowest Ask for Stock</h1>
    <p>Please enter stock</p>
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="stock_get_bid_ask" autofocus>
    </form>
  </body>
  </html> 

let render_bid_ask_spread request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Get Bid Ask Spread for Stock</h1>
    <p>Please enter stock</p>
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="stock_bid_ask_spread" autofocus>
    </form>
  </body>
  </html> 

let render_get_opinion request =
  <html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Stock Market Simulator</title>
  </head>
  <body>
    <h1>Get Opinion for Stock</h1>
    <p>Please enter stock</p>
    <%s! Dream.form_tag ~action:"/" request %>
      <input name="stock_get_opinion" autofocus>
    </form>
  </body>
  </html> 
  
let render_home ?msg ?res stocks players request =
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

%   begin match res with
%   | Some (p_funds, p_stocks) -> 
      <p>Funds: <%s string_of_float p_funds%></p>
      <% List.iter p_stocks ~f:begin fun (s, n) -> %>
        <p>Stock: <%s s %> Count: <%s string_of_int n %></p>
      <% end; %>
%   | _ -> ()
%   end;

  <%s! Dream.form_tag ~action:"/" request %>
  <p>Please choose one of the following options and indicate your choice below: </p>
  <li>Add stock</li>
  <li>Add player</li>
  <li>Buy IPO</li>
  <li>Bid offer</li>
  <li>Ask offer</li>
  <li>Get stock price</li>
  <li>Get player info</li>
  <li>Get highest bid lowest ask</li>
  <li>Get bid ask spread</li>
  <li>Shift opinion randomly</li>
  <li>Get stock opinion</li>

  <br>
  <input name="action" autofocus>

  <br>
  <p>Current number of stocks: <%s (string_of_int (Map.length stocks)) %></p>
  <br>
  <p>Current number of players: <%s (string_of_int (Map.length players)) %></p>
  </form>
  </body>
  </html> 

let format_order (order: string) : string list = 
  String.split_on_chars ~on:['|'; ' '; '\t'; '\n'] order
  |> List.filter ~f:(fun x -> String.(<>) x "");;

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
          | "get stock price" -> Dream.html (render_get_price request)
          | "get player info" -> Dream.html (render_get_player_info request)
          | "get highest bid lowest ask" -> Dream.html (render_get_bid_ask request)
          | "get bid ask spread" -> Dream.html (render_bid_ask_spread request)
          | "shift opinion randomly" -> 
            opinions := random_shift_opinion !opinions !stocks;
            Dream.html (render_home ~msg:"Shifted opinion randomly" !stocks !players request)
          | "get opinion" -> Dream.html (render_get_opinion request)
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
          let f = 
            try float_of_string player_funds
            with Failure _ -> -1.
          in
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
        | `Ok ["stock_get_price", stock_get_price] ->
          begin match get_price stock_get_price !stocks with
          | Ok (Some p) -> Dream.html (render_home ~msg:(String.concat ~sep:"" ["Current price of "; stock_get_price; ": "; string_of_float p]) !stocks !players request)
          | Error e -> Dream.html (render_home ~msg:e !stocks !players request)
          | _ -> Dream.html (render_home ~msg:"No price found" !stocks !players request)
          end
        | `Ok ["player_name_info", player_name_info] -> 
          begin match get_player_info player_name_info !players with
          | Ok res -> 
            Dream.html (render_home ~msg:(String.concat ~sep:"" ["Info for "; player_name_info]) ~res !stocks !players request)
          | Error e -> Dream.html (render_home ~msg:e !stocks !players request)
          end
        | `Ok ["stock_get_bid_ask", stock_get_bid_ask] ->
          begin match get_bid_ask stock_get_bid_ask !bids !asks with
          | (Some max_bid, Some min_ask) -> 
            Dream.html (render_home ~msg:(String.concat ~sep:"" [stock_get_bid_ask; ": \n"; "Highest bid: "; string_of_float max_bid; "\nLowest Ask: "; string_of_float min_ask]) !stocks !players request)
          | _ -> Dream.html (render_home ~msg:(String.concat ~sep:"" [stock_get_bid_ask; " not found"]) !stocks !players request)
          end
        | `Ok ["stock_bid_ask_spread", stock_bid_ask_spread] ->
          begin match get_bid_ask_spread stock_bid_ask_spread !bids !asks with
          | Ok diff -> Dream.html (render_home ~msg:(String.concat ~sep:"" ["Bid ask spread of "; stock_bid_ask_spread; ": "; string_of_float diff]) !stocks !players request)
          | Error e -> Dream.html (render_home ~msg:e !stocks !players request)
          end
        | `Ok ["stock_get_opinion", stock_get_opinion] ->
          begin match get_opinion stock_get_opinion !opinions with
          | Ok res -> Dream.html (render_home ~msg:(String.concat ~sep:"" ["Opinion rating for "; stock_get_opinion; ": "; string_of_int res]) !stocks !players request)
          | Error e -> Dream.html (render_home ~msg:e !stocks !players request)
          end
        | `Ok _ -> Dream.html (render_home ~msg:"Invalid choice" !stocks !players request)
        | _ -> Dream.empty `Bad_Request
      end;
  ]
  @@ Dream.not_found

  (* to do:
  - catch error where player fund is not float *)