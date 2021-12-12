#1 "market_sim.eml.ml"
open Market_sim_lib;;
open Core;;

let stocks = ref (Map.empty (module String));;
let bids = ref (Map.empty (module String));;
let asks = ref (Map.empty (module String));;
let players = ref (Map.empty (module String));;
let opinions = ref (Map.empty (module String));;
let cur_player = ref "";;

let render_stock ?duplicate request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Add Stock</h1>\n");
#20 "market_sim.eml.ml"
    begin match duplicate with

#21 "market_sim.eml.ml"
    | Some true ->

(Buffer.add_string ___eml_buffer "      <p>Stock already exists</p>\n");
#23 "market_sim.eml.ml"
    | _ -> ()

#24 "market_sim.eml.ml"
    end;

(Buffer.add_string ___eml_buffer "    <p>Please enter stock name:</p>\n  ");
(Printf.bprintf ___eml_buffer "%s" (
#26 "market_sim.eml.ml"
         Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n    <input name=\"stock_name\" autofocus>\n  </form>\n    \n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#33 "market_sim.eml.ml"
let render_player ?msg ~(duplicate: bool) ~(approved_player: string option) request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Add Player</h1>\n");
#42 "market_sim.eml.ml"
    begin match duplicate with

#43 "market_sim.eml.ml"
    | true ->

(Buffer.add_string ___eml_buffer "      <p>Player already exists</p>\n");
#45 "market_sim.eml.ml"
    | false -> ()

#46 "market_sim.eml.ml"
    end;

(Buffer.add_string ___eml_buffer "\n");
#48 "market_sim.eml.ml"
    begin match msg with

#49 "market_sim.eml.ml"
    | Some m ->

(Buffer.add_string ___eml_buffer "      <p>");
(Printf.bprintf ___eml_buffer "%s" (Dream.html_escape (
#50 "market_sim.eml.ml"
             m 
)));
(Buffer.add_string ___eml_buffer "</p>\n");
#51 "market_sim.eml.ml"
    | _ -> ()

#52 "market_sim.eml.ml"
    end;

(Buffer.add_string ___eml_buffer "\n");
#54 "market_sim.eml.ml"
    begin match approved_player with

#55 "market_sim.eml.ml"
    | Some p ->

(Buffer.add_string ___eml_buffer "      <p>Please enter funds for ");
(Printf.bprintf ___eml_buffer "%s" (Dream.html_escape (
#56 "market_sim.eml.ml"
                                    p 
)));
(Buffer.add_string ___eml_buffer ":</p>\n    ");
(Printf.bprintf ___eml_buffer "%s" (
#57 "market_sim.eml.ml"
           Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n      <input name=\"player_funds\" autofocus>\n    </form>\n");
#60 "market_sim.eml.ml"
    | None -> 

(Buffer.add_string ___eml_buffer "      <p>Please enter player name:</p>\n    ");
(Printf.bprintf ___eml_buffer "%s" (
#62 "market_sim.eml.ml"
           Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n      <input name=\"player_name\" autofocus>\n    </form>\n");
#65 "market_sim.eml.ml"
    end;

(Buffer.add_string ___eml_buffer "  </body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#69 "market_sim.eml.ml"
let render_buy_ipo request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Buy IPO</h1>\n\n  <p>Please enter IPO order request in form \"STOCK|COUNT|PURCHASE_VALUE|PLAYER_NAME\"</p>\n  ");
(Printf.bprintf ___eml_buffer "%s" (
#80 "market_sim.eml.ml"
         Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n    <input name=\"ipo_order\" autofocus>\n  </form>   \n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#86 "market_sim.eml.ml"
let render_offer_bid request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Bid Offer</h1>\n\n  <p>Please enter bid offer request in form \"STOCK|COUNT|PURCHASE_VALUE|PLAYER_NAME\"</p>\n  ");
(Printf.bprintf ___eml_buffer "%s" (
#97 "market_sim.eml.ml"
         Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n    <input name=\"bid_offer\" autofocus>\n  </form>\n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#103 "market_sim.eml.ml"
let render_offer_ask request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Ask Offer</h1>\n\n  <p>Please enter ask offer request in form \"STOCK|COUNT|PURCHASE_VALUE|PLAYER_NAME\"</p>\n  ");
(Printf.bprintf ___eml_buffer "%s" (
#114 "market_sim.eml.ml"
         Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n    <input name=\"ask_offer\" autofocus>\n  </form>\n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#120 "market_sim.eml.ml"
let render_get_price request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Get Stock Price</h1>\n  <p>Please enter stock</p>\n  ");
(Printf.bprintf ___eml_buffer "%s" (
#130 "market_sim.eml.ml"
         Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n    <input name=\"stock_get_price\" autofocus>\n  </form>\n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#136 "market_sim.eml.ml"
let render_get_player_info request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Get Player Info</h1>\n  <p>Please enter player name</p>\n  ");
(Printf.bprintf ___eml_buffer "%s" (
#146 "market_sim.eml.ml"
         Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n    <input name=\"player_name_info\" autofocus>\n  </form>\n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#152 "market_sim.eml.ml"
let render_get_bid_ask request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Get Highest Bid and Lowest Ask for Stock</h1>\n  <p>Please enter stock</p>\n  ");
(Printf.bprintf ___eml_buffer "%s" (
#162 "market_sim.eml.ml"
         Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n    <input name=\"stock_get_bid_ask\" autofocus>\n  </form>\n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#168 "market_sim.eml.ml"
let render_bid_ask_spread request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Get Bid Ask Spread for Stock</h1>\n  <p>Please enter stock</p>\n  ");
(Printf.bprintf ___eml_buffer "%s" (
#178 "market_sim.eml.ml"
         Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n    <input name=\"stock_bid_ask_spread\" autofocus>\n  </form>\n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#184 "market_sim.eml.ml"
let render_get_opinion request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Get Opinion for Stock</h1>\n  <p>Please enter stock</p>\n  ");
(Printf.bprintf ___eml_buffer "%s" (
#194 "market_sim.eml.ml"
         Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n    <input name=\"stock_get_opinion\" autofocus>\n  </form>\n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#200 "market_sim.eml.ml"
let render_home ?msg ?res stocks players request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n  <title>Stock Market Simulator</title>\n</head>\n<body>\n  <h1>Stock Market Simulator</h1>\n\n");
#210 "market_sim.eml.ml"
    begin match msg with

#211 "market_sim.eml.ml"
    | Some m ->

(Buffer.add_string ___eml_buffer "      <p>");
(Printf.bprintf ___eml_buffer "%s" (Dream.html_escape (
#212 "market_sim.eml.ml"
             m 
)));
(Buffer.add_string ___eml_buffer "</p>\n");
#213 "market_sim.eml.ml"
    | _ -> ()

#214 "market_sim.eml.ml"
    end;

(Buffer.add_string ___eml_buffer "\n");
#216 "market_sim.eml.ml"
    begin match res with

#217 "market_sim.eml.ml"
    | Some (p_funds, p_stocks) -> 

(Buffer.add_string ___eml_buffer "      <p>Funds: ");
(Printf.bprintf ___eml_buffer "%s" (Dream.html_escape (
#218 "market_sim.eml.ml"
                    string_of_float p_funds
)));
(Buffer.add_string ___eml_buffer "</p>\n    ");
#219 "market_sim.eml.ml"
         List.iter p_stocks ~f:begin fun (s, n) -> 
(Buffer.add_string ___eml_buffer "\n      <p>Stock: ");
(Printf.bprintf ___eml_buffer "%s" (Dream.html_escape (
#220 "market_sim.eml.ml"
                      s 
)));
(Buffer.add_string ___eml_buffer " Count: ");
(Printf.bprintf ___eml_buffer "%s" (Dream.html_escape (
#220 "market_sim.eml.ml"
                                      string_of_int n 
)));
(Buffer.add_string ___eml_buffer "</p>\n    ");
#221 "market_sim.eml.ml"
         end; 
(Buffer.add_string ___eml_buffer "\n");
#222 "market_sim.eml.ml"
    | _ -> ()

#223 "market_sim.eml.ml"
    end;

(Buffer.add_string ___eml_buffer "\n");
(Printf.bprintf ___eml_buffer "%s" (
#225 "market_sim.eml.ml"
       Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\n<p>Please choose one of the following options and indicate your choice below: </p>\n<li>Add stock</li>\n<li>Add player</li>\n<li>Buy IPO</li>\n<li>Bid offer</li>\n<li>Ask offer</li>\n<li>Get stock price</li>\n<li>Get player info</li>\n<li>Get highest bid lowest ask</li>\n<li>Get bid ask spread</li>\n<li>Shift opinion randomly</li>\n<li>Get stock opinion</li>\n\n<br>\n<input name=\"action\" autofocus>\n\n<br>\n<p>Current number of stocks: ");
(Printf.bprintf ___eml_buffer "%s" (Dream.html_escape (
#243 "market_sim.eml.ml"
                                   (string_of_int (Map.length stocks)) 
)));
(Buffer.add_string ___eml_buffer "</p>\n<br>\n<p>Current number of players: ");
(Printf.bprintf ___eml_buffer "%s" (Dream.html_escape (
#245 "market_sim.eml.ml"
                                    (string_of_int (Map.length players)) 
)));
(Buffer.add_string ___eml_buffer "</p>\n</form>\n</body>\n</html> \n\n");
(Buffer.contents ___eml_buffer)
#250 "market_sim.eml.ml"
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