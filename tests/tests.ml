open Market_sim_lib;;
open Core;;
open OUnit2;;

let test_add_stock _ =
  let (stocks_0: stock_price_map) = Map.empty (module String) in
  let stocks_res_1 = add_stock "AMD" stocks_0 in
  let stocks_1 = 
    match stocks_res_1 with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  assert_equal (Map.find_exn stocks_1 "AMD") ([None]);
  match add_stock "AMD" stocks_1 with
  | Ok _ -> failwith "Error expcted"
  | Error s -> assert_equal s "stock already exists"
;;

let test_add_player _ =
  let (players_0: player_map) = Map.empty (module String) in
  let players_res_1 = add_player "P1" 0. players_0 in
  let players_1 = 
    match players_res_1 with
    | Ok players -> players
    | Error s -> failwith s
  in
  assert_equal (Map.find_exn players_1 "P1").funds [0.];
  match add_player "P1" 0. players_1 with
  | Ok _ -> failwith "Error expcted";
  | Error s -> assert_equal s "player already exists";
  match (add_player "P2" (-1.) players_1) with
  | Ok _ -> failwith " Error expected";
  | Error s -> assert_equal s "starting funds cannot be negative";
;;

let test_buy_ipo _ =
  let (stocks_0: stock_price_map) = Map.empty (module String) in
  let stocks_res_1 = add_stock "AMD" stocks_0 in
  let stocks_1 = 
    match stocks_res_1 with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  let (players_0: player_map) = Map.empty (module String) in
  let players_res_1 = add_player "P1" 5000. players_0 in
  let players_1 =
    match players_res_1 with
    | Ok players_1 -> players_1
    | Error s -> failwith s
  in
  let players_res_2 = buy_ipo {ticker = "AMD"; ct = 100; value = 10.; p_id = "P1";} stocks_1 players_1 in
  let players_2 =
    match players_res_2 with
    | Ok players -> players
    | Error s -> failwith s
  in
  assert_equal (Map.find_exn players_2 "P1").funds [4000.; 5000.];
  assert_equal (Map.find_exn (Map.find_exn players_2 "P1").stocks "AMD") [100];
  let players_res_3 = buy_ipo {ticker = "AMD"; ct = 100; value = 10.; p_id = "P1"} stocks_1 players_2 in
  let players_3 =
    match players_res_3 with
    | Ok players -> players
    | Error s -> failwith s
  in
  assert_equal (Map.find_exn players_3 "P1").funds [3000.; 4000.; 5000.];
  assert_equal (Map.find_exn (Map.find_exn players_3 "P1").stocks "AMD") [200; 100];
  match buy_ipo {ticker = "AMZN"; ct = 100; value = 10.; p_id = "P1"} stocks_1 players_3 with
  | Ok _ -> failwith "error expected"
  | Error s -> assert_equal s "stock not listed on market";
  match buy_ipo {ticker = "AMD"; ct = 100; value = 10.; p_id = "P2"} stocks_1 players_3 with
  | Ok _ -> failwith "error expected"
  | Error s -> assert_equal s "player does not exist";
  match buy_ipo {ticker = "AMD"; ct = 1000; value = 10.; p_id = "P1"} stocks_1 players_3 with
  | Ok _ -> failwith "error expected"
  | Error s -> assert_equal s "insufficient funds";
  match buy_ipo {ticker = "AMZN"; ct = 1; value = 1.; p_id = "P2"} stocks_1 players_3 with
  | Ok _ -> failwith "error expected"
  | Error s -> assert_equal s "stock not listed on market and player does not exist"
;;

let test_offer_bid_ask_cov _ =
  let (stocks: stock_price_map) = Map.empty (module String) in
  let (players: player_map) = Map.empty (module String) in
  let (bids: order_map) = Map.empty (module String) in
  let (asks: order_map) = Map.empty (module String) in
  (* basic bid *)
  let stocks_0 =
    match add_stock "AMD" stocks with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  let players_0 =
    match add_player "P1" 1000. players with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_0, asks_0, stocks_0, players_0 =
    match offer_bid {ticker = "AMD"; ct = 0; value = 10.; p_id = "P1"} bids asks stocks_0 players_0 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_0) (Ok None);
  assert_equal (get_price "E" stocks_0) (Error "stock not listed on market");
  assert_equal (Map.find bids_0 "AMD") (None);
  let bids_0, asks_0, stocks_0, players_0 =
    match offer_bid {ticker = "AMD"; ct = 10; value = 10.; p_id = "P1"} bids_0 asks_0 stocks_0 players_0 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_0) (Ok None);
  assert_equal (Map.find_exn bids_0 "AMD") [{ticker = "AMD"; ct = 10; value = 10.; p_id = "P1"}];
  let players_0 =
    match add_player "P2" 1000. players_0 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let players_0 =
    match buy_ipo {ticker = "AMD"; ct = 100; value = 1.; p_id = "P2";} stocks_0 players_0 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_0, asks_0, stocks_0, players_0 =
    match offer_ask {ticker = "AMD"; ct = 10; value = 15.; p_id = "P2"} bids_0 asks_0 stocks_0 players_0 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_0) (Ok (Some 15.));
  assert_equal (Map.find_exn asks_0 "AMD") [{ticker = "AMD"; ct = 10; value = 15.; p_id = "P2"}];
  let bids_0, asks_0, stocks_0, players_0 =
    match offer_ask {ticker = "AMD"; ct = 10; value = 10.; p_id = "P2"} bids_0 asks_0 stocks_0 players_0 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_0) (Ok (Some 15.));
  assert_equal (Map.find_exn bids_0 "AMD") [];
  assert_equal (Map.find_exn asks_0 "AMD") [{ticker = "AMD"; ct = 10; value = 15.; p_id = "P2"}];
  assert_equal (Map.find_exn players_0 "P1").funds [900.; 1000.];
  assert_equal (Map.find_exn players_0 "P2").funds [1000.; 900.; 1000.];
  assert_equal (Map.find_exn (Map.find_exn players_0 "P1").stocks "AMD") [10];
  assert_equal (Map.find_exn (Map.find_exn players_0 "P2").stocks "AMD") [90; 100];
  let bids_0, asks_0, stocks_0, players_0 =
    match offer_bid {ticker = "AMD"; ct = 10; value = 15.; p_id = "P1"} bids_0 asks_0 stocks_0 players_0 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_0) (Ok None);
  assert_equal (Map.find_exn bids_0 "AMD") [];
  assert_equal (Map.find_exn asks_0 "AMD") [];
  assert_equal (Map.find_exn players_0 "P1").funds [750.; 900.; 1000.];
  assert_equal (Map.find_exn players_0 "P2").funds [1150.; 1000.; 900.; 1000.];
  assert_equal (Map.find_exn (Map.find_exn players_0 "P1").stocks "AMD") [20; 10];
  assert_equal (Map.find_exn (Map.find_exn players_0 "P2").stocks "AMD") [80; 90; 100];
  let e =
    (offer_bid {ticker = "AMD"; ct = 10; value = 10.; p_id = "E"} bids_0 asks_0 stocks_0 players_0)
  in
  assert_equal e (Error "player does not exist");
  let e =
    (offer_bid {ticker = "E"; ct = 10; value = 10.; p_id = "P1"} bids_0 asks_0 stocks_0 players_0)
  in
  assert_equal e (Error "stock not listed on market");
  let e =
    (offer_bid {ticker = "E"; ct = 10; value = 10.; p_id = "E"} bids_0 asks_0 stocks_0 players_0)
  in
  assert_equal e (Error "stock not listed on market and player does not exist");
  (* basic ask *)
  let stocks_1 =
    match add_stock "AMD" stocks with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  let players_1 =
    match add_player "P1" 1000. players with
    | Ok players -> players
    | Error s -> failwith s
  in
  let players_1 =
    match buy_ipo {ticker = "AMD"; ct = 100; value = 1.; p_id = "P1";} stocks_1 players_1 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_1, asks_1, stocks_1, players_1 =
    match offer_ask {ticker = "AMD"; ct = 10; value = 15.; p_id = "P1"} bids asks stocks_1 players_1 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_1) (Ok (Some 15.));
  assert_equal (Map.find_exn asks_1 "AMD") [{ticker = "AMD"; ct = 10; value = 15.; p_id = "P1"}];
  let players_1 =
    match add_player "P2" 1000. players_1 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_1, asks_1, stocks_1, players_1 =
    match offer_bid {ticker = "AMD"; ct = 10; value = 10.; p_id = "P2"} bids_1 asks_1 stocks_1 players_1 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_1) (Ok (Some 15.));
  assert_equal (Map.find_exn bids_1 "AMD") [{ticker = "AMD"; ct = 10; value = 10.; p_id = "P2"}];
  let bids_1, asks_1, stocks_1, players_1 =
    match offer_bid {ticker = "AMD"; ct = 10; value = 15.; p_id = "P2"} bids_1 asks_1 stocks_1 players_1 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_1) (Ok None);
  assert_equal (Map.find_exn bids_1 "AMD") [{ticker = "AMD"; ct = 10; value = 10.; p_id = "P2"}];
  assert_equal (Map.find_exn asks_1 "AMD") [];
  assert_equal (Map.find_exn players_1 "P1").funds [1050.; 900.; 1000.];
  assert_equal (Map.find_exn players_1 "P2").funds [850.; 1000.];
  assert_equal (Map.find_exn (Map.find_exn players_1 "P1").stocks "AMD") [90; 100];
  assert_equal (Map.find_exn (Map.find_exn players_1 "P2").stocks "AMD") [10];
  let bids_1, asks_1, stocks_1, players_1 =
    match offer_ask {ticker = "AMD"; ct = 10; value = 10.; p_id = "P1"} bids_1 asks_1 stocks_1 players_1 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_1) (Ok None);
  assert_equal (Map.find_exn bids_1 "AMD") [];
  assert_equal (Map.find_exn asks_1 "AMD") [];
  assert_equal (Map.find_exn players_1 "P1").funds [1150.; 1050.; 900.; 1000.];
  assert_equal (Map.find_exn players_1 "P2").funds [750.; 850.; 1000.];
  assert_equal (Map.find_exn (Map.find_exn players_1 "P1").stocks "AMD") [80; 90; 100];
  assert_equal (Map.find_exn (Map.find_exn players_1 "P2").stocks "AMD") [20; 10];
  let e =
    (offer_ask {ticker = "AMD"; ct = 10; value = 10.; p_id = "E"} bids_0 asks_0 stocks_0 players_0)
  in
  assert_equal e (Error "player does not exist");
  let e =
    (offer_ask {ticker = "E"; ct = 10; value = 10.; p_id = "P1"} bids_0 asks_0 stocks_0 players_0)
  in
  assert_equal e (Error "stock not listed on market");
  let e =
    (offer_ask {ticker = "E"; ct = 10; value = 10.; p_id = "E"} bids_0 asks_0 stocks_0 players_0)
  in
  assert_equal e (Error "stock not listed on market and player does not exist");
  let stocks_e =
    match add_stock "E" stocks with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  let e =
    (offer_ask {ticker = "E"; ct = 10; value = 10.; p_id = "P1"} bids_0 asks_0 stocks_e players_0)
  in
  assert_equal e (Error "player has never owned this stock");
  let players_e =
    match buy_ipo {ticker = "E"; ct = 0; value = 1.; p_id = "P1";} stocks_e players_1 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let e =
    (offer_ask {ticker = "E"; ct = 10; value = 10.; p_id = "P1"} bids_0 asks_0 stocks_e players_e)
  in
  assert_equal e (Error "player ownes 0 shares of this stock");
  (* bid and ask over and under order size *)
  let stocks_2 =
    match add_stock "AMD" stocks with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  let players_2 =
    match add_player "P1" 1000. players with
    | Ok players -> players
    | Error s -> failwith s
  in
  let players_2 =
    match buy_ipo {ticker = "AMD"; ct = 100; value = 1.; p_id = "P1";} stocks_2 players_2 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_2, asks_2, stocks_2, players_2 =
    match offer_ask {ticker = "AMD"; ct = 10; value = 15.; p_id = "P1"} bids asks stocks_2 players_2 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  let players_2 =
    match add_player "P2" 1000. players_2 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_2, asks_2, stocks_2, players_2 =
    match offer_bid {ticker = "AMD"; ct = 4; value = 15.; p_id = "P2"} bids_2 asks_2 stocks_2 players_2 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_2) (Ok (Some 15.));
  assert_equal (Map.find bids_2 "AMD") None;
  assert_equal (Map.find_exn asks_2 "AMD") [{ticker = "AMD"; ct = 6; value = 15.; p_id = "P1"}];
  assert_equal (Map.find_exn players_2 "P1").funds [960.; 900.; 1000.];
  assert_equal (Map.find_exn players_2 "P2").funds [940.; 1000.];
  assert_equal (Map.find_exn (Map.find_exn players_2 "P1").stocks "AMD") [96; 100];
  assert_equal (Map.find_exn (Map.find_exn players_2 "P2").stocks "AMD") [4];
  let bids_2, asks_2, stocks_2, players_2 =
    match offer_bid {ticker = "AMD"; ct = 7; value = 15.; p_id = "P2"} bids_2 asks_2 stocks_2 players_2 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_2) (Ok None);
  assert_equal (Map.find_exn bids_2 "AMD") [{ticker = "AMD"; ct = 1; value = 15.; p_id = "P2"}];
  assert_equal (Map.find_exn asks_2 "AMD") [];
  assert_equal (Map.find_exn players_2 "P1").funds [1050.; 960.; 900.; 1000.];
  assert_equal (Map.find_exn players_2 "P2").funds [850.; 940.; 1000.];
  assert_equal (Map.find_exn (Map.find_exn players_2 "P1").stocks "AMD") [90; 96; 100];
  assert_equal (Map.find_exn (Map.find_exn players_2 "P2").stocks "AMD") [10; 4];
  let bids_2, asks_2, stocks_2, players_2 =
    match offer_ask {ticker = "AMD"; ct = 10; value = 15.; p_id = "P1"} bids_2 asks_2 stocks_2 players_2 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_2) (Ok (Some 15.));
  assert_equal (Map.find_exn bids_2 "AMD") [];
  assert_equal (Map.find_exn asks_2 "AMD") [{ticker = "AMD"; ct = 9; value = 15.; p_id = "P1"}];
  assert_equal (Map.find_exn players_2 "P1").funds [1065.; 1050.; 960.; 900.; 1000.];
  assert_equal (Map.find_exn players_2 "P2").funds [835.; 850.; 940.; 1000.];
  assert_equal (Map.find_exn (Map.find_exn players_2 "P1").stocks "AMD") [89; 90; 96; 100];
  assert_equal (Map.find_exn (Map.find_exn players_2 "P2").stocks "AMD") [11; 10; 4];
  let bids_2, asks_2, stocks_2, players_2 =
    match offer_bid {ticker = "AMD"; ct = 19; value = 15.; p_id = "P2"} bids_2 asks_2 stocks_2 players_2 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  let bids_2, asks_2, stocks_2, players_2 =
    match offer_ask {ticker = "AMD"; ct = 8; value = 15.; p_id = "P1"} bids_2 asks_2 stocks_2 players_2 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_price "AMD" stocks_2) (Ok None);
  assert_equal (Map.find_exn bids_2 "AMD") [{ticker = "AMD"; ct = 2; value = 15.; p_id = "P2"}];
  assert_equal (Map.find_exn asks_2 "AMD") [];
  assert_equal (Map.find_exn players_2 "P1").funds [1320.; 1200.; 1065.; 1050.; 960.; 900.; 1000.];
  assert_equal (Map.find_exn players_2 "P2").funds [580.; 700.; 835.; 850.; 940.; 1000.];
  assert_equal (Map.find_exn (Map.find_exn players_2 "P1").stocks "AMD") [72; 80; 89; 90; 96; 100];
  assert_equal (Map.find_exn (Map.find_exn players_2 "P2").stocks "AMD") [28; 20; 11; 10; 4];
  (* bid and ask sorting *)
  let stocks_3 =
    match add_stock "AMD" stocks with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  let players_3 =
    match add_player "P1" 1000. players with
    | Ok players -> players
    | Error s -> failwith s
  in
  let players_3 =
    match buy_ipo {ticker = "AMD"; ct = 100; value = 1.; p_id = "P1";} stocks_3 players_3 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_3, asks_3, stocks_3, players_3 =
    match offer_ask {ticker = "AMD"; ct = 10; value = 15.; p_id = "P1"} bids asks stocks_3 players_3 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  let bids_3, asks_3, stocks_3, players_3 =
    match offer_ask {ticker = "AMD"; ct = 10; value = 17.; p_id = "P1"} bids_3 asks_3 stocks_3 players_3 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  let players_3 =
    match add_player "P2" 1000. players_3 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_3, asks_3, stocks_3, players_3 =
    match offer_bid {ticker = "AMD"; ct = 10; value = 10.; p_id = "P2"} bids_3 asks_3 stocks_3 players_3 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  let bids_3, asks_3, _, _ =
    match offer_bid {ticker = "AMD"; ct = 10; value = 8.; p_id = "P2"} bids_3 asks_3 stocks_3 players_3 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  let bids_exp =
    [{ticker = "AMD"; ct = 10; value = 10.; p_id = "P2"};
     {ticker = "AMD"; ct = 10; value = 8.; p_id = "P2"}]
  in
  let asks_exp =
    [{ticker = "AMD"; ct = 10; value = 15.; p_id = "P1"};
     {ticker = "AMD"; ct = 10; value = 17.; p_id = "P1"}]
  in
  assert_equal (Map.find_exn bids_3 "AMD") bids_exp;
  assert_equal (Map.find_exn asks_3 "AMD") asks_exp;
;;

let test_get_player_info _ =
  let (stocks: stock_price_map) = Map.empty (module String) in
  let (players: player_map) = Map.empty (module String) in
  let stocks_0 =
    match add_stock "AMD" stocks with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  let players_0 =
    match add_player "P1" 1000. players with
    | Ok players -> players
    | Error s -> failwith s
  in
  let players_0 =
    match buy_ipo {ticker = "AMD"; ct = 100; value = 1.; p_id = "P1";} stocks_0 players_0 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let f_info_0, s_info_0 = 
    match get_player_info "P1" players_0 with
    | Ok i -> i
    | Error s -> failwith s
  in
  assert_equal f_info_0 900.;
  assert_equal (List.length s_info_0) 1;
  assert_equal (get_player_info "E" players_0) (Error "player does not exist")
;;

let test_get_bid_ask _ =
  let (stocks: stock_price_map) = Map.empty (module String) in
  let (players: player_map) = Map.empty (module String) in
  let (bids: order_map) = Map.empty (module String) in
  let (asks: order_map) = Map.empty (module String) in
  let stocks_0 =
    match add_stock "AMD" stocks with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  assert_equal (get_bid_ask_spread "AMD" bids asks) (Error "no active bid or ask");
  let players_0 =
    match add_player "P1" 1000. players with
    | Ok players -> players
    | Error s -> failwith s
  in
  let players_0 =
    match add_player "P2" 1000. players_0 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let players_0 =
    match buy_ipo {ticker = "AMD"; ct = 100; value = 1.; p_id = "P1";} stocks_0 players_0 with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_0, asks_0, _, _ =
    match offer_ask {ticker = "AMD"; ct = 10; value = 15.; p_id = "P1"} bids asks stocks_0 players_0 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_bid_ask "AMD" bids_0 asks_0) (None, Some 15.);
  assert_equal (get_bid_ask_spread "AMD" bids_0 asks_0) (Error "no active bid");
  let bids_0, asks_0, _, _ =
    match offer_bid {ticker = "AMD"; ct = 10; value = 10.; p_id = "P2"} bids_0 asks_0 stocks_0 players_0 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_bid_ask_spread "AMD" bids_0 asks_0) (Ok 5.);
  let stocks_1 =
    match add_stock "AMD" stocks with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  assert_equal (get_bid_ask_spread "AMD" bids asks) (Error "no active bid or ask");
  let players_1 =
    match add_player "P1" 1000. players with
    | Ok players -> players
    | Error s -> failwith s
  in
  let bids_1, asks_1, _, _ =
    match offer_bid {ticker = "AMD"; ct = 10; value = 10.; p_id = "P1"} bids asks stocks_1 players_1 with
    | Ok (bids, asks, stocks, players) -> bids, asks, stocks, players
    | Error s -> failwith s
  in
  assert_equal (get_bid_ask_spread "AMD" bids_1 asks_1) (Error "no active ask");
;;

let test_get_line_plot _ =
  let f_0 = get_line_plot [0.; 1.; 2.] [4.; 6.; 26.] in
  assert_equal (f_0 0.5) 5.;
  assert_equal (f_0 1.25) 11.;
  assert_equal (f_0 (-0.1)) 0.;
  assert_equal (f_0 2.1) 0.;
  let f_1 = get_line_plot [0.; 1.; 2.; 3.] [4.; 6.; 26.; 18.] in
  assert_equal (f_1 0.5) 5.;
  assert_equal (f_1 1.25) 11.;
  assert_equal (f_1 2.25) 24.;
  assert_equal (f_1 (-0.1)) 0.;
  assert_equal (f_1 3.1) 0.;
  let f_2 = get_line_plot [0.] [0.] in
  assert_equal (f_2 1.) 0.;
;;

let test_opinion _ =
  let (stocks: stock_price_map) = Map.empty (module String) in
  let (opinions: opinion_map) = Map.empty (module String) in
  let opinions_0 = random_shift_opinion opinions stocks in
  assert_equal (List.length (Map.keys opinions_0)) 0;
  assert_equal (get_opinion "AMD" opinions_0) (Error "public opinion of this stock is unknown");
  let stocks_0 =
    match add_stock "AMD" stocks with
    | Ok stocks -> stocks
    | Error s -> failwith s
  in
  let opinions_0 = random_shift_opinion opinions_0 stocks_0 in
  assert_equal (List.length (Map.keys opinions_0)) 1;
  let opinions_0 = random_shift_opinion opinions_0 stocks_0 in
  assert_equal (List.length (Map.keys opinions_0)) 1;
;;

let market_sim_tests =
  "market sim" >: test_list [
    "add stock"                         >:: test_add_stock;
    "add player"                        >:: test_add_player;
    "buy ipo"                           >:: test_buy_ipo;
    "offer bid and offer ask coverage"  >:: test_offer_bid_ask_cov;
    "get player info"                   >:: test_get_player_info;
    "get bid ask"                       >:: test_get_bid_ask;
    "get line plot"                     >:: test_get_line_plot;
    "opinion"                           >:: test_opinion;
  ]

let test_series =
  "Project Tests" >::: [
    market_sim_tests;
  ]

let () = 
run_test_tt_main test_series
