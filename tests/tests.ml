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
  | Ok _ -> failwith "Error expcted"
  | Error s -> assert_equal s "player already exists"
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
;;

let market_sim_tests =
  "market sim" >: test_list [
    "add stock"     >:: test_add_stock;
    "add player"    >:: test_add_player;
    "buy ipo"       >:: test_buy_ipo
  ]

let test_series =
  "Project Tests" >::: [
    market_sim_tests;
  ]

let () = 
run_test_tt_main test_series