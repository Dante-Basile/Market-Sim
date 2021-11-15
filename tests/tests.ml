open Market_sim_lib;;
open Core;;
open OUnit2;;

let test_add_stock _ =
  let (stocks_0: stock_price_map) = Map.empty (module String) in
  let stocks_res_1 = add_stock "AMD" stocks_0 in
  let stocks_1 = 
    match stocks_res_1 with
    | Ok stocks_1 -> stocks_1
    | Error s -> failwith s
  in
  assert_equal (Map.find_exn stocks_1 "AMD") ([]);
  let stocks_res_2 = add_stock "AMD" stocks_1 in
  match stocks_res_2 with
  | Ok _ -> failwith "Error expcted"
  | Error s -> assert_equal s "stock already exists"
;;

let test_add_player _ =
  let (players_0: player_map) = Map.empty (module String) in
  let players_res_1 = add_player "P1" players_0 in
  let players_1 = 
    match players_res_1 with
    | Ok players_1 -> players_1
    | Error s -> failwith s
  in
  assert_equal (Map.find_exn players_1 "P1").funds [0.];
  let players_res_2 = add_player "P1" players_1 in
  match players_res_2 with
  | Ok _ -> failwith "Error expcted"
  | Error s -> assert_equal s "player already exists"
;;

let market_sim_tests =
  "market sim" >: test_list [
    "add stock" >:: test_add_stock;
    "add player" >:: test_add_player;
  ]

let test_series =
  "Project Tests" >::: [
    market_sim_tests;
  ]

let () = 
run_test_tt_main test_series