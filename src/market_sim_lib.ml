[@@@ocaml.warning "-27"]

open Core

type stock_price_map = float list Map.M(String).t

type stock_ct_map = int list Map.M(String).t

type player = {funds: float list; stocks: stock_ct_map}

type player_map = player Map.M(String).t 

type order = {id: string; ct: int; value: float}

type order_map = order list Map.M(String).t

let add_stock (ticker: string) (stocks: stock_price_map): (stock_price_map, string) result =
  match Map.add stocks ~key:ticker ~data:([]) with
  | `Ok stocks_new -> Ok stocks_new
  | `Duplicate -> Error "stock already exists"

let add_player (id: string) (pm: player_map): (player_map, string) result =
  match Map.add pm ~key:id ~data:{funds = [0.]; stocks = Map.empty (module String)} with
  | `Ok pm_new -> Ok pm_new
  | `Duplicate -> Error "player already exists"

(*
*)
let buy (ticker: string) (o: order) (players: player_map): (player_map, string) result =
  let inc_stock_ct (ct_add: int) (l_ct: int list option): int list =
    match l_ct with
    | Some l -> 
      let new_ct = 
        match l with
        current_ct :: l_r -> (ct_add + current_ct)
        | [] -> failwith "player stocks has no ct"
      in
      new_ct :: l
    | None -> [ct_add]
  in
  match Map.find players o.id with
  | Some p -> 
    let new_amt = 
      match p.funds with
      | current_amt :: funds_r -> current_amt -. (Float.of_int o.ct) *. o.value
      | [] -> failwith "player has no current funds"
    in
    if Float.(>=) new_amt 0. then
      let new_funds = new_amt :: p.funds in
      let new_stocks = Map.update p.stocks ticker ~f:(inc_stock_ct o.ct) in
      let new_player = {funds = new_funds; stocks = new_stocks} in
      Ok (Map.set players ~key:o.id ~data:new_player)
    else
      Error "insufficient funds"
  | None -> Error "player does not exist"

let buy_ipo (ticker: string) (o: order) (stocks: stock_price_map) (players: player_map): (player_map, string) result =
  match Map.find stocks ticker with
  | Some _ -> buy ticker o players
  | None -> Error "stock not listed on market"

(*
*)
let add_order (ticker: string) (o: order) (orders: order_map) (stocks: stock_price_map):
    (order_map * stock_price_map) =
  failwith "unimplemented"

let offer_bid (ticker: string) (o: order) (asks: order_map) (stocks: stock_price_map) (players: player_map):
    ((order_map * stock_price_map * player_map), string) result =
  failwith "unimplemented"

let offer_ask (ticker: string) (o: order) (bids: order_map) (stocks: stock_price_map) (players: player_map):
    ((order_map * stock_price_map * player_map), string) result =
  failwith "unimplemented"

let get_price (ticker: string) (asks: order_map): (float, string) result =
  failwith "unimplemented"

let get_bid_ask (ticker: string): ((float * float), string) result =
  failwith "unimplemented"

let get_bid_ask_spread (ticker: string): (float, string) result =
  failwith "unimplemented"
