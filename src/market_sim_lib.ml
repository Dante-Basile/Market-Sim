[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

open Core

type stock_price_map = float list Map.M(String).t

type stock_ct_map = int list Map.M(String).t

type order = {ticker: string; ct: int; value: float; p_id: string;}

type order_map = order list Map.M(String).t

(*
TODO: need unf orders and ct?
*)
type player = {funds: float list; stocks: stock_ct_map; unf_orders: (order * int) list; order_ct: int}

type player_map = player Map.M(String).t 

let add_stock (ticker: string) (stocks: stock_price_map): (stock_price_map, string) result =
  match Map.add stocks ~key:ticker ~data:([]) with
  | `Ok stocks_new -> Ok stocks_new
  | `Duplicate -> Error "stock already exists"

let add_player (id: string) (st_funds: float) (pm: player_map): (player_map, string) result =
  match Map.add pm ~key:id ~data:{funds = [st_funds]; stocks = Map.empty (module String); unf_orders = []; order_ct = 0} with
  | `Ok pm_new -> Ok pm_new
  | `Duplicate -> Error "player already exists"

(*
*)
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

(*
TODO: use consecutive matching
*)
let buy_ipo (o: order) (stocks: stock_price_map) (players: player_map): (player_map, string) result =
  match Map.find stocks o.ticker with
  | Some _ ->
    begin
    match Map.find players o.p_id with
    | Some p -> 
      let new_amt = 
        match p.funds with
        | current_amt :: funds_r -> current_amt -. (Float.of_int o.ct) *. o.value
        | [] -> failwith "player has no current funds"
      in
      if Float.(>=) new_amt 0. then
        let new_funds = new_amt :: p.funds in
        let new_stocks = Map.update p.stocks o.ticker ~f:(inc_stock_ct o.ct) in
        let new_player = {funds = new_funds; stocks = new_stocks; unf_orders = p.unf_orders; order_ct = p.order_ct} in
        Ok (Map.set players ~key:o.p_id ~data:new_player)
      else
        Error "insufficient funds"
    | None -> Error "player does not exist"
    end
  | None -> Error "stock not listed on market"

(*
*)
let add_bid (bids: order_map) (o: order): order_map =
  let compare_bid (o_l: order) (o_r: order): int =
    -1 * Float.compare o_l.value o_r.value
  in
  let update_bid (o: order) (l_o: order list option): order list =
    match l_o with
    | Some l -> List.sort (o :: l) ~compare:compare_bid
    | None -> [o]
  in
  if o.ct > 0 then
    Map.update bids o.ticker ~f:(update_bid o)
  else
    bids

let offer_bid (o: order) (bids: order_map) (asks: order_map) (stocks: stock_price_map) (players: player_map):
    ((order_map * order_map * stock_price_map * player_map), string) result =
    let rec scan_asks (l_asks: order list) (o: order) (matched_asks: order list): order list * order list * order =
      match l_asks with
      | o_ask :: l_r ->
        if Float.(>=) o.value o_ask.value then (* l_asks is sorted by increasing value, check still true in new *)
          let o_matched = {ticker = o.ticker; ct = o.ct - o_ask.ct; value = o.value; p_id = o.p_id} in
          if o_matched.ct > 0 then
            let ask = {ticker = o_ask.ticker; ct = o_ask.ct; value = o_ask.value; p_id = o_ask.p_id} in
            scan_asks l_r o_matched (ask :: matched_asks)
          else if o_matched.ct = 0 then
            let ask = {ticker = o_ask.ticker; ct = o_ask.ct; value = o_ask.value; p_id = o_ask.p_id} in
            (l_r, (ask :: matched_asks), o_matched)
          else
            let ask = {ticker = o_ask.ticker; ct = o.ct; value = o_ask.value; p_id = o_ask.p_id} in
            let o_ask_matched = {ticker = o_ask.ticker; ct = o_ask.ct - o.ct; value = o_ask.value; p_id = o_ask.p_id} in
            ((o_ask_matched :: l_r), (ask :: matched_asks), o_matched)
        else
          (l_asks, matched_asks, o)
      | [] -> ([], matched_asks, o)
    in
    match Map.find stocks o.ticker, Map.find players o.p_id with
    | Some stock_price, Some p ->
      let new_bids, new_asks, new_stocks, new_players =
        match Map.find asks o.ticker with
        | Some l_asks -> 
          let matched_asks, new_l_asks, bid_r = scan_asks l_asks o [] in
          let new_bids = add_bid bids bid_r in
          let new_players = execute_bids matched_bids in
          (new_bids, new_asks, new_players)
        | None ->
          let new_bids = add_bid bids o in
          (new_bids, asks, players)
      in
      Ok(new_bids, new_asks, new_stocks, new_players)
    | Some stock_price, None -> Error "player does not exist"
    | None, Some p -> Error "stock not listed on market"
    | None, None -> Error "stock not listed on market and player does not exist"

let offer_ask (o: order) (bids: order_map) (asks: order_map) (stocks: stock_price_map) (players: player_map):
    ((order_map * stock_price_map * player_map), string) result =
  failwith "unimplemented"

let get_price (ticker: string) (asks: order_map): (float, string) result =
  failwith "unimplemented"

let get_bid_ask (ticker: string): ((float * float), string) result =
  failwith "unimplemented"

let get_bid_ask_spread (ticker: string): (float, string) result =
  failwith "unimplemented"
