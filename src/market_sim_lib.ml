[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

open Core

type stock_price_map = float option list Map.M(String).t

type order = {ticker: string; ct: int; value: float; p_id: string;}

type order_map = order list Map.M(String).t

type stock_ct_map = int list Map.M(String).t

type player = {funds: float list; stocks: stock_ct_map}

type player_map = player Map.M(String).t 

let add_stock (ticker: string) (stocks: stock_price_map): (stock_price_map, string) result =
  match Map.add stocks ~key:ticker ~data:([None]) with
  | `Ok stocks_new -> Ok stocks_new
  | `Duplicate -> Error "stock already exists"

let add_player (id: string) (st_funds: float) (pm: player_map): (player_map, string) result =
  match Map.add pm ~key:id ~data:{funds = [st_funds]; stocks = Map.empty (module String)} with
  | `Ok pm_new -> Ok pm_new
  | `Duplicate -> Error "player already exists"

(*
*)
let inc_stock_ct (ct_add: int) (l_ct: int list option): int list =
  match l_ct with
  | Some l ->
    let current_ct = List.hd_exn l in
    let new_ct = ct_add + current_ct in
    new_ct :: l
  | None -> [ct_add]

let buy_ipo (o: order) (stocks: stock_price_map) (players: player_map): (player_map, string) result =
  match Map.find stocks o.ticker, Map.find players o.p_id with
  | Some _, Some p ->
    let current_amt = List.hd_exn p.funds in
    let new_amt = current_amt -. (Float.of_int o.ct) *. o.value in
    if Float.(>=) new_amt 0. then
      let new_funds = new_amt :: p.funds in
      let new_stocks = Map.update p.stocks o.ticker ~f:(inc_stock_ct o.ct) in
      let new_player = {funds = new_funds; stocks = new_stocks} in
      Ok (Map.set players ~key:o.p_id ~data:new_player)
    else
      Error "insufficient funds"
  | Some _, None -> Error "player does not exist"
  | None, Some _ -> Error "stock not listed on market"
  | None, None -> Error "stock not listed on market and player does not exist"

(*
*)
let compare_bid (o_l: order) (o_r: order): int =
    -1 * Float.compare o_l.value o_r.value

(*
*)
let compare_ask (o_l: order) (o_r: order): int =
    Float.compare o_l.value o_r.value

(*
*)
let add_order (comp: order -> order -> int) (orders: order_map) (o: order): order_map =
  let update_order (comp: order -> order -> int) (o: order) (l_o: order list option): order list =
    match l_o with
    | Some l -> List.sort (o :: l) ~compare:comp
    | None -> [o]
  in
  if o.ct > 0 then
    Map.update orders o.ticker ~f:(update_order comp o)
  else
    orders

(*
*)
let add_bid = add_order compare_bid

(*
*)
let add_ask = add_order compare_ask

(*
*)
let trade (ticker: string) (bidder: player) (asker: player) (ct: int) (value: float): player * player =
  let new_bidder_funds = (List.hd_exn bidder.funds -. ((Float.of_int ct) *. value)) :: bidder.funds in
  let new_asker_funds = (List.hd_exn asker.funds +. ((Float.of_int ct) *. value)) :: asker.funds in
  let new_bidder_stocks = Map.update bidder.stocks ticker ~f:(inc_stock_ct ct) in
  let new_asker_stocks = Map.update asker.stocks ticker ~f:(inc_stock_ct (-1 * ct)) in
  let new_bidder = {funds = new_bidder_funds; stocks = new_bidder_stocks} in
  let new_asker = {funds = new_asker_funds; stocks = new_asker_stocks} in
  new_bidder, new_asker

(*
*)
let update_price (p: float option) (l_p: float option list option): float option list =
  match l_p with
  | Some l -> p :: l
  | None -> [p]

(*
  TODO: ensure that over cost and over ct orders are terminated, combine recursive functions
*)
let offer_bid (o: order) (bids: order_map) (asks: order_map) (stocks: stock_price_map) (players: player_map):
    (order_map * order_map * stock_price_map * player_map, string) result =
  let rec process_asks (players: player_map) (o_opt: order option) (bidder: player) (l_asks: order list):
      order option * order list * player_map =
    match o_opt, l_asks with (* l_asks is sorted by increasing value, check still true in new *)
    | Some o, o_ask :: l_r ->
      if Float.(>=) o.value o_ask.value then
        let o_new_opt, l_asks_new, new_players, new_bidder =
          let bidder_funds_current = List.hd_exn bidder.funds in
          let bidder_afforded = Float.iround_towards_zero_exn (bidder_funds_current /. o_ask.value) in
          let asker = Map.find_exn players o_ask.p_id in
          let asker_owned = Map.find_exn asker.stocks o_ask.ticker in
          let asker_owned_current = List.hd_exn asker_owned in
          let max_order_exchange = Int.min o.ct o_ask.ct in
          let max_afford_exchange = Int.min bidder_afforded max_order_exchange in
          let max_owned_exchange = Int.min asker_owned_current max_order_exchange in
          let max_exchange = Int.min max_afford_exchange max_owned_exchange in
          let o_new_opt =
            if o.ct > max_exchange && max_afford_exchange >= max_exchange then
              Some {ticker = o.ticker; ct = o.ct - max_exchange; value = o.value; p_id = o.p_id}
            else
              None
          in
          let l_asks_new =
            if o_ask.ct > max_exchange && max_owned_exchange >= max_exchange then
              {ticker = o_ask.ticker; ct = o_ask.ct - max_exchange; value = o_ask.value; p_id = o_ask.p_id} :: l_r
            else
              l_r
          in
          let new_bidder, new_asker = trade o_ask.ticker bidder asker max_exchange o_ask.value in
          let new_players = Map.set players ~key:o.p_id ~data:new_bidder in
          let new_players = Map.set new_players ~key:o_ask.p_id ~data:new_asker in
          o_new_opt, l_asks_new, new_players, new_bidder
        in
        process_asks new_players o_new_opt new_bidder l_asks_new
      else
        (Some o, l_asks, players)
    | _, _ -> (Some o, l_asks, players)
  in
  match Map.find stocks o.ticker, Map.find players o.p_id with
  | Some stock_price, Some p ->
    let new_bids, new_asks, new_stocks, new_players =
      match Map.find asks o.ticker with
      | Some l_asks -> 
        let o_new_opt, new_l_asks, new_players = process_asks players (Some o) p l_asks in
        let new_bids =
          match o_new_opt with
          | Some o_new -> add_bid bids o_new
          | None -> bids
        in
        let new_asks = Map.set asks ~key:o.ticker ~data:new_l_asks in
        let new_price =
          match List.hd new_l_asks with
          | Some o -> Some o.value
          | None -> None
        in
        let new_stocks = Map.update stocks o.ticker ~f:(update_price new_price) in
        (new_bids, new_asks, new_stocks, new_players)
      | None ->
        let new_bids = add_bid bids o in
        (new_bids, asks, stocks, players)
    in
    Ok(new_bids, new_asks, new_stocks, new_players)
  | Some stock_price, None -> Error "player does not exist"
  | None, Some p -> Error "stock not listed on market"
  | None, None -> Error "stock not listed on market and player does not exist"

(*
TODO: also check if stock enough stock owned
*)
let offer_ask (o: order) (bids: order_map) (asks: order_map) (stocks: stock_price_map) (players: player_map):
    (order_map * order_map * stock_price_map * player_map, string) result =
  match Map.find stocks o.ticker, Map.find players o.p_id with
  | Some stock_price, Some p ->
    begin
    match Map.find p.stocks o.ticker with
    | Some p_owned ->
      let p_owned_current = List.hd_exn p_owned in
      if p_owned_current > 0 then
        let new_bids, new_asks, new_stocks, new_players =
          match Map.find bids o.ticker with
          | Some l_bids -> 
            let matched_bids, new_l_bids, ask_r = scan_asks l_asks o [] in
            let new_bids = add_bid bids bid_r in
            let new_l_asks, new_players = process_asks new_l_asks players o.p_id p matched_asks in
            let new_l_asks = List.sort new_l_asks ~compare:compare_ask in (* needed? *)
            let new_asks = Map.set asks ~key:o.ticker ~data:new_l_asks in
            let new_price =
              match List.hd new_l_asks with
              | Some o -> Some o.value
              | None -> None
            in
            let new_stocks = Map.update stocks o.ticker ~f:(update_price new_price) in
            (new_bids, new_asks, new_stocks, new_players)
          | None ->
            let new_bids = add_bid bids o in
            (new_bids, asks, stocks, players)
        in
        Ok(new_bids, new_asks, new_stocks, new_players)
      else
        Error "player ownes 0 shares of this stock"
    | None -> Error "player has never owned this stock"
    end
  | Some stock_price, None -> Error "player does not exist"
  | None, Some p -> Error "stock not listed on market"
  | None, None -> Error "stock not listed on market and player does not exist"

let get_price (ticker: string) (asks: order_map): (float, string) result =
  failwith "unimplemented"

let get_bid_ask (ticker: string): ((float * float), string) result =
  failwith "unimplemented"

let get_bid_ask_spread (ticker: string): (float, string) result =
  failwith "unimplemented"
