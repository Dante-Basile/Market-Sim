[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

open Core

type stock_price_map = float option list Map.M(String).t

type stock_ct_map = int list Map.M(String).t

type order = {ticker: string; ct: int; value: float; p_id: string;}

type order_map = order list Map.M(String).t

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

(*
TODO: use consecutive matching
*)
let buy_ipo (o: order) (stocks: stock_price_map) (players: player_map): (player_map, string) result =
  match Map.find stocks o.ticker with
  | Some _ ->
    begin
    match Map.find players o.p_id with
    | Some p ->
      let current_amt = List.hd_exn p.funds in
      let new_amt = current_amt -. (Float.of_int o.ct) *. o.value in
      if Float.(>=) new_amt 0. then
        let new_funds = new_amt :: p.funds in
        let new_stocks = Map.update p.stocks o.ticker ~f:(inc_stock_ct o.ct) in
        let new_player = {funds = new_funds; stocks = new_stocks} in
        Ok (Map.set players ~key:o.p_id ~data:new_player)
      else
        Error "insufficient funds"
    | None -> Error "player does not exist"
    end
  | None -> Error "stock not listed on market"

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
  let rec process_asks (l_asks: order list) (players: player_map) (id_bidder: string) (bidder: player) (matched_asks: order list): order list * player_map =
    match matched_asks with
    | o_ask :: l_r ->
      let bidder_funds_current = List.hd_exn bidder.funds in
      let bidder_afforded = Float.iround_towards_zero_exn (bidder_funds_current /. o_ask.value) in
      let asker = Map.find_exn players o_ask.p_id in
      let asker_owned = Map.find_exn asker.stocks o_ask.ticker in
      let asker_owned_current = List.hd_exn asker_owned in
      let max_exchange_ct = Int.min bidder_afforded asker_owned_current in
      let new_l_asks, new_bidder, new_asker =
        if max_exchange_ct >= o_ask.ct then
          let new_l_asks = l_asks in
          let new_bidder, new_asker = trade o_ask.ticker bidder asker o_ask.ct o_ask.value in
          new_l_asks, new_bidder, new_asker
        else
          let new_l_asks = {ticker = o_ask.ticker; ct = o_ask.ct - max_exchange_ct; value = o_ask.value; p_id = o_ask.p_id} :: l_asks in
          let new_bidder, new_asker = trade o_ask.ticker bidder asker max_exchange_ct o_ask.value in
          new_l_asks, new_bidder, new_asker
      in
      let new_players = Map.set players ~key:id_bidder ~data:new_bidder in
      let new_players = Map.set new_players ~key:o_ask.p_id ~data:new_asker in
      process_asks new_l_asks new_players id_bidder bidder l_r
    | [] -> l_asks, players
  in
  match Map.find stocks o.ticker, Map.find players o.p_id with
  | Some stock_price, Some p ->
    let new_bids, new_asks, new_stocks, new_players =
      match Map.find asks o.ticker with
      | Some l_asks -> 
        let matched_asks, new_l_asks, bid_r = scan_asks l_asks o [] in
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
  | Some stock_price, None -> Error "player does not exist"
  | None, Some p -> Error "stock not listed on market"
  | None, None -> Error "stock not listed on market and player does not exist"

(*
TODO: also check if stock enough stock owned
*)
let offer_ask (o: order) (bids: order_map) (asks: order_map) (stocks: stock_price_map) (players: player_map):
    ((order_map * order_map * stock_price_map * player_map), string) result =
  failwith "unimplemented"

let get_price (ticker: string) (asks: order_map): (float, string) result =
  failwith "unimplemented"

let get_bid_ask (ticker: string): ((float * float), string) result =
  failwith "unimplemented"

let get_bid_ask_spread (ticker: string): (float, string) result =
  failwith "unimplemented"
