[@@@ocaml.warning "-27"]

open Core

(*
  Contains a ticker for all companies listed on the market
  Maps ticker to list of past and present stock prices
  string -> float list
*)
type stock_price_map = (float, string) result list Map.M(String).t

(*
  Maps ticker to past and present shares owned
  string -> int list
*)
type stock_ct_map = int list Map.M(String).t

(*
  Record to store funds and stocks of player
*)
type player = {funds: int list; stocks: stock_ct_map}

(*
  Maps id to player
  int -> player
*)
type player_map = player Map.M(Int).t 

(*
  Record to store details of order
*)
type order = {id: int; qty: int; value: float}

(*
  Maps ticker to list of relevant order tuples
  string -> order list 
*)
type order_map = order list Map.M(String).t

(*
  Add stock to stock_price_map
*)
let add_stock (ticker: string) (stocks: stock_price_map): (stock_price_map, string) result =
  failwith "unimplemented"

(*
  Add player to player_map
*)
let add_player (id: int) (p: player) (pm: player_map): (player_map, string) result =
  failwith "unimplemented"

(*
  Player acquires stock from IPO
*)
let buy_ipo (ticker: string) (o: order): (player_map, string) result =
  failwith "unimplemented"

(*
  Attempt to find an ask matching this bid and conduct the transaction
*)
let offer_bid (ticker: string) (o: order) (asks: order_map) (stocks: stock_price_map) (players: player_map):
    ((order_map * stock_price_map * player_map), string) result =
  failwith "unimplemented"

(*
  Attempt to find a bid matching this ask and conduct the transaction
*)
let offer_ask (ticker: string) (o: order) (bids: order_map) (stocks: stock_price_map) (players: player_map):
    ((order_map * stock_price_map * player_map), string) result =
  failwith "unimplemented"

(*
  Place an order in the order map
*)
let add_order (ticker: string) (o: order) (orders: order_map) (stocks: stock_price_map):
    (order_map * stock_price_map) =
  failwith "unimplemented"

(*
  Get the current price of a stock
*)
let get_price (ticker: string) (asks: order_map): (float, string) result =
  failwith "unimplemented"

(*
  Get the highest bid (price a buyer is willing to pay)
  and the lowest ask (price a seller is willing to accept)
  tuple: (bid, ask)
*)
let get_bid_ask (ticker: string): ((float * float), string) result =
  failwith "unimplemented"

(*
  Get the difference between the bid and ask: ask - bid
*)
let get_bid_ask_spread (ticker: string): (float, string) result =
  failwith "unimplemented"
