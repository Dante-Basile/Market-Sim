[@@@ocaml.warning "-27"]

open Core

(*
  Maps ticker to shares owned
  string -> int
*)
type stock_ct_map = int Map.M(String).t

(*
  Record to store funds and stocks of player
*)
type player = {funds: int; stocks: stock_ct_map}

(*
  Maps id to player
  int -> player
*)
type player_map = player Map.M(Int).t 

(*
  Maps ticker to list of relevant order tuples
  tuple: (id of player who made order, order amount)
  string -> (int * float) list 
*)
type order_map = (int * float) list Map.M(String).t

(*
  Add player to player_map
*)
let add_player (id: int) (funds: float) (pm: player_map): player_map option =
  failwith "unimplemented"

(*
  Attempt to find an ask matching this bid and conduct the transaction
*)
let offer_bid (id: int) (amt: float) (asks: order_map) (players: player_map): (order_map * player_map) option =
  failwith "unimplemented"

(*
  Attempt to find a bid matching this ask and conduct the transaction
*)
let offer_ask (id: int) (amt: float) (bids: order_map) (players: player_map): (order_map * player_map) option =
  failwith "unimplemented"

(*
  Place an order in the order map
*)
let add_order (id: int) (amt: float) (orders: order_map): order_map option =
  failwith "unimplemented"

(*
  Get the highest bid (price a buyer is willing to pay)
  and the lowest ask (price a seller is willing to accept)
  tuple: (bid, ask)
*)
let get_bid_ask (ticker: string): (float * float) =
  failwith "unimplemented"

(*
  Get the difference between the bid and ask: ask - bid
*)
let get_bid_ask_spread (ticker: string): float =
  failwith "unimplemented"
