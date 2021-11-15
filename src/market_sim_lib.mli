(*
  Contains a ticker for all companies listed on the market
  Maps ticker to list of past and present stock prices
  string -> float list
*)
type stock_price_map = float list Base.Map.M(Core.String).t

(*
  Maps ticker to past and present shares owned
  string -> int list
*)
type stock_ct_map = int list Base.Map.M(Core.String).t

(*
  Record to store funds and stocks of player
*)
type player = { funds : float list; stocks : stock_ct_map; }

(*
  Maps id to player
  string -> player
*)
type player_map = player Base.Map.M(Core.String).t

(*
  Record to store details of order
*)
type order = { id : string; ct : int; value : float; }

(*
  Maps ticker to list of relevant order tuples
  string -> order list 
*)
type order_map = order list Base.Map.M(Core.String).t

(*
  Add stock to stock_price_map
*)
val add_stock : string -> stock_price_map -> (stock_price_map, string) result

(*
  Add player to player_map
*)
val add_player : string -> player_map -> (player_map, string) result

(*
  Player acquires stock from IPO
*)
val buy_ipo : string -> order -> stock_price_map -> player_map
  -> (player_map, string) result

(*
  Attempt to find an ask matching this bid and conduct the transaction
*)
val offer_bid : string -> order -> order_map -> stock_price_map -> player_map
  -> (order_map * stock_price_map * player_map, string) result

(*
  Attempt to find a bid matching this ask and conduct the transaction
*)
val offer_ask : string -> order -> order_map -> stock_price_map -> player_map
  -> (order_map * stock_price_map * player_map, string) result

(*
  Get the current price of a stock
*)
val get_price : string -> order_map -> (float, string) result

(*
  Get the highest bid (price a buyer is willing to pay)
  and the lowest ask (price a seller is willing to accept)
  tuple: (bid, ask)
*)
val get_bid_ask : string -> (float * float, string) result

(*
  Get the difference between the bid and ask: ask - bid
*)
val get_bid_ask_spread : string -> (float, string) result
