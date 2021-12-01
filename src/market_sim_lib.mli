(*
  Contains a ticker for all companies listed on the market
  Maps ticker to list of past and present stock prices
  Stock price is None if the stock was offered in the past but is not currently offered
  string -> float option list
*)
type stock_price_map = float option list Base.Map.M(Core.String).t

(*
  Maps ticker to past and present shares owned
  string -> int list
*)
type stock_ct_map = int list Base.Map.M(Core.String).t

(*
  Record to store details of order
*)
type order = { ticker: string; ct : int; value : float; p_id : string }

(*
  Maps ticker to list of relevant order tuples
  string -> order list 
*)
type order_map = order list Base.Map.M(Core.String).t

(*
  Record to store funds and stocks of player
*)
type player = { funds : float list; stocks : stock_ct_map }

(*
  Maps id to player
  string -> player
*)
type player_map = player Base.Map.M(Core.String).t

(*
  Maps ticker to public opinion
  string -> int list
*)
type opinion_map = int list Base.Map.M(Core.String).t

(*
  Add stock to stock_price_map
*)
val add_stock : string -> stock_price_map -> (stock_price_map, string) result

(*
  Add player to player_map
*)
val add_player : string -> float -> player_map -> (player_map, string) result

(*
  Player acquires stock from IPO
*)
val buy_ipo : order -> stock_price_map -> player_map
  -> (player_map, string) result

(*
  Attempt to find an ask matching this bid and conduct the transaction
*)
val offer_bid : order -> order_map -> order_map -> stock_price_map -> player_map
  -> (order_map * order_map * stock_price_map * player_map, string) result

(*
  Attempt to find a bid matching this ask and conduct the transaction
*)
val offer_ask : order -> order_map -> order_map -> stock_price_map -> player_map
  -> (order_map * order_map * stock_price_map * player_map, string) result

(*
  Get the current price of a stock
*)
val get_price : string -> stock_price_map -> (float option, string) result

(*
  Get the funds and stocks owned of a player
*)
val get_player_info : string -> player_map -> (float * (string * int) list, string) result

(*
  Get the highest bid (price a buyer is willing to pay)
  and the lowest ask (price a seller is willing to accept)
  tuple: (bid, ask)
*)
val get_bid_ask : string -> order_map -> order_map -> float option * float option

(*
  Get the difference between the bid and ask: ask - bid
*)
val get_bid_ask_spread : string -> order_map -> order_map -> (float, string) result

(*
  Trigger random shift in opinion
*)
val random_shift_opinion : opinion_map -> stock_price_map -> opinion_map

(*
  Get current public opinion of stock
*)
val get_opinion : string -> opinion_map -> (int, string) result
