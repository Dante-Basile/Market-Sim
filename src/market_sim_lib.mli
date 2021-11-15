type stock_price_map = float list Base.Map.M(Core.String).t
type stock_ct_map = int list Base.Map.M(Core.String).t
type player = { funds : float list; stocks : stock_ct_map; }
type player_map = player Base.Map.M(Core.String).t
type order = { id : string; ct : int; value : float; }
type order_map = order list Base.Map.M(Core.String).t
val add_stock : string -> stock_price_map -> (stock_price_map, string) result
val add_player : string -> player_map -> (player_map, string) result
val buy : string -> order -> player_map -> (player_map, string) result
val buy_ipo :
  string ->
  order -> stock_price_map -> player_map -> (player_map, string) result
val offer_bid :
  string ->
  order ->
  order_map ->
  stock_price_map ->
  player_map -> (order_map * stock_price_map * player_map, string) result
val offer_ask :
  string ->
  order ->
  order_map ->
  stock_price_map ->
  player_map -> (order_map * stock_price_map * player_map, string) result
val add_order :
  string ->
  order -> order_map -> stock_price_map -> order_map * stock_price_map
val get_price : string -> order_map -> (float, string) result
val get_bid_ask : string -> (float * float, string) result
val get_bid_ask_spread : string -> (float, string) result
