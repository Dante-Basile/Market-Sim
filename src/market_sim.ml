[@@@ocaml.warning "-26"]

open Market_sim_lib;;
open Core;;

(*
  main:
*)
let () =
  let (players: player_map) = Map.empty (module Int) in
  let (bids: order_map) = Map.empty (module String) in
  let (asks: order_map) = Map.empty (module String) in
  failwith "unimplemented"
