(* In OCaml, `begin ... end` is the same as `( ... )` *)
[@@@ocaml.warning "-27"] (* unused variable *)
[@@@ocaml.warning "-33"] (* unused package *)

open Market_sim_lib;;
open Core;;

let render_home ?action request =
  <html>
  <body>
    <h1>Stock Market Simulator</h1>

%   begin match action with
%   | None -> 
      <%s! Dream.form_tag ~action:"/" request %>
        <p>Please choose one of the following options and indicate your choice below: </p>
        <li>Add stock</li>
        <li>Add player</li>
        <li>Buy IPO</li>
        <li>Bid offer</li>
        <li>Ask offer</li>
        <li>Get price</li>
        <li>Get player info</li>

        <br>
        <input name="action" autofocus>
      </form>
%   | Some action -> match String.lowercase action with
%     | "add stock" -> 
        <h1>Add stock: </h1>
%     | "add player" -> 
        <h1>Add player: </h1>
%     | "add buy ipo" -> 
        <h1>Buy IPO: </h1>
%     | "bid offer" -> 
        <h1>Bid offer: </h1>
%     | "ask offer" -> 
        <h1>Ask offer: </h1>
%     | "get price" -> 
        <h1>Get price: </h1>
%     | "get player" -> 
        <h1>Get player: </h1>
%     | _ -> 
      <p>Invalid option</p>
      <%s! Dream.form_tag ~action:"/" request %>
        <p>Please choose one of the following options and indicate your choice below: </p>
        <li>Add stock</li>
        <li>Add player</li>
        <li>Buy IPO</li>
        <li>Bid offer</li>
        <li>Ask offer</li>
        <li>Get price</li>
        <li>Get player info</li>

        <br>
        <input name="action" autofocus>
      </form>
      
%   end;

    
    
  </body>
  </html> 

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get  "/"
      (fun request ->
        Dream.html (render_home request));

    Dream.post "/"
      (fun request ->
        match%lwt Dream.form request with
        | `Ok ["action", action] ->
          Dream.html (render_home ~action request)
        | _ ->
          Dream.empty `Bad_Request);
        


  ]
  @@ Dream.not_found
