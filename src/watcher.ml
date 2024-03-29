open Batteries

type website = [ `Alientech | `AmazonEspana | `Aquario | `Banggood | `Base | `BestGames
                | `ElCorteIngles | `Fnac | `GamingReplay | `GearBest | `Globaldata
                | `Mediamarkt | `Novoatalho | `Pccomponentes | `Pcdiga | `ToyJapan
                | `Worten | `NotSupported ]

exception Http_error of int * string
exception Not_supported of string

let send_message id text =
  let open Telegram.Api in
  Command.SendMessage(id, text, Some ParseMode.Html, false, false, None, None)

let url_of_string s =
  let s1 = match Uri.of_string s |> Uri.host with
      None -> "//" ^ s
    | Some _ -> s in
  let s2 = match Uri.of_string s1 |> Uri.scheme with
      None -> "http:" ^ s1
    | Some _ -> s1 in
  Uri.of_string s2

let rec get_html id chat_id url =
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  Client.get url >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in  
  if (code = 302) then
    let url = resp |> Response.headers |> Header.get_location |> Option.get in
    let _ = Db.try_operation (fun db ->
      let db = Db.open_db () in
      let stmt = Db.update_item_url_stmt db id (Uri.to_string url) in
      let _ = Sqlite3.step stmt in
      true, None) in
    get_html id chat_id url
  else if (code = 200) then
    body |> Cohttp_lwt.Body.to_string
  else
    Lwt.fail (Http_error (code, Uri.to_string url)) (* TODO improve error handling *)

module Make
    (Parser : sig
       val get_price : string -> Price.t
       val get_name : string -> string
     end)
    (Object : sig
       val url : Uri.t
       val interval : int
     end) = struct

  let price_key = Lwt.new_key

  let rec run id chat_id previous_price () =
    let%lwt new_price = begin
      try%lwt
        let%lwt html = get_html id chat_id Object.url in
        let name = Parser.get_name html in
        let price = Parser.get_price html in
        let price_string = Price.to_string price in
        let message =
          match previous_price with
          | Some pp when not (Price.equal price pp) ->
              let db = Db.open_db () in
              let stmt = Db.update_item_price_stmt db id price in
              let _ = Sqlite3.step stmt in
              let _ = Sqlite3.db_close db in
              Some (Printf.sprintf "<b>%s</b>\nAtual: <i>%s</i>\nAnterior: <i>%s</i>\n%s" name price_string (Price.to_string pp) (Uri.to_string Object.url))
          | None ->
              let db = Db.open_db () in
              let stmt = Db.update_item_price_stmt db id price in
              let _ = Sqlite3.step stmt in
              let _ = Sqlite3.db_close db in
              Some (Printf.sprintf "<b>%s</b>\nAtual: <i>%s</i>\n%s" name price_string (Uri.to_string Object.url))
          | Some _ -> None in
        Log.debug "Chat id: %d | URL: %s | Previous price: %s | Current price: %s" chat_id (Uri.to_string Object.url) (match previous_price with Some p -> Price.to_string p | None -> "None") (price_string);
        match message with
          Some m ->
            let%lwt () = send_message chat_id m |> Actions_queue.push in
            Lwt.return (Some price)
        | None -> Lwt.return (Some price)
      with
        Http_error (code, s) -> Log.error "HTTP Code: %d - %s" code s; Lwt.return previous_price
      | _ as e -> Printexc.to_string e |> Log.error "URL: %s | Exception: %s" (Uri.to_string Object.url); Lwt.return previous_price
    end in
    let%lwt () = float_of_int Object.interval |> Lwt_unix.sleep in
    run id chat_id new_price ()

end

let get_site url =
  if (String.exists url "alientech.pt") then `Alientech
  else if (String.exists url "amazon.es") then `AmazonEspana
  else if (String.exists url "aquario.pt") then `Aquario
  else if (String.exists url "banggood.com") then `Banggood
  else if (String.exists url "base.com") then `Base
  else if (String.exists url "bestgames.pt") then `BestGames
  else if (String.exists url "elcorteingles.pt") then `ElCorteIngles
  else if (String.exists url "fnac.pt") then `Fnac
  else if (String.exists url "gamingreplay.com") then `GamingReplay
  else if (String.exists url "gearbest.com") then `GearBest
  else if (String.exists url "globaldata.pt") then `Globaldata
  else if (String.exists url "mediamarkt.pt") then `Mediamarkt
  else if (String.exists url "novoatalho.pt") then `Novoatalho
  else if (String.exists url "pccomponentes.com" || String.exists url "pccomponentes.pt") then `Pccomponentes
  else if (String.exists url "pcdiga.com") then `Pcdiga
  else if (String.exists url "toyland.pt") then `ToyJapan
  else if (String.exists url "worten.pt") then `Worten
  else `NotSupported

let get_thread id chat_id url site previous_price interval =
  let module Object = (struct
    let url = url_of_string url
    let interval = interval
  end) in
  let f = match site with
      `Alientech -> let module W = Make(Alientech)(Object) in W.run
    | `AmazonEspana -> let module W = Make(AmazonEspana)(Object) in W.run
    | `Aquario  -> let module W = Make(Aquario)(Object) in W.run
    | `Banggood -> let module W = Make(Banggood)(Object) in W.run
    | `Base -> let module W = Make(BaseCom)(Object) in W.run
    | `BestGames -> let module W = Make(BestGames)(Object) in W.run
    | `ElCorteIngles -> let module W = Make(Elcorteingles)(Object) in W.run
    | `Fnac -> let module W = Make(Fnac)(Object) in W.run
    | `GamingReplay -> let module W = Make(GamingReplay)(Object) in W.run
    | `GearBest -> let module W = Make(GearBest)(Object) in W.run
    | `Globaldata -> let module W = Make(Globaldata)(Object) in W.run
    | `Mediamarkt -> let module W = Make(Mediamarkt)(Object) in W.run
    | `Novoatalho -> let module W = Make(Novoatalho)(Object) in W.run
    | `Pccomponentes -> let module W = Make(Pccomponentes)(Object) in W.run
    | `Pcdiga -> let module W = Make(Pcdiga)(Object) in W.run
    | `ToyJapan ->  let module W = Make(ToyJapan)(Object) in W.run
    | `Worten -> let module W = Make(Worten)(Object) in W.run
    | `NotSupported -> raise (Not_supported url)
  in f id chat_id previous_price