open Batteries

type website = [ `AmazonEspana | `BestGames | `Fnac | `GamingReplay | `ToyJapan | `Worten | `NotSupported ]

exception Http_error
exception Not_supported

module Make
    (Parser : sig
       val get_price : string -> Price.t
     end)
    (Object : sig
       val name : string
       val url : string
       val interval : int
     end) = struct

  let send_message text = Bot.send_message ~chat_id:87784254
      ~text:text
      ~disable_notification:false
      ~reply_to:None
      ~reply_markup:None

  let get_html url =
    let open Lwt in
    let open Cohttp in
    let open Cohttp_lwt_unix in
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if (code = 200) then begin
      body |> Cohttp_lwt.Body.to_string
    end
    else raise Http_error

  let price_key = Lwt.new_key

  let rec run previous_price () =
    let%lwt html = get_html Object.url in
    let price = Parser.get_price html in
    let message = match previous_price with
      | Some pp when not (Price.equal price pp) ->
          Some (Printf.sprintf "%s: %s\nAnterior: %s\n%s" Object.name (match price with NoStock -> "Sem stock" | Stock p -> string_of_float p) (match pp with NoStock -> "Sem stock" | Stock p -> string_of_float p) Object.url)
      | None ->
          Some (Printf.sprintf "%s: %s\n%s" Object.name (match price with NoStock -> "Sem stock" | Stock p -> string_of_float p) Object.url)
      | Some _ -> None in
    match message with
      Some m ->
        let%lwt result = send_message m in
        let%lwt () = float_of_int Object.interval |> Lwt_unix.sleep in
        run (Some price) ()
    | None -> run (Some price) ()
end

let get_thread name url interval =
  let get_site url =
    if (String.exists url "amazon.es") then `AmazonEspana
    else if (String.exists url "bestgames.pt") then `BestGames
    else if (String.exists url "fnac.pt") then `Fnac
    else if (String.exists url "gamingreplay.com") then `GamingReplay
    else if (String.exists url "toyland.pt") then `ToyJapan
    else if (String.exists url "worten.pt") then `Worten
    else `NotSupported in
  let module Object = (struct
    let name = name
    let url = url
    let interval = interval
  end) in
  match (get_site url) with
    `AmazonEspana -> let module W = Make(AmazonEspana)(Object) in W.run None
  | `BestGames -> let module W = Make(BestGames)(Object) in W.run None
  | `Fnac -> let module W = Make(Fnac)(Object) in W.run None
  | `GamingReplay -> let module W = Make(GamingReplay)(Object) in W.run None
  | `ToyJapan ->  let module W = Make(ToyJapan)(Object) in W.run None
  | `Worten -> let module W = Make(Worten)(Object) in W.run None
  | `NotSupported -> raise Not_supported