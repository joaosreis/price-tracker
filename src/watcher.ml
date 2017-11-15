open Batteries

type website = [ `AmazonEspana | `BestGames | `Fnac | `GamingReplay | `GearBest | `ToyJapan | `Worten | `NotSupported ]

exception Http_error
exception Not_supported

module Bot = Telegram.Api.Mk(struct
  include Telegram.BotDefaults

  let token = Configuration.token
end)

let send_message id text =
  Bot.send_message ~chat_id:id
    ~text:text
    ~parse_mode:(Some Telegram.Api.ParseMode.Html)
    ~disable_web_page_preview:false
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

module Make
    (Parser : sig
       val get_price : string -> Price.t
       val get_name : string -> string
     end)
    (Object : sig
       val url : string
       val interval : int
     end) = struct

  let price_key = Lwt.new_key

  let rec run id previous_price () =
    let%lwt html = get_html Object.url in
    let name = Parser.get_name html in
    let price = Parser.get_price html in
    let price_string = match price with NoStock -> "Sem stock" | Stock p -> string_of_float p in
    let message = (*Some (Printf.sprintf "*%s*\nAtual: _%s_\n%s" name price_string Object.url)*)
    match previous_price with
      | Some pp when not (Price.equal price pp) ->
          Some (Printf.sprintf "<b>%s</b>\nAtual: <i>%s</i>\nAnterior: <i>%s</i>\n%s" name price_string (match pp with NoStock -> "Sem stock" | Stock p -> string_of_float p) Object.url)
      | None ->
          Some (Printf.sprintf "<b>%s</b>\nAtual: <i>%s</i>\n%s" name price_string Object.url)
      | Some _ -> None in
    match message with
      Some m ->
        let%lwt result = send_message id m in
        let%lwt () = float_of_int Object.interval |> Lwt_unix.sleep in
        run id (Some price) ()
    | None -> run id (Some price) ()
end

let get_thread id url interval =
  let get_site url =
    if (String.exists url "amazon.es") then `AmazonEspana
    else if (String.exists url "bestgames.pt") then `BestGames
    else if (String.exists url "fnac.pt") then `Fnac
    else if (String.exists url "gamingreplay.com") then `GamingReplay
    else if (String.exists url "gearbest.com") then `GearBest
    else if (String.exists url "toyland.pt") then `ToyJapan
    else if (String.exists url "worten.pt") then `Worten
    else `NotSupported in
  let module Object = (struct
    let url = url
    let interval = interval
  end) in
  match (get_site url) with
    `AmazonEspana -> let module W = Make(AmazonEspana)(Object) in W.run id None
  | `BestGames -> let module W = Make(BestGames)(Object) in W.run id None
  | `Fnac -> let module W = Make(Fnac)(Object) in W.run id None
  | `GamingReplay -> let module W = Make(GamingReplay)(Object) in W.run id None
  | `GearBest -> let module W = Make(GearBest)(Object) in W.run id None
  | `ToyJapan ->  let module W = Make(ToyJapan)(Object) in W.run id None
  | `Worten -> let module W = Make(Worten)(Object) in W.run id None
  | `NotSupported -> raise Not_supported