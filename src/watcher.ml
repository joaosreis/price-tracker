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
    else begin
      raise Http_error
    end

  let rec run () =
    let%lwt html = get_html Object.url in
    let price = Parser.get_price html in
    let%lwt result = send_message (Printf.sprintf "%s: %s\n%s" Object.name (match price with NoStock -> "Sem stock" | Stock p -> string_of_float p) Object.url) in
    let%lwt () = float_of_int Object.interval |> Lwt_unix.sleep in
    run ()
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
    `AmazonEspana -> let module W = Make(AmazonEspana)(Object) in W.run
  | `BestGames -> let module W = Make(BestGames)(Object) in W.run
  | `Fnac -> let module W = Make(Fnac)(Object) in W.run
  | `GamingReplay -> let module W = Make(GamingReplay)(Object) in W.run
  | `ToyJapan ->  let module W = Make(ToyJapan)(Object) in W.run
  | `Worten -> let module W = Make(Worten)(Object) in W.run
  | `NotSupported -> raise Not_supported