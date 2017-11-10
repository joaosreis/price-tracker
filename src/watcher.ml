module Make
    (Parser : sig
       val get_price : string -> Utils.price
     end)
    (Object : sig
       val url : string
       val interval : int
     end) = struct

  let send_message text = Bot.send_message ~chat_id:87784254
      ~text:text
      ~disable_notification:false
      ~reply_to:None
      ~reply_markup:None

  let lwt_html = Utils.get_html Object.url

  let run =
    let%lwt html = lwt_html in
    let price = Parser.get_price html in
    let%lwt result = send_message (match price with NoStock -> "Sem stock" | Stock p -> string_of_float p) in
    Lwt.return_unit
end