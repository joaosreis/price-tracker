open Batteries
open Telegram.Api

let track message  =
  let open Message in
  let open Command in
  match message.text with
    Some url ->
      (*let interval1 = 5 in
      let thread1 = get_thread url interval1 in
      Lwt.async thread1;*)
      SendMessage (message.chat.id, "Tracking\n" ^ url, Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
  | None -> SendMessage (message.chat.id, "URL required", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)