open Batteries
open Telegram.Api
open Telegram.Actions

let start message =
  let open Sqlite3 in
  let open Message in
  let open Command in
  try
    let db = Database.open_db () in
    let stmt = prepare db "INSERT INTO chats (chat_id,interval) VALUES(?,?)" in
    let _ = bind stmt 1 (Data.INT (Int64.of_int message.chat.id)) in
    let _ = bind stmt 2 (Data.INT 5L) in
    let _ = step stmt in
    let _ = finalize stmt in
    let _ = db_close db in
    SendMessage (message.chat.id, "Para fazer tracking de um produto envie `/track url_do_produto`", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
  with InternalError s | Error s -> SendMessage (message.chat.id, s, Some Telegram.Api.ParseMode.Markdown, false, false, None, None)

let track message  =
  let open Message in
  let open Command in
  match message.text with
    Some text -> begin
      let regex = Str.regexp "/track\\(.+\\)? \\(.+\\)" in
      try
        let _ = Str.string_match regex text 0 in
        let url = Str.matched_group 2 text in
        let interval = 5 in
        let thread = Watcher.get_thread message.chat.id url interval in
        Lwt.async thread;
        let open Sqlite3 in
        let db = Database.open_db () in
        let stmt = prepare db "INSERT INTO items (chat_id,url) VALUES(?,?)" in
        let _ = bind stmt 1 (Data.INT (Int64.of_int message.chat.id)) in
        let _ = bind stmt 2 (Data.TEXT url) in
        let _ = step stmt in
        let _ = finalize stmt in
        let _ = db_close db in
        Nothing
      with Not_found -> SendMessage (message.chat.id, "URL required", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
         | _ -> Nothing
    end
  | None -> assert false